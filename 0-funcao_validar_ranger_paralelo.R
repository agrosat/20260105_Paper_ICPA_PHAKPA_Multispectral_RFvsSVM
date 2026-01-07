validar_ranger_paralelo <- function(data, resposta, 
                                    ntree = 500, 
                                    k = 5, 
                                    mtry = NULL, 
                                    min_node_size = 1, 
                                    cores = NULL,
                                    seed = 123) {
  require(ranger)
  require(doParallel)
  require(foreach)
  require(Metrics)
  require(caret)
  
  set.seed(seed)
  
  # Valida nome da resposta
  if (!resposta %in% names(data)) stop("❌ Variável de resposta não encontrada.")
  
  # Imputação simples
  for (v in names(data)) {
    if (anyNA(data[[v]])) {
      if (is.numeric(data[[v]])) {
        data[[v]][is.na(data[[v]])] <- median(data[[v]], na.rm = TRUE)
      } else {
        moda <- names(which.max(table(data[[v]])))
        data[[v]][is.na(data[[v]])] <- moda
        if (is.factor(data[[v]]) && !(moda %in% levels(data[[v]]))) {
          levels(data[[v]]) <- c(levels(data[[v]]), moda)
        }
      }
    }
  }
  
  if (is.null(mtry)) mtry <- floor(sqrt(ncol(data) - 1))
  if (is.null(cores)) cores <- max(1, parallel::detectCores() - 1)
  
  # Caso especial: k = 1 → treino com todos os dados
  if (k == 1) {
    cat("⚙️ k=1 | Treinando modelo com TODOS os dados (sem validação cruzada)...\n")
    
    modelo <- ranger(
      dependent.variable.name = resposta,
      data = data,
      num.trees = ntree,
      mtry = mtry,
      respect.unordered.factors = "order",
      num.threads = cores,
      importance = "impurity",
      keep.inbag = TRUE,
      write.forest = TRUE,
      min.node.size = min_node_size
    )
    
    return(list(
      df_cv = data.frame(Fold = 1, RMSE = NA),
      modelo_ranger = modelo,
      modelos_folds = list(modelo),
      previsoes = predict(modelo, data = data)$predictions
    ))
  }
  
  # Validação cruzada normal se k > 1
  folds <- createFolds(data[[resposta]], k = k, list = TRUE)
  
  rmse_lista <- numeric(k)
  modelos_lista <- vector("list", k)
  melhor_rmse <- Inf
  melhor_modelo <- NULL
  
  for (i in seq_along(folds)) {
    cat("\n=== Fold", i, "===\n")
    
    idx_teste <- folds[[i]]
    df_train <- data[-idx_teste, ]
    df_test  <- data[idx_teste, ]
    
    modelo <- ranger(
      dependent.variable.name = resposta,
      data = df_train,
      num.trees = ntree,
      mtry = mtry,
      respect.unordered.factors = "order",
      num.threads = cores,
      importance = "impurity",
      keep.inbag = TRUE,
      write.forest = TRUE,
      min.node.size = min_node_size
    )
    
    pred <- predict(modelo, data = df_test)$predictions
    rmse_lista[i] <- rmse(df_test[[resposta]], pred)
    modelos_lista[[i]] <- modelo
    
    cat("RMSE Fold", i, ":", rmse_lista[i], "\n")
    
    if (rmse_lista[i] < melhor_rmse) {
      melhor_rmse <- rmse_lista[i]
      melhor_modelo <- modelo
    }
  }
  
  df_cv <- data.frame(Fold = 1:k, RMSE = rmse_lista)
  
  return(list(
    df_cv = df_cv,
    modelo_ranger = melhor_modelo,
    modelos_folds = modelos_lista,
    previsoes = predict(melhor_modelo, data = data)$predictions
  ))
}
