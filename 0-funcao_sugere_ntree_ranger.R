sugere_ntree_ranger <- function(data, resposta,
                                trees = c(10, 50, 100, 200, 300, 400, 500),
                                limiar_pct = 0.001,
                                interativo = FALSE,
                                interativo_base = FALSE) {
  require(ranger)
  require(Metrics)
  require(ggplot2)
  require(plotly)
  
  set.seed(123)
  resposta_chr <- resposta
  if (!resposta_chr %in% names(data)) stop("❌ A variável de resposta não está presente na base.")
  
  # Preencher NA
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
  
  # Separar preditoras
  variaveis_preditoras <- setdiff(names(data), resposta_chr)
  rmse_lista <- numeric(length(trees))
  
  for (i in seq_along(trees)) {
    modelo <- ranger(
      dependent.variable.name = resposta_chr,
      data = data,
      num.trees = trees[i],
      mtry = length(variaveis_preditoras),
      respect.unordered.factors = "order",
      num.threads = parallel::detectCores(logical = TRUE),
      importance = "impurity",
      keep.inbag = TRUE,
      write.forest = TRUE
    )
    pred <- predictions(modelo)
    rmse_lista[i] <- rmse(data[[resposta_chr]], pred)
    cat("ntree =", trees[i], "-> RMSE:", rmse_lista[i], "\n")
  }
  
  df <- data.frame(ntree = trees, RMSE = rmse_lista)
  df$RMSE_diff <- c(NA, diff(df$RMSE))
  df$RMSE_pct <- c(NA, abs(diff(df$RMSE)) / head(df$RMSE, -1))
  
  idx_auto <- which(df$RMSE_pct < limiar_pct)[1]
  ntree_sugerido <- if (!is.na(idx_auto)) df$ntree[idx_auto] else max(df$ntree)
  df$label <- ifelse(df$ntree == ntree_sugerido, paste0("Sugerido: ", ntree_sugerido), NA)
  
  modelo_nls <- try(
    nls(RMSE ~ a * exp(-b * ntree) + c, data = df,
        start = list(a = max(df$RMSE) - min(df$RMSE), b = 0.005, c = min(df$RMSE))),
    silent = TRUE
  )
  if (!inherits(modelo_nls, "try-error")) {
    df$fit <- predict(modelo_nls)
  } else {
    message("⚠️ Ajuste não convergiu.")
    df$fit <- NA
  }
  
  if (interativo_base) {
    plot(df$ntree, df$RMSE, type = "b", pch = 19, col = "blue",
         main = "Clique no ponto para escolher o nº ideal de árvores",
         xlab = "nº Árvores", ylab = "RMSE")
    idx_click <- identify(df$ntree, df$RMSE, labels = df$ntree, n = 1)
    ntree_escolhido <- df$ntree[idx_click]
    cat("\ud83d\udfe2 Você escolheu:", ntree_escolhido, "árvores.\n")
    return(list(tabela = df, escolhido = ntree_escolhido, modelo = modelo_nls))
  }
  
  if (interativo) {
    p_plotly <- ggplot(df, aes(x = ntree, y = RMSE, text = label)) +
      geom_point(size = 2, color = "gray40") +
      { if (!all(is.na(df$fit))) geom_line(aes(y = fit), color = "blue", size = 1) } +
      geom_vline(xintercept = ntree_sugerido, linetype = "dotted", color = "darkgreen") +
      labs(title = "RMSE vs Número de Árvores", x = "nº Árvores", y = "RMSE") +
      theme_minimal()
    print(ggplotly(p_plotly, tooltip = "text"))
    message("\ud83d\uddb1️ Clique no gráfico e use: plotly::event_data('plotly_click') para capturar o valor.")
    return(invisible(list(tabela = df, sugerido = ntree_sugerido, plot = p_plotly)))
  }
  
  # Gráfico estático
  p <- ggplot(df, aes(x = ntree, y = RMSE)) +
    geom_point(size = 2, color = "gray40") +
    { if (!all(is.na(df$fit))) geom_line(aes(y = fit), color = "blue", size = 1) } +
    #geom_vline(xintercept = ntree_sugerido, linetype = "dotted", color = "darkgreen") +
    #geom_text(aes(label = label), na.rm = TRUE, vjust = -1.2, color = "darkgreen", size = 4.5) +
    coord_cartesian(clip = "off") +
    labs(title = "", 
         x = "nº Árvores", 
         y = "RMSE - Raiz do erro quadrático médio\n(Unidade da variável resposta)") +
    theme_bw()
  print(p)
  
  g_var <- ggplot(df, aes(x = ntree, y = RMSE_pct * 100)) +
    geom_line(color = "red", size = 1) +
    #geom_vline(xintercept = ntree_sugerido, linetype = "dotted", color = "darkgreen") +
    geom_hline(yintercept = limiar_pct * 100, linetype = "dashed", color = "gray") +
    #geom_text(aes(label = label), na.rm = TRUE, vjust = -1.2, color = "darkgreen", size = 4.5) +
    coord_cartesian(clip = "off") +
    labs(title = "", 
         x = "nº Árvores", 
         y = "Variação RMSE (%)") +
    theme_bw()
  print(g_var)
  
  # Retorno final correto
  return(list(
    tabela = df,
    sugerido = ntree_sugerido,
    modelo = if (!inherits(modelo_nls, "try-error")) modelo_nls else NULL,
    grafico_rmse = p,
    grafico_variacao = g_var
  ))
  
}
