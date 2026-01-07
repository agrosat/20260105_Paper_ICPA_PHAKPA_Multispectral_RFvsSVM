# --------------------------------------------------------------------
# seleciona_modelos_svm() - versão robusta
# - Lida com nomes com espaços/parênteses (backticks automáticos)
# - Dummifica fatores antes de normalizar (SVM exige escala)
# - Detecta regressão vs classificação (RMSE/R2/MAE ou Accuracy/Kappa)
# - select_best() compatível com versões antigas/novas (metric/.metric)
# --------------------------------------------------------------------
seleciona_modelos_svm <- function(dados,
                                  X,                   # vetor de nomes das preditoras (char)
                                  Y,                   # nome da resposta (char)
                                  custos = 2^(-3:5),   # grid de C
                                  gammas = 2^(-5:3),   # grid de gamma (rbf_sigma)
                                  n_folds = 5,
                                  seed = 123) {
  if (!requireNamespace("tidymodels", quietly = TRUE))
    stop("Preciso do pacote 'tidymodels'. instale com install.packages('tidymodels').")
  suppressPackageStartupMessages(library(tidymodels))
  
  set.seed(seed)
  
  # --- saneamento de entradas ---
  if (missing(X) || missing(Y)) stop("Forneça X (vetor de preditoras) e Y (resposta).")
  if (!is.data.frame(dados)) stop("'dados' precisa ser um data.frame.")
  
  # remove backticks caso o usuário tenha passado `...`
  dequote <- function(x) gsub("^`|`$", "", x)
  Y_clean <- dequote(as.character(Y)[1])
  X_clean <- dequote(as.character(X))
  
  # checa colunas
  faltam <- setdiff(c(X_clean, Y_clean), names(dados))
  if (length(faltam) > 0) stop("Colunas ausentes no data.frame: ", paste(faltam, collapse = ", "))
  
  # tipo da resposta
  if (is.character(dados[[Y_clean]])) dados[[Y_clean]] <- factor(dados[[Y_clean]])
  tipo_resp <- if (is.factor(dados[[Y_clean]])) "classification" else "regression"
  
  # fórmula com backticks automáticos
  form <- as.formula(
    paste0("`", Y_clean, "` ~ ", paste0("`", X_clean, "`", collapse = " + "))
  )
  
  # receita: dummies -> normalize
  rec <- recipe(form, data = dados) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%  # trata Ploidia, Genotipo etc.
    step_normalize(all_predictors())
  
  # especificação SVM RBF
  spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
    set_engine("kernlab") %>%
    set_mode(tipo_resp)
  
  # grid
  grid <- tidyr::expand_grid(cost = custos, rbf_sigma = gammas)
  
  # CV
  folds <- vfold_cv(dados, v = n_folds)
  
  # workflow
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
  
  # métricas & critério
  if (tipo_resp == "classification") {
    metricas <- yardstick::metric_set(yardstick::accuracy, yardstick::kap)
    criterio <- "accuracy"
  } else {
    metricas <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
    criterio <- "rmse"
  }
  
  # tuning
  tune_res <- tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metricas,
    control = control_grid(save_pred = FALSE, verbose = FALSE)
  )
  
  all_metrics <- collect_metrics(tune_res)
  if (nrow(all_metrics) == 0) {
    stop("Não foi possível calcular métricas. Verifique NAs/variância zero/tamanho da amostra.")
  }
  
  # select_best compatível com versões diferentes do tune:
  best_params <- tryCatch(
    select_best(tune_res, metric = criterio),          # versões antigas
    error = function(e) select_best(tune_res, .metric = criterio) # versões novas
  )
  
  final_wf <- finalize_workflow(wf, best_params)
  fit_final <- fit(final_wf, data = dados)
  
  list(
    resultados   = all_metrics,   # tabela de métricas por (C, gamma)
    melhores     = best_params,   # hiperparâmetros escolhidos
    modelo_final = fit_final,     # workflow final treinado
    tipo         = tipo_resp
  )
}

