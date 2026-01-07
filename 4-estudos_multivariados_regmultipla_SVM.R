# Codigo de estudo de SVM considerando a variavel resposta 
# Nota percentual de severidade como variável **contínua**

# pacotes necessários
options(OutDec = ",")
library(tidymodels)
library(stringr)
library(glue)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggpmisc)
library(broom)
library(readr)
library(writexl)
library(fs)
library(caret)
library(patchwork)
library(RColorBrewer) 
library(ggalluvial)
library(caret)
library(patchwork)


library(readxl)
df <- read_excel("./1-Dados/20250917-dados_ensaio&indices.xlsx", 
                 sheet = "Sheet1")

colnames(df)




# Diretorio para salvar o modelo RF
dir.create("./4-resultados_IndVeg_multivariado_SVM/1-modelos", 
           recursive = TRUE, showWarnings = FALSE)


# Diretorio para salvar imagens sober o modelo
dir.create("./4-resultados_IndVeg_multivariado_SVM/2-imagens", 
           recursive = TRUE, showWarnings = FALSE)



# ------- Randon Forest -----------
# seus vetores 


df_svm <- df

colnames(df_svm)


var_resp <- df_svm$Nota


var_IndVeg <- df_svm[, c(10, 13:30)]
# var_agron <- df_cls_RF[c(2, 6, 10)]
# var_clima <- df_cls_RF[41:258]

colnames(var_IndVeg)
# colnames(var_clima)
# colnames(var_agron)


df_final_svm <- cbind(var_resp, var_IndVeg)

colnames(df_final_svm)[1] <- "Nota (%)"


set.seed(123)
indice_treino <- createDataPartition(df_final_svm$`Nota (%)`, 
                                     p = 0.7, 
                                     list = FALSE)

treino <- df_final_svm[indice_treino, ]
teste  <- df_final_svm[-indice_treino, ]



res_svm <- seleciona_modelos_svm(
  dados  = treino,
  X      = colnames(var_IndVeg),
  Y      = colnames(df_final_svm)[1],
  custos = 2^(-3:5),
  gammas = 2^(-5:3),
  n_folds = 10,
  seed = 123
)




res_svm$melhores

res_svm$melhores$.config

res_svm$resultados %>% 
  filter(.config == res_svm$melhores$.config)

# prever

# Previsões no conjunto de treino
train_pred <- predict(res_svm$modelo_final, new_data = treino)$.pred
test_pred  <- predict(res_svm$modelo_final, new_data = teste)$.pred

# Avaliação
r2_train   <- R2(train_pred, treino$`Nota (%)`)
r2_test    <- R2(test_pred,  teste$`Nota (%)`)
rmse_train <- RMSE(train_pred, treino$`Nota (%)`)
rmse_test  <- RMSE(test_pred,  teste$`Nota (%)`)



cat("R² Treinamento:", round(r2_train, 3), "\n")
cat("R² Teste:", round(r2_test, 3), "\n")
cat("RMSE Treinamento:", round(rmse_train, 2), "\n")
cat("RMSE Teste:", round(rmse_test, 2), "\n")


# Criar data.frames de comparação
graf_treino <- data.frame(
  Real = treino$`Nota (%)`,
  Predito = train_pred
)

graf_teste <- data.frame(
  Real = teste$`Nota (%)`,
  Predito = test_pred
)

# Gráfico de treino
g1 <- ggplot(graf_treino, aes(x = Real, y = Predito)) +
  geom_point(alpha = 0.1, size = 2.0, color = "#00000010", fill = "#ffffff90") +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Real vs Predict (Training)",
       subtitle = paste0("R² = ", round(r2_train, 3), " | RMSE = ", round(rmse_train, 1)),
       x = expression("Real (Percent)"), 
       y = expression("Predict (Percent)")) +
  coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100)) +
  theme_bw(base_size = 12)

# Gráfico de teste
g2 <- ggplot(graf_teste, aes(x = Real, y = Predito)) +
  geom_point(alpha = 0.1, size = 2.0, color = "#00000010", fill = "#ffffff90") +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Real vs Predict (Validation)",
       subtitle = paste0("R² = ", round(r2_test, 3), " | RMSE = ", round(rmse_test, 1)),
       x = expression("Real (Percent)"), 
       y = expression("Predict (Percent)")) +
  coord_fixed(ratio= 1, xlim = c(0, 100), ylim = c(0, 100)) +
  theme_bw(base_size = 12)

# Combinar com patchwork
p4 <- g1 + g2 +
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")

p4


ggsave(file.path("./4-resultados_IndVeg_multivariado_SVM/2-imagens", "SVM_Estudo_Real_Predito.png"), 
       plot = p4,
       scale = 1.75,
       units = c("cm"), 
       dpi = 170,
       height = 6,
       width = 12,
       create.dir = T)




