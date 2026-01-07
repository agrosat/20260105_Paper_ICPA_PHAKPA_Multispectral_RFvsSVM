# Codigo de estudo de RF considerando a variavel resposta 
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
dir.create("./3-resultados_IndVeg_multivariado_RF/1-modelos", 
           recursive = TRUE, showWarnings = FALSE)


# Diretorio para salvar imagens sober o modelo
dir.create("./3-resultados_IndVeg_multivariado_RF/2-imagens", 
           recursive = TRUE, showWarnings = FALSE)



# ------- Randon Forest -----------
# seus vetores 


df_cls_RF <- df

colnames(df_cls_RF)


var_resp <- df_cls_RF$Nota


var_IndVeg <- df_cls_RF[, c(10, 13:30)]
# var_agron <- df_cls_RF[c(2, 6, 10)]
# var_clima <- df_cls_RF[41:258]

colnames(var_IndVeg)
# colnames(var_clima)
# colnames(var_agron)





df_final_RF <- cbind(var_resp, var_IndVeg)

colnames(df_final_RF)[1] <- "Nota (%)"

ntrees <- c(
  seq(
    25, 100, 25
  ),
  seq(
    100, 1000, 50
  ),
  seq(
    1000, 3000, 100
  ),
  seq(
    3000, 5000, 250
  )
)



set.seed(123)
indice_treino <- createDataPartition(df_final_RF$`Nota (%)`, 
                                     p = 0.7, 
                                     list = FALSE)

treino <- df_final_RF[indice_treino, ]
teste  <- df_final_RF[-indice_treino, ]



res <- sugere_ntree_ranger(treino, 
                           trees = ntrees,
                           "Nota (%)", 
                           interativo_base = F)


# Combina os dois gráficos e adiciona rótulos A e B
p1 <- (
  # GRafico A
  (res$grafico_rmse+
     geom_vline(xintercept = 3500, lty = "dashed", color = "gray60")) + 
    # Grafico B
    (res$grafico_variacao+
       geom_vline(xintercept = 3500, lty = "dashed", color = "gray60"))
) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")


p1

res$escolhido <- 3500


ggsave(file.path("./3-resultados_IndVeg_multivariado_RF/2-imagens", "Estudo_ntrees.png"), 
       plot = p1,
       scale = 1.75,
       units = c("cm"), 
       dpi = 170,
       height = 6,
       width = 12,
       create.dir = T)


resultado_validacao <- validar_ranger_paralelo(  data = treino,  
                                                 resposta = "Nota (%)", 
                                                 ntree = res$escolhido,  
                                                 k = 10,  
                                                 mtry = ncol(treino) - 1,  
                                                 min_node_size = 1)




resultado_validacao$df_cv$diff_RMSEmean_pct <- 100*(resultado_validacao$df_cv$RMSE - mean(resultado_validacao$df_cv$RMSE))/mean(resultado_validacao$df_cv$RMSE)

# Gráfico 1 – RMSE por Fold
g1 <- ggplot(resultado_validacao$df_cv, aes(x = as.factor(Fold), y = RMSE)) +
  #geom_line(color = "darkblue", linewidth = 1) +
  geom_col(size = 0.5, fill = "gray80", color="gray40", width = 0.4) +
  geom_hline(yintercept = mean(resultado_validacao$df_cv$RMSE), 
             lty= "dashed",
             color="#00000080") +
  labs(title = "", 
       x = "Fold", 
       y = "RMSE - Root Mean Square Error\n(Unit of predicted variable)") +
  theme_bw()

# Gráfico 2 – Variação percentual em relação ao RMSE médio
g2 <- ggplot(resultado_validacao$df_cv, aes(x = Fold, y = diff_RMSEmean_pct )) +
  geom_line(color = "gray", linewidth = 0.75) +
  geom_point(shape = 21, color = "white",fill = "gray", stroke = 1.5, size=2.5) +
  geom_hline(yintercept = 0,  
             lty= "dashed",
             color="#00000080") +
  labs(title = "", 
       x = "Fold", 
       y = "Variation RMSE (%)") +
  scale_x_continuous(breaks = seq(1,10, by=1))+
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )


# Combina os dois gráficos e adiciona rótulos A e B
p2 <- (
  # GRafico A
  (g1) + 
    # Grafico B
    (g2)
) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")

p2


ggsave(file.path("./3-resultados_IndVeg_multivariado_RF/2-imagens", "RF_Estudo_RMSE_Treino.png"), 
       plot = p2,
       scale = 1.75,
       units = c("cm"), 
       dpi = 170,
       height = 6,
       width = 12,
       create.dir = T)


# treina totalmente
resultado <- validar_ranger_paralelo(  data = treino,  
                                       resposta = "Nota (%)", 
                                       ntree = res$escolhido,  
                                       k = 1,  
                                       mtry = ncol(treino) - 1,  
                                       min_node_size = 1)






# Converter em data frame e ordenar
importancia_df <- data.frame(
  Variavel = names(resultado$modelo_ranger$variable.importance),
  Importancia = as.numeric(resultado$modelo_ranger$variable.importance)
)

# Ordenar da maior para a menor
importancia_df <- importancia_df[order(-importancia_df$Importancia), ]

# Visualizar
print(importancia_df)


importancia_df <- importancia_df %>% 
  mutate(Variavel = Variavel) %>% 
  filter(Importancia > 0) %>% 
  slice_head(n = 25)


p3 <- ggplot(importancia_df, aes(x = reorder(Variavel, Importancia), y = Importancia)) +
  geom_col(fill = "gray60") +
  coord_flip() +
  labs(title = "",
       x = "Variable",
       y = "Importance") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))

p3


ggsave(file.path("./3-resultados_IndVeg_multivariado_RF/2-imagens", "RF_Estudo_var_importancia.png"), 
       plot = p3,
       scale = 1.75,
       units = c("cm"), 
       dpi = 170,
       height = 5,
       width = 12,
       create.dir = T)


# Previsões no conjunto de treino
train_pred <- predict(resultado$modelo_ranger, data = treino)$predictions
test_pred  <- predict(resultado$modelo_ranger, data = teste)$predictions

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


ggsave(file.path("./3-resultados_IndVeg_multivariado_RF/2-imagens", "RF_Estudo_Real_Predito.png"), 
       plot = p4,
       scale = 1.75,
       units = c("cm"), 
       dpi = 170,
       height = 6,
       width = 12,
       create.dir = T)


