# Pacotes
library(dplyr)

# Prepara join de dados do ensaios com os dados
# de índices calculados
dt_ens_idx <- dados_finais %>%
  left_join(
    tabela_indices,
    by = c("Id"  = "id_Parcela",
           "Voo" = "data_voo")
  ) %>% 
  relocate(Nota, .after = DiasPosAplica)

# Salvar
writexl::write_xlsx(
  dt_ens_idx,
  "./1-Dados/20250917-dados_ensaio&indices.xlsx"
)

