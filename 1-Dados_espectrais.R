# -------------------------------------------
# Pacotes
# -------------------------------------------
library(terra)         # substitui raster
library(sf)
library(exactextractr)
library(dplyr)
library(tools)

# -------------------------------------------
# Função para calcular estatísticas zonais 
# -------------------------------------------
processa_raster <- function(raster_path, buffer) {
  # lê o raster como SpatRaster
  r <- rast(raster_path)
  
  # calcula médias por polígono
  res <- exact_extract(r, buffer, fun = "mean", progress = FALSE)
  df <- as.data.frame(res)
  
  # detectar campo de ID no shapefile
  id_col <- setdiff(names(buffer), attr(buffer, "sf_column"))
  
  # cola o campo de ID
  df[[id_col[1]]] <- buffer[[id_col[1]]][seq_len(nrow(df))]
  
  # nome do raster (sem extensão)
  df$raster <- file_path_sans_ext(basename(raster_path))
  
  # renomear bandas
  names(df)[1:nlyr(r)] <- paste0("Banda", 1:nlyr(r))
  
  return(df)
}

# -------------------------------------------
# Dados referentes a Gleba3D
# -------------------------------------------

# --- Carregar dados ---
dir <- "J:/PROJETOS_SAZONAIS/AGR25 - CARACTERIZACAO RADIOMETRICA DA FERRUGEM SOJA/3D/Dados_Processados_Analise"

buffer <- st_read(file.path(dir, "avaliacao_phakopsora3d_Id_Buffer.shp"))

# --- Rasters ---
raster1 <- "20240206_Micasense_CDECastro3D_GSD5.tif"
raster2 <- "20240223_Micasense_CDECastro3D_GSD5.tif"
raster3 <- "20240314_Micasense_CDECastro3D_GSD5.tif"

rasters <- c(file.path(dir, raster1),
             file.path(dir, raster2),
             file.path(dir, raster3))

# --- Aplicar a função para cada raster ---
lista_resultados <- lapply(rasters, processa_raster, buffer = buffer)


# --- Unir em uma única tabela ---
tabela_final <- bind_rows(lista_resultados) %>%
  mutate(
    data_voo = as.Date(substr(raster, 1, 8), format = "%Y%m%d")
  ) %>% 
  rename(
    Blue    = Banda1, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    Green   = Banda2, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    Red     = Banda3, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    Nir     = Banda4, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    RedEdge = Banda5, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
  ) 


# Visualizar
print(tabela_final)

# Salvar
writexl::write_xlsx(
  tabela_final,
  "./1-Dados/20250917-tabela_bandas_epectrais_3D.xlsx"
)


tabela_indices_3D <- tabela_final %>%
  mutate(
    # ------- Índices clássicos ------- 
    NDVI   = (Nir - Red) / (Nir + Red),
    GNDVI  = (Nir - Green) / (Nir + Green),
    NDRE   = (Nir - RedEdge) / (Nir + RedEdge),
    NDVIre = (RedEdge - Red) / (RedEdge + Red),
    # fator L=0.5 (L está na formula e determina o 0.5 e o 1.5)
    SAVI   = (1.5 * (Nir - Red)) / (Nir + Red + 0.5),   
    # 0.16 parece que é um fator fixo definido pela metodologia
    OSAVI  = (1.16 * (Nir - Red)) / (Nir + Red + 0.16), 
    
    # ------- Clorofila ------- 
    Cigreen = (Nir / Green) - 1,
    RECI    = (Nir / RedEdge) - 1,
    # +eps p/ evitar div/0
    CVI     = (Nir * Red) / (Green^2 + 1e-6),   
    MTCI    = (Nir - RedEdge) / (RedEdge - Red),
    
    # ------- Outros índices ------- 
    # C1, C2 = coeficientes de correção atmosférica (padrão: C1 = 6, C2 = 7.5), G = fator de ganho (geralmente 2.5)
    EVI   = 2.5 * ( (Nir - Red) / (Nir + 6*Red - 7.5*Blue + 1) ), 
    SIPI  = (Nir - Blue) / (Nir - Red),
    VARI  = (Green - Red) / (Green + Red - Blue),
    ExG   = 2*Green - Red - Blue,
    GLI   = (2*Green - Red - Blue) / (2*Green + Red + Blue),
    # os valores constantes (190 e 120) vem da diff entre os comprimentos de ondas
    TGI   = -0.5 * ((190*(Red - Green)) - (120*(Red - Blue))), 
    TVI   = sqrt(abs(NDVI + 0.5)),
    
    # Intensidade / brilho
    Intensity = (Red + Green + Blue) / 3
  ) %>% 
  dplyr::select(id_Parcela, data_voo, everything(), 
                -c(Blue, Red, Green, RedEdge, Nir, raster)) %>% 
  dplyr::mutate(id_Parcela = id_Parcela + 3000)

# Visualizar
print(tabela_indices_3D)



# Salvar
writexl::write_xlsx(
  tabela_indices_3D,
  "./1-Dados/20250917-tabelas_indices_3D.xlsx"
)


# -------------------------------------------
# Dados referentes a Gleba11C
# -------------------------------------------

# --- Carregar dados ---
dir <- "J:/PROJETOS_SAZONAIS/AGR25 - CARACTERIZACAO RADIOMETRICA DA FERRUGEM SOJA/11C/Dados_Processados_Analise"

buffer <- st_read(file.path(dir, "Buffer1m_Centroide.shp"))  

raster1 <- "20240206_Micasense_CDECastro11C_GSD5_modificado.tif"  
raster2 <- "20240223_Micasense_CDECastro11C_GSD5_modificado.tif"  
raster3 <- "20240314_Micasense_CDECastro11C_GSD5_modificado.tif"  

rasters <- c(file.path(dir, raster1),
             file.path(dir, raster2),
             file.path(dir, raster3))



# --- Aplicar a função para cada raster ---
lista_resultados <- lapply(rasters, processa_raster, buffer = buffer)


# --- Unir em uma única tabela ---
tabela_final <- bind_rows(lista_resultados) %>%
  mutate(
    data_voo = as.Date(substr(raster, 1, 8), format = "%Y%m%d")
  ) %>% 
  rename(
    Blue    = Banda1, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    Green   = Banda2, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    Red     = Banda3, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    Nir     = Banda4, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    RedEdge = Banda5, # espec em https://support.micasense.com/hc/en-us/articles/360010025413-Altum-Integration-Guide
    # Precisei inclui essa linha pq o shp tinha outro nome para id
    id_Parcela = id
  )

# Visualizar
print(tabela_final)

# # Salvar
# writexl::write_xlsx(
#   tabela_final,
#   "./1-Dados/20250917-tabela_bandas_epectrais_11C.xlsx"
# )


tabela_indices_11C <- tabela_final %>%
  mutate(
    # ------- Índices clássicos ------- 
    NDVI   = (Nir - Red) / (Nir + Red),
    GNDVI  = (Nir - Green) / (Nir + Green),
    NDRE   = (Nir - RedEdge) / (Nir + RedEdge),
    NDVIre = (RedEdge - Red) / (RedEdge + Red),
    # fator L=0.5 (L está na formula e determina o 0.5 e o 1.5)
    SAVI   = (1.5 * (Nir - Red)) / (Nir + Red + 0.5),   
    # 0.16 é um fator fixo definido pela metodologia
    OSAVI  = (1.16 * (Nir - Red)) / (Nir + Red + 0.16), 
    
    # ------- Clorofila ------- 
    Cigreen = (Nir / Green) - 1,
    RECI    = (Nir / RedEdge) - 1,
    # +eps p/ evitar div/0
    CVI     = (Nir * Red) / (Green^2 + 1e-6),   
    MTCI    = (Nir - RedEdge) / (RedEdge - Red),
    
    # ------- Outros índices ------- 
    # C1, C2 = coeficientes de correção atmosférica (padrão: C1 = 6, C2 = 7.5), G = fator de ganho (geralmente 2.5)
    EVI   = 2.5 * ( (Nir - Red) / (Nir + 6*Red - 7.5*Blue + 1) ), 
    SIPI  = (Nir - Blue) / (Nir - Red),
    VARI  = (Green - Red) / (Green + Red - Blue),
    ExG   = 2*Green - Red - Blue,
    GLI   = (2*Green - Red - Blue) / (2*Green + Red + Blue),
    # os valores constantes (190 e 120) vem da diff entre os comprimentos de ondas
    TGI   = -0.5 * ((190*(Red - Green)) - (120*(Red - Blue))), 
    TVI   = sqrt(abs(NDVI + 0.5)),
    
    # Intensidade / brilho
    Intensity = (Red + Green + Blue) / 3
  ) %>% 
  dplyr::select(id_Parcela, data_voo, everything(), 
                -c(Blue, Red, Green, RedEdge, Nir, raster)) %>% 
  dplyr::mutate(id_Parcela = id_Parcela + 11000)

# Visualizar
print(tabela_indices_11C)

# # Salvar
# writexl::write_xlsx(
#   tabela_indices_11C,
#   "./1-Dados/20250917-tabelas_indices_11C.xlsx"
# )



# -------------------------------------------
# Dados indices unidos
# -------------------------------------------
# Unindo as tabelas de indices
tabela_indices <- rbind(tabela_indices_3D, tabela_indices_11C)

