######################################################################################################################################################
######

######################################################################################################################################################
###### Preliminaries

rm(list = ls())
options(scipen = 999)
gc()

library(ggplot2)
library(magrittr)

windowsFonts(font = windowsFont("Times New Roman"))

######################################################################################################################################################
###### File paths

fp_main     <- here::here()
fp_data     <- paste0(fp_main, "/data")
fp_data_map <- paste0(fp_data, "/map")
fp_exhibits <- paste0(fp_main, "/exhibits")
fp_tables   <- paste0(fp_exhibits, "/tables")
fp_figures  <- paste0(fp_exhibits, "/figures")
fp_code     <- paste0(fp_main, "/code")

######################################################################################################################################################
###### Read data

df_shp_ethnic_groups <- sf::st_read(paste0(fp_data_map, "/epr_ethnic_groups_shapefile/GeoEPR.shp"))
df_shp_mali          <- sf::st_read(paste0(fp_data_map, "/mali_shapefile/gadm36_MLI_0.shp"))
df_shp_niger         <- sf::st_read(paste0(fp_data_map, "/niger_shapefile/gadm36_NER_0.shp"))
df_shp_africa        <- sf::st_read(paste0(fp_data_map, "/africa_shapefile/Africa.shp"))
df_country_charas    <- read.csv(paste0(fp_data, "/country_characteristics.csv"))

######################################################################################################################################################
###### Wrangle data

df_shp_tuareg <- dplyr::filter(df_shp_ethnic_groups, group == "Tuareg") %>% dplyr::distinct(statename, .keep_all = TRUE)

sf::st_crs(df_shp_africa) <- 4326

# 
colnames(df_country_charas) <- c("Country", "Mali", "Niger")
df_country_charas <- as.matrix(df_country_charas)

######################################################################################################################################################
###### Source other scripts 

source(paste0(fp_code, "/vis_tuareg_map.R"))

