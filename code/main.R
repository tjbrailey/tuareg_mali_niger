######################################################################################################################################################
######

######################################################################################################################################################
###### preliminaries

rm(list = ls())
options(scipen = 999)
gc()

library(ggplot2)
library(magrittr)

windowsFonts(font = windowsFont("Times New Roman"))

######################################################################################################################################################
###### file paths

fp_main     <- here::here()
fp_data     <- paste0(fp_main, "/data")
fp_data_map <- paste0(fp_data, "/map")
fp_exhibits <- paste0(fp_main, "/exhibits")
fp_tables   <- paste0(fp_exhibits, "/tables")
fp_figures  <- paste0(fp_exhibits, "/figures")
fp_code     <- paste0(fp_main, "/code")

######################################################################################################################################################
###### read data

df_shp_ethnic_groups <- sf::st_read(paste0(fp_data_map, "/epr_ethnic_groups_shapefile/GeoEPR.shp"))
df_shp_mali          <- sf::st_read(paste0(fp_data_map, "/mali_shapefile/gadm36_MLI_0.shp"))
df_shp_niger         <- sf::st_read(paste0(fp_data_map, "/niger_shapefile/gadm36_NER_0.shp"))
df_shp_africa        <- sf::st_read(paste0(fp_data_map, "/africa_shapefile/Africa.shp"))
df_country_charas    <- read.csv(paste0(fp_data, "/country_characteristics.csv"))
df_cpa               <- read.csv(paste0(fp_data, "/pam_peace_agreements.csv"))

######################################################################################################################################################
###### wrangle data

# tuareg subset
df_shp_tuareg <- dplyr::filter(df_shp_ethnic_groups, group == "Tuareg") %>% dplyr::distinct(statename, .keep_all = TRUE)

# africa shapefile
sf::st_crs(df_shp_africa) <- 4326

# prepare country characteristic table for tex output
colnames(df_country_charas) <- c("Country", "Mali", "Niger")
df_country_charas <- as.matrix(df_country_charas)

# peace agreements subset
df_cpa_mali_niger <- dplyr::filter(df_cpa, country %in% c("Mali", "Niger")) %>% 
  dplyr::group_by(country) %>%
  dplyr::distinct(year, .keep_all = TRUE)

df_cpa_mali_niger_long <- df_cpa_mali_niger %>% 
  dplyr::select(country, year, dplyr::ends_with("implem")) %>% 
  tidyr::pivot_longer(cols = dplyr::ends_with("implem")) %>% 
  dplyr::group_by(name, country) %>% 
  dplyr::mutate(remove = sum(value),
                remove = ifelse(remove > 0, 0, 1)) %>%
  dplyr::filter(remove != 1)

######################################################################################################################################################
###### source other scripts 

source(paste0(fp_code, "/vis_tuareg_map.R"))

source(paste0(fp_code, "/vis_cpa_implementation.R"))
