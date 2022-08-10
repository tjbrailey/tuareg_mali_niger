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

vec_time_start <- Sys.time()

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
df_mali_healthsite   <- read.csv(paste0(fp_data, "/unicef_mali_healthsites.csv"))
df_niger_healthsite  <- readRDS(paste0(fp_data, "/niger_healthsites_multiple_source.rds"))
df_conflict          <- read.csv(paste0(fp_data, "/acled_conflict_Africa_1997_2022.csv"))
df_conflict_prio     <- read.csv(paste0(fp_data, "/prio_conflict.csv"))

######################################################################################################################################################
###### wrangle data

source(paste0(fp_code, "/dat_wrangle.R"))

######################################################################################################################################################
###### source exhibit scripts 

# dat_niger_healthsites_multiple_source.R requires manual updates and so is not called in this script

source(paste0(fp_code, "/vis_tuareg_map.R"))

source(paste0(fp_code, "/vis_cpa_implementation.R"))

source(paste0(fp_code, "/tab_country_characteristics.R"))

source(paste0(fp_code, "/vis_health_services.R"))

source(paste0(fp_code, "/tab_conflict.R"))

source(paste0(fp_code, "/man_numbers.R"))

######################################################################################################################################################
###### end of script

vec_time_end <- Sys.time()

print(vec_time_end - vec_time_start)

rm(list = ls())
