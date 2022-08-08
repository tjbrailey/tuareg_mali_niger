
# create tuareg subset
df_shp_tuareg <- dplyr::filter(df_shp_ethnic_groups, group == "Tuareg") %>% dplyr::distinct(statename, .keep_all = TRUE)

sf::st_crs(df_shp_tuareg) <- 4326

# add crs to africa shapefile
sf::st_crs(df_shp_africa) <- 4326

# prepare country characteristic table for tex output
colnames(df_country_charas) <- c("Country", "Mali", "Niger")
df_country_charas <- as.matrix(df_country_charas)

# create peace agreements subset
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

# geolocate healthsites
df_mali_healthsite_sub <- df_mali_healthsite %>% 
  dplyr::filter(!is.na(Longitude)) %>%
  sf::st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  dplyr::mutate(tuareg_region  = as.numeric(sf::st_intersects(., df_shp_tuareg)),
                tuareg_region  = ifelse(is.na(tuareg_region), 0, tuareg_region),
                DATE.OUVERTURE = as.Date(DATE.OUVERTURE, "%d-%b-%Y"),
                year           = as.numeric(stringr::str_extract(DATE.OUVERTURE, "^.{4}")),
                intervention   = ifelse(year < 2000, 0, 1)) %>% 
  dplyr::filter(year <= 2022 & year >= 1990) 

# collapse data
df_mali_healthsite_sub_collapse <-
  df_mali_healthsite_sub %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry) %>%
  dplyr::group_by(year, intervention, tuareg_region) %>% 
  dplyr::summarize(healthsites = dplyr::n()) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(total_healthsites = sum(healthsites, na.rm = TRUE),
                pct_healthsites   = healthsites / total_healthsites)

# look at just 1996 - 2011
df_mali_healthsite_sub_collapse_1996 <- 
  df_mali_healthsite_sub_collapse %>% 
  dplyr::filter(year >= 1996) %>% 
  dplyr::group_by(tuareg_region) %>% 
  dplyr::mutate(cum_total_healthsites = sum(healthsites, na.rm = TRUE),
                cum_healthsites       = cumsum(healthsites),
                cum_healthsites_pct   = cum_healthsites / cum_total_healthsites)

# subset conflict data
vec_years <- c(1997:2003)

df_conflict_tuareg <- 
  df_conflict %>% 
  dplyr::filter(!is.na(longitude)) %>%
  sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::mutate(tuareg_region = as.numeric(sf::st_intersects(., df_shp_tuareg)),
                tuareg_region = ifelse(is.na(tuareg_region), 0, 1)) %>%
  dplyr::filter(country %in% c("Niger", "Mali"),
                year %in% c(vec_years)) %>% 
  dplyr::mutate(intervention = ifelse(year < 2000, 0, 1)) 

# collapse data
df_conflict_tuareg_sub <- 
  df_conflict_tuareg %>% 
  dplyr::group_by(country, year, intervention, tuareg_region) %>% 
  dplyr::summarise(incidents  = dplyr::n(),
                   fatalities = sum(fatalities, na.rm = TRUE)) %>% 
  dplyr::group_by(country, year) %>%
  dplyr::mutate(total_incidents  = sum(incidents, na.rm = TRUE),
                pct_incidents    = incidents / total_incidents,
                total_fatalities = sum(fatalities, na.rm = TRUE),
                pct_fatalities   = fatalities / total_fatalities,
                pct_fatalities   = ifelse(is.nan(pct_fatalities), 0, pct_fatalities))

# subset prio data and reshape
df_conflict_prio_sub <-
  df_conflict_prio %>% 
  dplyr::filter(country %in% c("Niger", "Mali", "Burkina Faso", "Algeria", "Libya"))


