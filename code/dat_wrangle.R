
# create tuareg subset
df_shp_tuareg <- dplyr::filter(df_shp_ethnic_groups, group == "Tuareg") %>% dplyr::distinct(statename, .keep_all = TRUE)

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

# join health site data together
unique(df_niger_healthsite$type)

df_niger_healthsite_points <- df_niger_healthsite %>% 
  dplyr::filter(type == "Point") %>% 
  dplyr::mutate(geometry = purrr::map(coordinates, sf::st_point)) %>% 
  sf::st_as_sf()

df_niger_healthsite_polygons <- df_niger_healthsite %>% 
  dplyr::filter(type == "Polygon") %>%
  dplyr::mutate(coordinates = purrr::map(coordinates, matrix)) %>%
  dplyr::mutate(coordinates = purrr::map(coordinates, list)) %>%
  dplyr::mutate(geometry = purrr::map(coordinates, sf::st_sfc)) %>%
  dplyr::mutate(geometry = purrr::map(coordinates, sf::st_polygon))

# subset conflict data


  