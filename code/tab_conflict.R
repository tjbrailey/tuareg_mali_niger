######################################################################################################################################################
######

# exploratory visualizations

# using acled data

# geographic distributions of conflict in the mali/niger region 
ggplot() +
  geom_sf(data = df_shp_mali) + 
  geom_sf(data = df_shp_niger) + 
  geom_sf(data = df_shp_tuareg, fill = "blue", alpha = 0.5) +
  geom_point(
    data = dplyr::filter(
      df_conflict, country %in% c("Mali", "Niger", "Libya"), 
      year %in% c(vec_years)), 
    mapping = aes(x = longitude, y = latitude))

# number of fatalities by year and by tuareg region (raw count)
ggplot(df_conflict_tuareg_sub, mapping = aes(x = year, y = fatalities, color = factor(tuareg_region))) + 
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2) + 
  facet_wrap(. ~ country) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "blue", "1" = "purple"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Fatalities (raw count)", color = "") + 
  theme_classic() + 
  theme()

# number of fatalities by year and by tuareg region (percentage)
ggplot(df_conflict_tuareg_sub, mapping = aes(x = year, y = pct_fatalities, color = factor(tuareg_region))) + 
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2) + 
  facet_wrap(. ~ country) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "blue", "1" = "purple"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Fatalities (%)", color = "Tuareg region") + 
  theme_classic() + 
  theme()

# number of incidents by year and by tuareg region (raw count)
ggplot(df_conflict_tuareg_sub, mapping = aes(x = year, y = incidents, color = factor(tuareg_region))) + 
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2) + 
  facet_wrap(. ~ country) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "blue", "1" = "purple"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Incidents (raw count)", color = "Tuareg region") + 
  theme_classic() + 
  theme()

# number of incidents by year and by tuareg region (percentage)
ggplot(df_conflict_tuareg_sub, mapping = aes(x = year, y = pct_incidents, color = factor(tuareg_region))) + 
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2) + 
  facet_wrap(. ~ country) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "blue", "1" = "purple"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Incidents (%)", color = "Tuareg region") + 
  theme_classic() + 
  theme()

# tabulation of fatalities by year and by tuareg region
df_conflict_tuareg_sub %>% 
  tibble::as_tibble(.) %>%
  dplyr::select(-geometry) %>%
  dplyr::group_by(country, intervention, tuareg_region) %>% 
  dplyr::summarise(incidents  = dplyr::n(),
                   fatalities = sum(fatalities, na.rm = TRUE))

# using prio data

