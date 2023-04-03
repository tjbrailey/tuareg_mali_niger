######################################################################################################################################################
######

# exploratory visualizations

# raw counts
ggplot(
  df_mali_healthsite_sub_collapse, 
  mapping = aes(x = year, y = healthsites, color = factor(tuareg_region))) +
  geom_line(size = 1, alpha = 0.5) + 
  geom_point(size = 2) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "grey", "1" = "black"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Number of healthsites in Mali (raw count)", color = "Tuareg region") +
  theme_classic() + 
  theme(text = element_text(size = 18))

# percentages
plot_healthsite_prolif <- ggplot(
  df_mali_healthsite_sub_collapse, 
  mapping = aes(x = year, y = pct_healthsites, color = factor(tuareg_region))) +
  geom_line(size = 1, alpha = 0.5) + 
  geom_point(size = 2) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "grey", "1" = "black"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Proportion of total healthsites in Mali", color = "Tuareg region") +
  theme_classic() + 
  theme(text = element_text(size = 18, family = "Times"))
plot_healthsite_prolif

ggsave(plot = plot_healthsite_prolif, 
       file = paste0(fp_figures, "/vis_health_serivces.pdf"), 
       device = cairo_pdf)

lm(pct_healthsites ~ tuareg_region * intervention, data = dplyr::filter(
  df_mali_healthsite_sub_collapse, year %in% c(1996:2001))) %>% summary(.)

MASS::glm.nb(healthsites ~ tuareg_region * intervention, 
             data = dplyr::filter(
               df_mali_healthsite_sub_collapse, 
               year %in% c(1996:2001))) %>% summary(.)

# sum all healthsites in tuareg and non tuareg region for comparative time 
# span (probably 1996 - 2011), then look at increase per year as a 
# proportion of total in that time period
ggplot(
  df_mali_healthsite_sub_collapse_1996, 
  mapping = aes(x = year, y = cum_healthsites_pct, 
                color = factor(tuareg_region))) +
  geom_line(size = 1, alpha = 0.5) + 
  geom_point(size = 2) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "grey", "1" = "black"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", 
       y = "Cumulative percentage increase in healthsites in Mali", 
       color = "Tuareg region") +
  theme_classic() + 
  theme(text = element_text(size = 18))

lm(cum_healthsites_pct ~ tuareg_region * intervention, 
   data = dplyr::filter(df_mali_healthsite_sub_collapse_1996, 
                        year %in% c(1996:2001))) %>% summary()

###########################################################################
##### 
