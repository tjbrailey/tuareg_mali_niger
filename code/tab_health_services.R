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
    values = c("0" = "blue", "1" = "purple"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Number of healthsites (raw count)", color = "Tuareg region") +
  theme_classic() + 
  theme(text = element_text(size = 18))

# percentages
ggplot(
  df_mali_healthsite_sub_collapse, 
  mapping = aes(x = year, y = pct_healthsites, color = factor(tuareg_region))) +
  geom_line(size = 1, alpha = 0.5) + 
  geom_point(size = 2) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_color_manual(
    values = c("0" = "blue", "1" = "purple"),
    labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Year", y = "Proportion of total healthsites", color = "Tuareg region") +
  theme_classic() + 
  theme(text = element_text(size = 18))

lm(healthsites ~ tuareg_region, data = df_mali_healthsite_sub_collapse) %>% summary(.)
