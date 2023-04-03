######################################################################################################################################################
######

# Create plot
plot_tuareg_map <- 
  ggplot() +
  geom_sf(data = df_shp_tuareg, fill = "gray", aes(color = "Tuareg Region"), alpha = .7) +
  geom_sf(data = df_shp_africa, color = "gray", fill = NA) +
  geom_sf(data = df_shp_mali, color = "black", fill = NA) +
  geom_sf(data = df_shp_niger, color = "black", fill = NA) +
  scale_color_manual(values = "gray") +
  labs(color = "") +
  coord_sf(xlim = c(-18, 20), ylim = c(0, 28)) +
  theme_void() + 
  theme(
    legend.position = c(0.5, 0.95),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),   
    text = element_text(size = 18, family = "Times")) 
plot_tuareg_map

# Save plot
ggsave(plot = plot_tuareg_map, filename = paste0(fp_figures, "/vis_tuareg_map.pdf"), 
       device = cairo_pdf,
       height = 10, width = 10)
