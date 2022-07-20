
# line plot of cumulative implementation score
plot_cum_imp_score <- df_cpa_mali_niger %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = agg_implem_score, color = country), size = 2, alpha = 0.7) +
  scale_x_continuous(limits = c(1990, 2005), breaks = seq(1990, 2005, 5)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) + 
  scale_color_manual(values = c("Mali" = "blue", "Niger" = "purple")) +
  labs(x = "Year", y = "Aggregated implementation score (%)", color = "") + 
  theme_classic() + 
  theme(
    legend.position = c(0.1, 0.95),
    text = element_text(size = 18, family = "Times"))

ggsave(plot = plot_cum_imp_score, filename = paste0(fp_figures, "/vis_cumulative_implementation_score.pdf"), 
       device = cairo_pdf,
       height = 10, width = 15)

# Number of provision implementation
vec_total_provis_mali <- unique(df_cpa_mali_niger_long$name[df_cpa_mali_niger_long$country == "Mali"]) 
vec_total_provis_score_mali <- length(vec_total_provis_mali) * 3

vec_total_provis_niger <- unique(df_cpa_mali_niger_long$name[df_cpa_mali_niger_long$country == "Niger"]) 
vec_total_provis_score_niger <- length(vec_total_provis_niger) * 3

df_lines <- data.frame(country = c("Mali", "Niger"), intercept = c(vec_total_provis_score_mali, vec_total_provis_score_niger))

plot_provis_imp <-
  ggplot() +
  geom_area(data = df_cpa_mali_niger_long, mapping = aes(x = year, y = value, fill = name)) + 
  geom_hline(data = df_lines, mapping = aes(yintercept = intercept), linetype = "dashed") + 
  scale_x_continuous(limits = c(1990, 2005), breaks = seq(1990, 2005, 5)) + 
  scale_fill_discrete(
    labels = c("amnest_implem" = "Amnesty",
               "cease_implem" = "Cease fire",
               "decen_implem" = "Decentralization",
               "demob_implem" = "Demobilation",
               "disarm_implem" = "Disarmament",
               "dispute_implem" = "Dispute resolution commission",
               "donor_implem" = "Donor support",
               "educat_implem" = "Education reform",
               "media_implem" = "Media reform",
               "legref_implem" = "Legislative branch reform",
               "milrfm_implem" = "Military reform",
               "offlan_implem" = "Official languages and symbols",
               "prisr_implem" = "Prisoner release",
               "polrfm_implem" = "Police reform",
               "refug_implem" = "Refugees",
               "reint_implem" = "Reintegration",
               "time_implem" = "Detailed timeline",
               "with_implem" = "Withdrawal of troops")) +
  facet_wrap(. ~ country, ncol = 1) +
  labs(x = "Year", y = "Successful implementation of provisions", fill = "") + 
  theme_classic() + 
  theme(
    legend.background = element_blank(), 
    text = element_text(size = 18, family = "Times"))

ggsave(plot = plot_provis_imp, filename = paste0(fp_figures, "/vis_provision_implementation.pdf"), 
       device = cairo_pdf,
       height = 10, width = 15)
