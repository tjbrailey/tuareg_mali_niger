
df_polityV %>% 
  dplyr::filter(country %in% c("Mali", "Niger")) %>% 
  dplyr::select(country, year, polity2, polity) %>% 
  tidyr::pivot_longer(cols = c(polity, polity2)) %>%
  dplyr::filter(name == "polity2") %>%
  ggplot(., mapping = aes(x = year, y = value, color = country)) + 
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(-10, 10)) + 
  labs(x = "Year", y = "Polity Score", color = "") +
  #facet_wrap(. ~ name) + 
  theme_minimal() + 
  theme(text = element_text(size = 18, family = "Times"))
