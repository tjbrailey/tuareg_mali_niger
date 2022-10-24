######################################################################################################################################################
###### Exploratory

rm(list = ls())

library(ggplot2)
library(magrittr)

df <- readRDS(paste0(here::here(), "/data/V-Dem-CY-Full+Others-v12.rds"))

# variable names
df_var_names <- as.data.frame(variable.names(df))

# mali and niger only
df_sub <- dplyr::filter(df, country_name %in% c(
  "Mali"
  , "Niger" 
  #, "Libya" 
  #, "Burkina Faso"
  ))

# missingness
Amelia::missmap(df_sub)

# remove variables that only have na
df_sub2 <- dplyr::select(df_sub, where(~ sum(!is.na(.x)) > 0))

Amelia::missmap(df_sub2)

df_sub2$v2x_divparctrl

ggplot() +
  geom_line(data = df_sub2, mapping = aes(x = year, y = v2x_divparctrl, color = country_name), alpha = 1, size = 1) + 
  geom_vline(xintercept = 2000, linetype = "dashed") + 
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 20))
