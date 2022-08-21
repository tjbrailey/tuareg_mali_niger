######################################################################################################################################################
###### numbers and statistics cited in the manuscript

### percentage completion of CPA provisions
df_cpa_mali_niger_long %>% 
  dplyr::filter(year == max(year)) %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(imp = sum(value), 
                   max = dplyr::n() * 3,
                   pct = imp / max * 100)

### percentage change in conflict
fxn_pct_change(
  start_val = as.data.frame(df_conflict_tuareg_sub_fatalities)$Mali[2],
  end_val    = as.data.frame(df_conflict_tuareg_sub_fatalities)$Mali[4],
  sig_fig    = 2)

fxn_pct_change(
  start_val = as.data.frame(df_conflict_tuareg_sub_fatalities)$Mali[1],
  end_val   = as.data.frame(df_conflict_tuareg_sub_fatalities)$Mali[3],
  sig_fig   = 2)

### max ratio of tuareg healthsites in mali
max(df_mali_healthsite_sub_collapse$pct_healthsites[df_mali_healthsite_sub_collapse$tuareg_region == 1])
df_mali_healthsite_sub_collapse$pct_healthsites[df_mali_healthsite_sub_collapse$year == 2000]

### percentage change in tuareg healthsite proliferation
fxn_pct_change(
  end_val = df_mali_healthsite_sub_collapse$pct_healthsites[
    df_mali_healthsite_sub_collapse$year == 1996 & df_mali_healthsite_sub_collapse$tuareg_region == 1],
  start_val   = df_mali_healthsite_sub_collapse$pct_healthsites[
    df_mali_healthsite_sub_collapse$year == 2000 & df_mali_healthsite_sub_collapse$tuareg_region == 1],
  sig_fig   = 2
)
