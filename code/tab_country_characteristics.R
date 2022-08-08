######################################################################################################################################################
######

tab_country_charas <- stargazer::stargazer(df_country_charas, float = FALSE, model.numbers = TRUE)
starpolishr::star_tex_write(starlist = tab_country_charas, file = paste0(fp_tables, "/tab_country_characteristics.tex"))
