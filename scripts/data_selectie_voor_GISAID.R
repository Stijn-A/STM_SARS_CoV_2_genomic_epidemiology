

data_variants_org %>% 
  select(`CoronIT Id`, `Monster-ID`) %>% 
  # selecteer alleen de gebruikte monster (bekende immuuntatus)
  filter(`CoronIT Id` %in% (data_variants %>% filter(sample_status != "Unknown" & !is.na(sample_status)) %>% pull(Monsternummer))) %>% 
  select(`Monster-ID`) %>% 
  write_xlsx("Monster_IDs_Alpha_Beta_Gamma_Delta_kiemsurv_studie.xlsx")
