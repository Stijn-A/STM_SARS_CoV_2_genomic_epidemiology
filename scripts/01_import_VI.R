# Import

data_teststraten_tmp <- 
  str_c(PATH_data, "Teststraten/Geschoond/Previous/Teststraten_data_20210928_0912.rds") %>% 
  readRDS()
  
data_osiris_tmp <- 
  str_c(PATH_data, "OSIRIS/Geschoond/Previous/Osiris_Data_20210928_1048.rds") %>% 
  readRDS()

data_variants_org <- 
  str_c(PATH_data, "IDS_sequentie_uitslagen/Previous/Lijst_voor_Epi_20210923.xlsx") %>% 
  read_excel()

data_oversampling_org <- 
  str_c(PATH_data, "IDS_samples_opgevraagd/csv") %>% 
  list.files(full.names = TRUE) %>% 
  str_subset(pattern = ".csv") %>% 
  map_dfr(~read_csv2(.x) %>% select(Monsternummer)) %>% 
  filter(!is.na(Monsternummer)) %>% # remove samples without community testing ID
  distinct(Monsternummer) # remove duplicates