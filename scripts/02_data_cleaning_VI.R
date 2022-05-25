# Tue Jul 27 10:45:49 2021 ------------------------------
# data cleaning

# Testing data, coronit. Reduce number of rows and column. 
data_teststraten_tmp <- data_teststraten_tmp %>% 
  filter(!is.na(Monsternummer) & Afspraak_start_datum >= as.Date("2021-01-01")) %>% 
  select(Afspraak_start_datum, Monsternummer)

# Positive cases data, osiris. Reduce number of rows and column. Make immune status.
data_osiris_tmp <- data_osiris_tmp %>% 
  filter(Land == "Nederland") %>% 
  filter(!is.na(Monsternummer) & MELGGDOntvDt > as.Date("2021-01-01")) %>% 
  select(Herinfectie, Monsternummer,
         Vaccinatie_status_ezd_Desc, 
         Vaccinatie_EZD_interval,
         Leeftijdsgroep10, 
         Vaccinatie_merk, 
         Geslacht_osiris = Geslacht,
         infectiesoort, 
         Afspraak_start_datum_CoronIT,
         NCOVVast1eziektedag 
         ) %>%  
  mutate(sample_status = case_when(
    # recode vaccination and prevous infection status from surveillance database
           infectiesoort == "infectie" & !Herinfectie %in% c("Ja, bewezen herinfectie") &
             Vaccinatie_status_ezd_Desc %in% c("Geen")
           ~ "Naive",
           (infectiesoort == "herinfectie" | Herinfectie == "Ja, bewezen herinfectie") & 
             Vaccinatie_status_ezd_Desc %in% c("Geen")
           ~ "Previous infection",
           Vaccinatie_status_ezd_Desc == "Deels" &
             infectiesoort  == "infectie" & !Herinfectie %in% c("Ja, bewezen herinfectie")
           ~ "Partially vaccinated",
           Vaccinatie_status_ezd_Desc == "Volledig" &
             infectiesoort  == "infectie" & !Herinfectie %in% c("Ja, bewezen herinfectie")
           ~ "Fully vaccinated",
           (infectiesoort == "herinfectie" | Herinfectie == "Ja, bewezen herinfectie") & 
             Vaccinatie_status_ezd_Desc %in% c("Deels", "Volledig")
           ~ "Partially/fully vaccinated and\nprevious infection",
           Vaccinatie_status_ezd_Desc == "Geen (pas gevaccineerd)" ~ "Recently vaccinated",
           TRUE ~ "Unknown") %>% 
         factor(levels = c("Naive", "Partially vaccinated", "Fully vaccinated", "Previous infection",
                           "Partially/fully vaccinated and\nprevious infection", "Recently vaccinated","Unknown"))
      )

data_oversampling <- data_oversampling_org %>% 
  # select additional sampled isolates within the relevant vaccination and previous infection statuses.
  left_join(data_osiris_tmp %>% select(Monsternummer, Vaccinatie_status_ezd_Desc,infectiesoort) %>% 
              mutate(
                IDS_opgevraagd = T
                ),
            by = "Monsternummer") %>% 
  filter((Monsternummer %in% data_variants_org$`CoronIT Id` &
           Vaccinatie_status_ezd_Desc %in% c("Deels", "Volledig")) | 
           (Monsternummer %in% data_variants_org$`CoronIT Id` & 
           infectiesoort == "herinfectie")) %>% 
  select(-Vaccinatie_status_ezd_Desc, -infectiesoort)

data_variants <- data_variants_org %>% 
  mutate(`Datum-monstername` = as.Date(`Datum-monstername`, format = "%d-%m-%Y")) %>% 
  filter(`Datum-monstername` >= as.Date("2021-03-01") & `Datum-monstername` < as.Date("2021-09-01")) %>%
  filter(Monsterstroom %in% c("TESTSTRAAT", "STUDIE")) %>% 
  # data cleaning independent of CoronIT en OSIRIS
  mutate(
         WHO_label = case_when(
           Clade == "20I (Alpha, V1)"  ~ "Alpha",
           Clade == "20H (Beta, V2)"   ~ "Beta",
           Clade == "20J (Gamma, V3)"  ~ "Gamma",
           Clade == "21A (Delta)"      ~ "Delta",
           TRUE ~ "Other") %>% factor(levels = c("Alpha", "Beta","Gamma", "Delta", "Other")),
         
         # LR dependent variables
         Beta_alpha = case_when(
           Clade == "20H (Beta, V2)" ~ 1,
           Clade == "20I (Alpha, V1)" ~ 0),
         
         Gamma_alpha = case_when(
           Clade == "20J (Gamma, V3)" ~ 1,
           Clade == "20I (Alpha, V1)" ~ 0),
         
         Delta_alpha = case_when(
           Clade == "21A (Delta)" ~ 1,
           Clade == "20I (Alpha, V1)" ~ 0),
         
         # Rename "Monsternummer" for linkage Osiris / Coronit
         Monsternummer = `CoronIT Id` %>% as_factor(),
         Leeftijd      = Leeftijd %>% round(),
         Monsterstroom = Monsterstroom %>% factor(levels = c("TESTSTRAAT", "ZORG","STUDIE")),
         isoweek_monstername = isoweek(`Datum-monstername`)
         ) %>% 
  # Link coronit data
  left_join(data_teststraten_tmp,
            by = "Monsternummer") %>% 
  # Link osiris data
  left_join(data_osiris_tmp,
             by = "Monsternummer") %>% 
  # Link additional sampling
  left_join(data_oversampling,
            by = "Monsternummer") %>% 
  # Mutations dependent on CoronIT / Osiris
  mutate(
    vacc_status = case_when(
      Vaccinatie_status_ezd_Desc == "Geen" ~ "Not",
      Vaccinatie_status_ezd_Desc %in% c("Deels") ~ "Partially",
      Vaccinatie_status_ezd_Desc == "Volledig" ~ "Fully",
      TRUE ~ "Unknown"
    ),
    # Age groups
    Leeftijdsgroep10 = Leeftijdsgroep10 %>% factor(levels = 
                                                     c("40-49","0-9", "10-19", "20-29", "30-39",
                                                       "50-59", "60-69", "70-79", "80+")),
    # Sex
    Geslacht = case_when(!is.na(Geslacht) ~ Geslacht,
                         is.na(Geslacht) & Geslacht_osiris == "Man" ~ "M",
                         is.na(Geslacht) & Geslacht_osiris == "Vrouw" ~ "V",
    ) %>% factor(levels = c("M", "V")),
    
    vaccin = case_when(
      Vaccinatie_merk == "Onbekend" ~ "Unk",
      Vaccinatie_merk == "Pfizer/BioNTech" ~ "Comirnaty",
      Vaccinatie_merk == "Moderna" ~ "Spikevax",
      Vaccinatie_merk == "Janssen Phamaceutical Companies" ~ "Janssen",
      Vaccinatie_merk == "Universiteit van Oxford / AstraZeneca" ~ "Vaxzevria"
    ) %>% factor(levels = c("Unk","Comirnaty","Spikevax","Janssen","Vaxzevria")),
    
    selectie = case_when(
      Monsterstroom %in% c("TESTSTRAAT") ~ "kiemsurveillance", 
      IDS_opgevraagd == T ~ "over-sampling", 
      TRUE ~ "Rest")
  ) %>% 
  # filter samples from kiemsurveillance (TESTSTRAAT) and additional samples
  filter(
           selectie %in% c("kiemsurveillance","over-sampling")) %>% 
  filter(!is.na(`CoronIT Id`)) %>% 
  # remove duplicates
  arrange(Monsterstroom) %>% 
  group_by(`CoronIT Id`) %>% 
    slice(1) %>% 
    ungroup 

rm(data_oversampling_org)
