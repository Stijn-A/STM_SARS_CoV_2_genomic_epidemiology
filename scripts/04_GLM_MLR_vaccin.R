# per vaccin
n_knots <- 5

model_MLR_vaccin <- data_variants %>% 
  mutate(
    isoweek_monstername = isoweek(`Datum-monstername`),
    Leeftijdsgroep10 = Leeftijdsgroep10 %>% factor(levels = 
                                                     c("40-49","0-9", "10-19", "20-29", "30-39",
                                                       "50-59", "60-69", "70-79", "80+")),
    vaccin = case_when(
      Vaccinatie_merk == "Onbekend" & sample_status == "Naive"                                               ~ "Unk",
      # Fully vacc.
      Vaccinatie_merk == "Pfizer/BioNTech" & sample_status == "Fully vaccinated"                             ~ "Comirnaty, full",
      Vaccinatie_merk == "Moderna" & sample_status == "Fully vaccinated"                                     ~ "Spikevax, full",
      Vaccinatie_merk == "Janssen Phamaceutical Companies" & sample_status == "Fully vaccinated"       ~ "Janssen, full",
      Vaccinatie_merk == "Universiteit van Oxford / AstraZeneca" & sample_status == "Fully vaccinated" ~ "Vaxzevria, full",
      
      # Partially vacc.
      Vaccinatie_merk == "Pfizer/BioNTech" & sample_status == "Partially vaccinated"                            ~ "Comirnaty, part",
      Vaccinatie_merk == "Moderna" & sample_status == "Partially vaccinated"                                    ~ "Spikevax, part",
      Vaccinatie_merk == "Universiteit van Oxford / AstraZeneca" & sample_status == "Partially vaccinated"      ~ "Vaxzevria, part") %>% 
      factor(levels = c("Unk","Comirnaty, full","Spikevax, full",
                        "Janssen, full","Vaxzevria, full",
                        "Comirnaty, part","Spikevax, part",
                        "Vaxzevria, part"))) %>% 
  select(WHO_label,sample_status,vaccin,Leeftijdsgroep10, Geslacht,isoweek_monstername) %>% 
  filter(WHO_label != "Other" &
           sample_status %in% c("Naive","Partially vaccinated","Fully vaccinated", "Previous infection")) %>%
  droplevels() %>% 
  vglm(
    formula = WHO_label ~ ns(isoweek_monstername, df = n_knots) + vaccin + Leeftijdsgroep10 + Geslacht,
    family  = multinomial(refLevel = 1),
    data    = .)


OR_vaccin    <- model_MLR_vaccin %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_vaccin <- model_MLR_vaccin %>% confint() %>% exp() %>% as.data.frame()
tabel_figuur_variant_immunestatus_vaccin <- bind_cols(OR = OR_vaccin, ci = ci_OR_vaccin) %>% as_tibble() %>% 
  filter(str_detect(var, "vaccin")) %>% 
  mutate(
    var = var %>% str_replace(":1", ":Beta") %>% 
      str_replace(":2", ":Gamma") %>% 
      str_replace(":3", ":Delta") %>% 
      str_remove("sample_status"),
    label = str_c(format(round(OR,1), nsmall = 1, trim = T)
                  ," (",format(round(`2.5 %`,1), nsmall = 1, trim = T),"-", 
                  format(round(`97.5 %`,1), nsmall = 1, trim = T),")")) %>% 
  separate(var, into = c("Immuunstatus", "Variant"),  sep = ":") %>% 
  mutate(Variant = Variant %>% factor(levels = c("Beta", "Gamma", "Delta")))
