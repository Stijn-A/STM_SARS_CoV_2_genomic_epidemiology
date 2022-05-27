# Time since vaccination
n_knots <- 5

model_MLR_waning <- data_variants %>% 
  mutate(
    isoweek_monstername = isoweek(`Datum-monstername`),
    Leeftijdsgroep10 = Leeftijdsgroep10 %>% factor(levels = 
                                                     c("40-49","0-9", "10-19", "20-29", "30-39",
                                                       "50-59", "60-69", "70-79", "80+")),
    waning_status = 
      case_when(
        sample_status == "Naive" ~ "Naive",
        sample_status == "Partially vaccinated" ~ "Partially vaccinated",
        sample_status == "Fully vaccinated" & Vaccinatie_EZD_interval < 60 ~ "Fully vaccinated <60d",
        sample_status == "Fully vaccinated" & Vaccinatie_EZD_interval >= 60 ~ "Fully vaccinated >=60d",
        sample_status == "Previous infection" ~ "Previous infection") %>% 
      factor(levels = c("Naive", "Partially vaccinated", "Fully vaccinated <60d","Fully vaccinated >=60d","Previous infection"))) %>% 
  select(WHO_label,sample_status,waning_status,Leeftijdsgroep10, Geslacht,isoweek_monstername) %>% 
  filter(WHO_label != "Other" &
           sample_status %in% c("Naive","Partially vaccinated","Fully vaccinated", "Previous infection")) %>%
  droplevels() %>% 
  vglm(
    formula = WHO_label ~ ns(isoweek_monstername, df = n_knots) + waning_status + Leeftijdsgroep10 + Geslacht,
    family  = multinomial(refLevel = 1),
    data    = .)

OR_waning    <- model_MLR_waning %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_waning <- model_MLR_waning %>% confint() %>% exp() %>% as.data.frame()

table_MLR_variant_waning <- bind_cols(OR = OR_waning, ci = ci_OR_waning) %>% as_tibble() %>% 
  filter(str_detect(var, "waning_status")) %>% 
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
