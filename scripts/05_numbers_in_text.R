
# introduction

# Fractie
JANS <- 792084
AZ <- 2800755
MOD <- 1958396
COM <- 17555618

round(JANS / (JANS + AZ + MOD + COM) * 100,1)
round(AZ / (JANS + AZ + MOD + COM) * 100,1)
round(MOD / (JANS + AZ + MOD + COM) * 100,1)
round(COM / (JANS + AZ + MOD + COM) * 100,1)

#
# Start table 1
#

#Tabel 1
## immune status
data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  count(sample_status, name = "National_surveillance") %>% 
  add_row(.before = 1, sample_status = "Totaal", National_surveillance = sum(.$National_surveillance)) %>% 
  mutate(`%` = round(National_surveillance / first(National_surveillance) * 100,1)) %>% 
  left_join(
    data_variants %>% 
      filter(selectie == "kiemsurveillance") %>% 
      filter(!is.na(sample_status)) %>% 
      count(sample_status, name = "Kiemsurveillance") %>% 
      add_row(.before = 1, sample_status = "Totaal", Kiemsurveillance = sum(.$Kiemsurveillance)) %>% 
      mutate(`% ` = round(Kiemsurveillance / first(Kiemsurveillance) * 100,1)),
    by = "sample_status"
    ) %>% 
  left_join(
    data_variants %>% 
      filter(selectie == "over-sampling") %>% 
      count(sample_status, name = "Additional_samples") %>% 
      add_row(.before = 1, sample_status = "Totaal", Additional_samples = sum(.$Additional_samples)) %>% 
      mutate(`%  ` = round(Additional_samples / first(Additional_samples) * 100,1)),
    by = "sample_status"
  )

# tabel 1
## Age
data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  count(Leeftijdsgroep10, name = "National_surveillance") %>% 
  add_row(.before = 1, Leeftijdsgroep10 = "Totaal", National_surveillance  = sum(.$National_surveillance)) %>% 
  mutate(`%` = round(National_surveillance / first(National_surveillance) * 100,1)) %>% 
  left_join(
  
    data_variants %>% 
      filter(selectie == "kiemsurveillance") %>% 
      filter(!is.na(sample_status)) %>% 
      count(Leeftijdsgroep10, name = "Kiemsurveillance") %>% 
      add_row(.before = 1, Leeftijdsgroep10 = "Totaal", Kiemsurveillance = sum(.$Kiemsurveillance)) %>% 
      mutate(`% ` = round(Kiemsurveillance / first(Kiemsurveillance) * 100,1)),
    by = "Leeftijdsgroep10") %>% 
  left_join(
  
    data_variants %>% 
      filter(selectie == "over-sampling") %>% 
      count(Leeftijdsgroep10, name = "Additional_samples") %>% 
      add_row(.before = 1, Leeftijdsgroep10 = "Totaal", Additional_samples = sum(.$Additional_samples)) %>% 
      mutate(`%  ` = round(Additional_samples / first(Additional_samples) * 100,1)),
    by = "Leeftijdsgroep10")

## Sex
data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  mutate(Geslacht = case_when(
    Geslacht_osiris == "Man" ~ "M",
    Geslacht_osiris == "Vrouw" ~ "V")) %>% 
  count(Geslacht, name = "National_surveillance") %>% 
  add_row(.before = 1, Geslacht = "Totaal", National_surveillance = sum(.$National_surveillance)) %>% 
  mutate(`%` = round(National_surveillance / first(National_surveillance) * 100,1)) %>% 
  left_join(
  
    data_variants %>% 
      filter(selectie == "kiemsurveillance") %>% 
      filter(!is.na(sample_status)) %>% 
      count(Geslacht, name = "Kiemsurveillance") %>% 
      add_row(.before = 1, Geslacht = "Totaal", Kiemsurveillance = sum(.$Kiemsurveillance)) %>% 
      mutate(`% ` = round(Kiemsurveillance / first(Kiemsurveillance) * 100,1)),
    by = "Geslacht") %>% 
  left_join(
    data_variants %>% 
      filter(selectie == "over-sampling") %>% 
      count(Geslacht, name = "Additional_samples") %>% 
      add_row(.before = 1, Geslacht = "Totaal", Additional_samples = sum(.$Additional_samples)) %>% 
      mutate(`%  ` = round(Additional_samples / first(Additional_samples) * 100,1)),
    by = "Geslacht")

# symptoms
data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  mutate(symptoms = case_when(
           NCOVVast1eziektedag %in% c("G", "V") ~ "Yes",  #Geschat en vastgesteld
           NCOVVast1eziektedag == "NVT" ~ "No",
           TRUE ~ "Unknown"
           )) %>%
  count(symptoms, name = "National_surveillance") %>% 
  add_row(.before = 1, symptoms = "Totaal", National_surveillance = sum(.$National_surveillance)) %>% 
  mutate(`%` = round(National_surveillance / first(National_surveillance) * 100,1)) %>% 
  left_join(
  
    data_variants %>% 
      filter(selectie == "kiemsurveillance") %>% 
      filter(!is.na(sample_status)) %>% 
      mutate(symptoms = case_when(
        NCOVVast1eziektedag %in% c("G", "V") ~ "Yes",  #Geschat en vastgesteld
        NCOVVast1eziektedag == "NVT" ~ "No",
        TRUE ~ "Unknown")) %>%
      count(symptoms, name = "Kiemsurveillance") %>% 
      add_row(.before = 1, symptoms = "Totaal", Kiemsurveillance = sum(.$Kiemsurveillance)) %>% 
      mutate(`% ` = round(Kiemsurveillance / first(Kiemsurveillance) * 100,1)),
    by = "symptoms") %>% 
  left_join(
    
    data_variants %>% 
      filter(selectie == "over-sampling") %>% 
      mutate(symptoms = case_when(
        NCOVVast1eziektedag %in% c("G", "V") ~ "Yes",  #Geschat en vastgesteld
        NCOVVast1eziektedag == "NVT" ~ "No",
        TRUE ~ "Unknown"
      )) %>%
      count(symptoms, name = "Additional_samples") %>% 
      add_row(.before = 1, symptoms = "Totaal", Additional_samples = sum(.$Additional_samples)) %>% 
      mutate(`%  ` = round(Additional_samples / first(Additional_samples) * 100,1)),
    by = "symptoms")


## Month (sampling date)
data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  mutate(Month = month(as.Date(Afspraak_start_datum_CoronIT), label = T)) %>% 
  count(Month, name = "National_surveillance") %>% 
  add_row(.before = 1, Month = "Totaal", National_surveillance = sum(.$National_surveillance)) %>% 
  mutate(`%` = round(National_surveillance / first(National_surveillance) * 100,1)) %>% 
  left_join(
    
    data_variants %>% 
      filter(selectie == "kiemsurveillance") %>% 
      filter(!is.na(sample_status)) %>% 
      mutate(Month = month(`Datum-monstername`, label = T)) %>% 
      count(Month, name = "Kiemsurveillance") %>% 
      add_row(.before = 1, Month = "Totaal", Kiemsurveillance = sum(.$Kiemsurveillance)) %>% 
      mutate(`% ` = round(Kiemsurveillance / first(Kiemsurveillance) * 100,1)),
    by = "Month") %>% 
  left_join(
    data_variants %>% 
      filter(selectie == "over-sampling") %>% 
      mutate(Month = month(`Datum-monstername`, label = T)) %>% 
      count(Month, name = "Additional_samples") %>% 
      add_row(.before = 1, Month = "Totaal", Additional_samples = sum(.$Additional_samples)) %>% 
      mutate(`%  ` = round(Additional_samples / first(Additional_samples) * 100,1)),
    by = "Month")

#
# End table 1
#

# geen Coronit ID bij de sequence data
data_variants_org %>% 
  mutate(`Datum-monstername` = as.Date(`Datum-monstername`, format = "%d-%m-%Y")) %>% 
  filter(`Datum-monstername` >= as.Date("2021-03-01") & `Datum-monstername` < as.Date("2021-09-01")) %>% # SA: vaststellen van de startdatum, dit is al redelijk vroeg (geen gevaccineerde verwacht)
  filter(Monsterstroom %in% c("TESTSTRAAT","ZORG")) %>% 
  filter(is.na(`CoronIT Id`)) %>% 
  nrow


# Per vaccine
data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  filter(Vaccinatie_status_ezd_Desc %in% c("Deels", "Volledig")) %>% 
  count( Vaccinatie_merk) %>% 
  filter(!Vaccinatie_merk == "Heteroloog: 1-AstraZeneca, 2-Pfizer") %>% 
  add_row(.before = 1, Vaccinatie_merk = "Totaal", n = sum(.$n)) %>% 
  mutate(`%  ` = round(n / first(n) * 100,1))
  


# Coronit ID niet in osiris
data_variants %>% 
  filter(selectie == "kiemsurveillance") %>% 
  filter(is.na(sample_status)) %>% nrow


data_variants %>% filter(is.na(sample_status)) %>% 
  count(Monsternummer %in% data_osiris_tmp$Monsternummer,
        Monsternummer %in% data_teststraten_tmp$Monsternummer)

data_variants %>% filter(!is.na(Beta_alpha) & !is.na(sample_status)) %>% 
  count(sample_status, Beta_alpha, Vaccinatie_status_ezd_Desc) 


data_variants %>% 
  filter(selectie == "kiemsurveillance" & !is.na(sample_status)) %>% 
  mutate(sample_month = month(`Datum-monstername`)) %>% 
  filter(
    sample_month < 6
  ) %>% 
  count(WHO_label) %>% 
  add_row(.before = 1, WHO_label = "Totaal", n = sum(.$n)) %>% 
  mutate(`%  ` = round(n / first(n) * 100,1))
  
data_variants %>% 
  filter(#selectie == "kiemsurveillance" & 
           !is.na(sample_status)) %>% 
  count(WHO_label) %>% 
  add_row(.before = 1, WHO_label = "Totaal", n = sum(.$n)) %>% 
  mutate(`%  ` = round(n / first(n) * 100,1))

data_variants %>% filter(sample_status == "Fully vaccinated" & vaccin == "Janssen") %>% 
  mutate(sample_month = month(`Datum-monstername`)) %>% 
  count(sample_month)
