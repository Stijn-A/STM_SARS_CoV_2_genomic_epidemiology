#Data for figures

list(
  `Fig. 1` = table_immune_status %>%
    mutate(value = value %>% replace_na(0)) %>%
    filter(
      sample_status %in% c(
        "Naive",
        "Partially vaccinated",
        "Fully vaccinated",
        "Previous infection"
      )
    ) %>%
    pivot_wider(names_from = Y, values_from = value) %>%
    select(
      `Week of sampling`,
      sample_status,
      Variant,
      Count,
      `Proportion (%)`
    ) %>%
    arrange(`Week of sampling`, sample_status, Variant),
  
  `Fig. 2` = tabel_figuur_variant_immunestatus,
  
  `Fig. S1` = table_osiris_immune %>%
    group_by(`Week of sampling`) %>%
    summarise(
      Count = n,
      sample_status = sample_status,
      `Proportion (%)` = Count /  sum(n) * 100
    ) %>%
    select(`Week of sampling`, sample_status, Count, `Proportion (%)`) %>%
    arrange(`Week of sampling`, sample_status),
  
  `Fig. S2` = table_immune_status %>%
    mutate(value = value %>% replace_na(0)) %>%
    filter(
      sample_status %in% c(
        "Naive",
        "Recently vaccinated",
        "Partially/fully vaccinated and\nprevious infection"
      )
    ) %>%
    pivot_wider(names_from = Y, values_from = value) %>%
    select(
      `Week of sampling`,
      sample_status,
      Variant,
      Count,
      `Proportion (%)`
    ) %>%
    arrange(`Week of sampling`, sample_status, Variant),
  
  `Fig. S3` = tabel_figuur_variant_immunestatus_full %>% 
    rename(Variable = Immuunstatus)
) %>% 
  write_xlsx("Data_figures_20220525.xlsx")
