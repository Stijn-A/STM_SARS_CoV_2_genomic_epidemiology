# ## Figure 2 and S3 -- Generalized linear model multinomial logistic regression
n_knots <- 5
## colors
color_OR <- "#ffb612" # Dark yellow

# Figure 2
model_MLR <- data_variants %>% 
  select(WHO_label,sample_status,Leeftijdsgroep10, Geslacht,isoweek_monstername) %>% 
  filter(WHO_label != "Other" &
           sample_status %in% c("Naive","Partially vaccinated","Fully vaccinated", "Previous infection")) %>%
  droplevels() %>% 
  
  vglm(
    formula = WHO_label ~ ns(isoweek_monstername, df = n_knots) + sample_status + Leeftijdsgroep10 + Geslacht,
    family  = multinomial(refLevel = 1),
    data    = .)


OR    <- model_MLR %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR <- model_MLR %>% confint() %>% exp() %>% as.data.frame()

table_MLR_variant_immunestatus <- bind_cols(OR = OR, ci = ci_OR) %>% as_tibble() %>% 
  filter(str_detect(var, "sample_status")) %>% 
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

F2_variant_immune_adjusted <- table_MLR_variant_immunestatus %>% 
  ggplot(data = ., aes(x = OR, y = Immuunstatus, label = label)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = `97.5 %`, xmin = `2.5 %`), 
                 size = 0.7, 
                 height = 0.2, 
                 color = "gray50") +
  geom_point(size = 2.5, color = color_OR) +
  geom_label(aes(x = 9),label.size = NA) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 20)) +
  ylab("") +
  xlab("Odds ratio") +
  facet_wrap(vars(Variant), nrow = 3, strip.position = "left") + 
  coord_cartesian(xlim=c(0, 10))


# Figure S3
table_MLR_variant_immunestatus_full <- bind_cols(OR = OR, ci = ci_OR) %>% as_tibble() %>% 
  filter(str_detect(var, "sample_status|Leeftijdsgroep|Geslacht")) %>% 
  # Add empty reference class
  add_row(var = "Leeftijdsgroep1040-49:1", .before = 10) %>% 
  add_row(var = "Leeftijdsgroep1040-49:2", .before = 10) %>% 
  add_row(var = "Leeftijdsgroep1040-49:3", .before = 10) %>% 
  mutate(
    var = var %>% str_replace(":1", ":Beta") %>% 
      str_replace(":2", ":Gamma") %>% 
      str_replace(":3", ":Delta") %>% 
      str_remove("sample_status") %>% 
      str_replace("Leeftijdsgroep10", "Age ") %>% 
      str_replace("GeslachtV", "Sex F"),
    
    label = str_c(format(round(OR,1), nsmall = 1, trim = T)
                  ," (",format(round(`2.5 %`,1), nsmall = 1, trim = T),"-", 
                  format(round(`97.5 %`,1), nsmall = 1, trim = T),")"),
    label = if_else(is.na(OR), " ", label)
    
    ) %>% 
  separate(var, into = c("Immuunstatus", "Variant"),  sep = ":") %>% 
  mutate(Immuunstatus = Immuunstatus %>% 
           factor(levels = rev(c("Partially vaccinated",  "Fully vaccinated","Previous infection",
                    "Sex F", "Age 0-9", "Age 10-19", "Age 20-29", "Age 30-39","Age 40-49", 
                    "Age 50-59", "Age 60-69", "Age 70-79",  "Age 80+"))),
         Variant = Variant %>% factor(levels = c("Beta", "Gamma", "Delta")))


S3_OR_age_sex_immune_status <- table_MLR_variant_immunestatus_full %>% 
  ggplot(data = ., aes(x = OR, y = Immuunstatus, label = label)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = color_OR) +
  geom_label(aes(x = 9),label.size = NA) +
  scale_x_continuous(breaks = seq(0,15,1)) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 20)) +
  ylab("") +
  xlab("Odds ratio") +
  coord_cartesian(xlim=c(0, 10)) +
  facet_wrap(vars(Variant))


# Only kiemsurveillance, statement in text
model_MLR_kiem <- data_variants %>% 
  filter(selectie == "kiemsurveillance") %>% 
  select(WHO_label,sample_status,Leeftijdsgroep10, Geslacht,isoweek_monstername) %>% 
  filter(WHO_label != "Other" &
           sample_status %in% c("Naive","Partially vaccinated","Fully vaccinated", "Previous infection")) %>%
  droplevels() %>% 
  
  vglm(
    formula = WHO_label ~ ns(isoweek_monstername, df = n_knots) + sample_status + Leeftijdsgroep10 + Geslacht,
    family  = multinomial(refLevel = 1),
    data    = .)


OR_kiem    <- model_MLR_kiem %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_kiem <- model_MLR_kiem %>% confint() %>% exp() %>% as.data.frame()
table_MLR_variant_immunestatus_kiem <- bind_cols(OR = OR_kiem, ci = ci_OR_kiem) %>% as_tibble() %>% 
  filter(str_detect(var, "sample_status")) %>% 
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

