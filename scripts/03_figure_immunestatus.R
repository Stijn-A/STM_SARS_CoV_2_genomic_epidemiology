# Table for Figure 1 and Figure S2
table_immune_status <- data_variants %>% 
  mutate(`Datum-monstername` = as.Date(`Datum-monstername`, format = "%d-%m-%Y")) %>% 
  mutate(
    `Week-monstername` = `Datum-monstername` %>% 
      floor_date(unit = "week", week_start = 1) %>% 
      ISOweek(),
  ) %>% 
  count(
    `Week-monstername`, WHO_label,sample_status) %>% 
  group_by(`Week-monstername`,sample_status) %>% 
  summarise(
    Count = n, 
    #totaal = sum(n),
    `Proportion (%)` = Count /  sum(n) * 100,
    WHO_label = WHO_label %>% factor(levels = rev(c("Delta","Gamma","Beta", "Alpha", "Other")))) %>%
  ungroup() %>% 
  rename(`Week of sampling` = `Week-monstername`, `Variant` = WHO_label) %>% 
  pivot_longer(names_to = "Y", cols = c(Count, `Proportion (%)`)) %>% 
  complete(`Week of sampling`, Variant, Y, sample_status, fill = list(value = NA)) %>%  # fill x-axis
  filter(sample_status != "Unknown")
  
#
# Figure 1
#

my.cols <- c("#8DD3C7", "#FB8072", "#BEBADA", "#FFFFB3", "#80B1D3")

# P1 is the first row, P2 second row
theme_P1 <- list(
  theme_minimal() +
  theme(text = element_text(size=20),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)))

theme_P2 <- list(
  theme_minimal() +
  theme(text = element_text(size=20),
        strip.placement = "outside",
        strip.background = element_blank(),
        legend.position = "none",
        axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5)))

P1_naive <- table_immune_status %>% 
  filter(sample_status %in% c("Naive") & Y == "Count") %>%
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  ylab("Counts") +
  ggtitle("Naive")
  
legend <- get_legend(
  P1_naive + theme(legend.key.size = unit(1, 'cm'),
                   text = element_text(size=20)))

P1_naive <- P1_naive + theme_P1

P1_part <- table_immune_status %>% 
  filter(sample_status %in% c("Partially vaccinated") & Y == "Count") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P1 +
  ylab(NULL) +
  ggtitle("Partially vaccinated")
  
P1_full <- table_immune_status %>% 
  filter(sample_status %in% c("Fully vaccinated") & Y == "Count") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P1 +
  ylab(NULL) +
  ggtitle("Fully vaccinated")

P1_prev <- table_immune_status %>% 
  filter(sample_status %in% c("Previous infection") & Y == "Count") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P1 +
  ylab(NULL) +
  ggtitle("Previous infection")


P2_naive <- table_immune_status %>% 
  filter(sample_status %in% c("Naive") & Y == "Proportion (%)") %>%
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P2 +
  xlab(NULL) +
  ylab("Proportion (%)")

P2_part <- table_immune_status %>% 
  filter(sample_status %in% c("Partially vaccinated") & Y == "Proportion (%)") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P2 +
  xlab(NULL) +
  ylab(NULL)

P2_full <- table_immune_status %>% 
  filter(sample_status %in% c("Fully vaccinated") & Y == "Proportion (%)") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P2 +
  xlab(NULL) +
  ylab(NULL)

P2_prev <- table_immune_status %>% 
  filter(sample_status %in% c("Previous infection") & Y == "Proportion (%)") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P2 +
  xlab(NULL) +
  ylab(NULL)

x.grob <- textGrob("Week of sampling", 
                   gp=gpar(fontsize=20))

F1_immune_status <- plot_grid(
  plot_grid(P1_naive, P1_part, P1_full, P1_prev,P2_naive, P2_part, P2_full, 
            P2_prev, nrow = 2, rel_heights = c(0.8,1), rel_widths = c(1,1,1,1),  
            align=c("v")),
  legend,x.grob, ncol = 2, rel_widths = c(1,0.1), rel_heights = c(1,0.05))

#
# Figure S2
#

P1_recent <- table_immune_status %>% 
  filter(sample_status %in% c("Recently vaccinated") & Y == "Count") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P1 +
  ylab(NULL) +
  ggtitle("Recently vaccinated")

# Beta is not observed in this group
my.cols_short <- c("#8DD3C7", "#FB8072", "#FFFFB3", "#80B1D3")

P1_vac_prev <- table_immune_status %>% 
  filter(sample_status %in% c("Partially/fully vaccinated and\nprevious infection") & Y == "Count") %>%
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols_short) +
  scale_color_manual(values = my.cols_short) +
  theme_P1 +
  ylab(NULL) +
  ggtitle("Partially/fully vaccinated and\nprevious infection")


P2_recent <- table_immune_status %>% 
  filter(sample_status %in% c("Recently vaccinated") & Y == "Proportion (%)") %>%
  
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols) +
  scale_color_manual(values = my.cols) +
  theme_P2 +
  xlab(NULL) +
  ylab(NULL)

P2_vac_prev <- table_immune_status %>% 
  filter(sample_status %in% c("Partially/fully vaccinated and\nprevious infection") & Y == "Proportion (%)") %>%
  ggplot(data = ., aes(x = `Week of sampling`, y = value)) +
  geom_bar(stat = "identity",aes(fill = Variant, colour = Variant),
           alpha = .75) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_manual(values = my.cols_short) +
  scale_color_manual(values = my.cols_short) +
  theme_P2 +
  xlab(NULL) +
  ylab(NULL)

S2_immune_status <- plot_grid(
  plot_grid(P1_naive, P1_recent, P1_vac_prev, P2_naive, P2_recent, P2_vac_prev, 
            nrow = 2, rel_heights = c(0.8,1), rel_widths = c(1,1,1,1),  
            align=c("v")),
  legend,x.grob, ncol = 2, rel_widths = c(1,0.1), rel_heights = c(1,0.05))

#
# Figure S1
#

table_osiris_immune <- data_osiris_tmp %>% 
  filter(Afspraak_start_datum_CoronIT %in% seq(as.Date("2021-03-01"),as.Date("2021-08-31"),1)) %>% 
  mutate(
    `Week of sampling` = Afspraak_start_datum_CoronIT %>% 
      floor_date(unit = "week", week_start = 1) %>% 
      ISOweek(), 
    sample_status = sample_status %>% factor(levels = rev(c("Naive", "Recently vaccinated","Partially vaccinated", "Fully vaccinated", "Previous infection",
                      "Partially/fully vaccinated and\nprevious infection", "Unknown")))
  ) %>% 
  count(`Week of sampling`, sample_status)

P1_bar_immune_status_osiris <- table_osiris_immune %>% 
  ggplot(data = ., aes(x = `Week of sampling`, y = n)) +
  geom_bar(stat = "identity",aes(fill = sample_status, colour = sample_status),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  scale_color_brewer(type = "qual", palette = "Set3") +
  ylab("Counts") + 
  theme_minimal()

legend_osiris <- get_legend(
  P1_bar_immune_status_osiris + theme(legend.key.size = unit(1, 'cm'),
                   text = element_text(size=20),
                   legend.title = element_blank()))

P1_bar_immune_status_osiris <- P1_bar_immune_status_osiris +
  theme(text = element_text(size=20),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing=unit(0.5,"cm"),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# alle settingsgeom_bar(position="fill", stat="identity") +
P2_bar_immune_status_prop_osiris <- table_osiris_immune %>% 
  #filter(totaal > 10) %>% 
  ggplot(data = ., aes(x = `Week of sampling`, y = n)) +
  scale_y_continuous(labels = scales::percent, breaks = c(seq(0,1,0.1))) +
  geom_bar(position="fill", stat = "identity",aes(fill = sample_status, colour = sample_status),
           alpha = .75) +
  scale_x_discrete(breaks = function(x) {x[c(TRUE, FALSE)] <- '';x}) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  scale_color_brewer(type = "qual", palette = "Set3") +
  ylab("Proportion (%)") + 
  theme_minimal() +
  theme(text = element_text(size=20),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing=unit(0.5,"cm"),
        axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")

S1_osiris_immune_status <- plot_grid(
  plot_grid(P1_bar_immune_status_osiris, P2_bar_immune_status_prop_osiris,
            nrow = 2, rel_heights = c(0.8,1.2),  
            align=c("v")),
  legend_osiris,ncol = 2, rel_widths = c(1,0.4))

rm(P1_full, P1_naive, P1_part, P1_prev, P1_recent, P1_vac_prev, 
   P2_full, P2_naive, P2_part, P2_prev, P2_recent, P2_vac_prev,
   legend, legend_osiris)
