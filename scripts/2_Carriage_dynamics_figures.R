#Fig 2A overall carriage positivity
pvalue <- 
  pneumov_5 %>%
  ungroup() %>%
  filter(!is.na(hiv))

table(pvalue$hiv, pvalue$carriage)
chisq.test(pvalue$carriage, pvalue$hiv)
a <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "HIV-",     "PLHIV>1yr", 0.0002453)

A <-
  pneumov_5 %>%
  ungroup() %>%
  filter(!is.na(hiv)) %>%
  group_by(hiv, carriage) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2]) %>%
  filter(carriage != "no carriage") %>%
  
  ggplot(aes(x = hiv, y = prev)) +  
  geom_bar(stat = "identity", aes(fill = hiv)) +
  stat_pvalue_manual(a, y.position = 0.9, label = "p {scales::pvalue(p.adj)}", 
                     size = 8, bracket.size = 1) +
  ylim(0,1) +
  #geom_text(aes(label = n), position = position_dodge(0.9), size = 7, vjust = -2.2) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.2, position = position_dodge(0.9), linewidth = 0.8) +
  labs(title="A.", x = "", y = "overall pneumococcal nasopharyngeal carriage positivity") +
  theme_bw(base_size = 25, base_family = "Lato") +
  theme(plot.title = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  theme(legend.title = element_text(face= "bold"), legend.position = c(0.9, 0.878)) + #legend.text=element_text(size = 25), 
  guides(fill = guide_legend(title = "HIV status")) +
  #theme(legend.position = "none") + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  scale_fill_manual("", values = c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9"))

#Fig 2B _lineplot and bargraph overall carriage prevelance overtime
B <- 
  pneumov_5 %>%
  ungroup() %>%
  filter(!is.na(hiv)) %>%
  group_by(vday, hiv, hivst) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  #filter(vday != 0) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2],
         day = case_when(vday == 0 ~ 0, vday == 1 ~ 3, vday == 2 ~ 7, vday == 3 ~ 14, vday == 4 ~ 21, vday == 5 ~ 28, vday == 6 ~ 56,
                         vday == 7 ~ 84, vday == 8 ~ 112, vday == 9 ~ 140)) %>%
  filter(hivst != "None") %>%
  
  ggplot() + 
  geom_point(aes(x = day, y = prev, color = hivst, size = n), shape = 1) +
  geom_line(aes(x = day, y = prev, color = hivst), linewidth = 2) + 
  geom_ribbon(aes(x = day, y = prev, group = hivst, fill = hivst, color = hivst, ymin = obs_lci, ymax = obs_uci), alpha = 0.2, linewidth = 0.1) +
  #scale_x_discrete(limits=c("3","7","14","21","28","56","84","112","140")) +
  scale_x_continuous(limits = c(0, 140), breaks = c(0, 3, 7, 14, 21, 28, 56, 84, 112, 140)) +
  ylim(0, 1) +
  labs(title="B.", x = "Visit day", y = "Pneumococcal carriage prevalence") +
  theme_bw(base_size = 25, base_family = "Lato") +
  theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
  guides(fill = "none", color = guide_legend(title = "Serotype, HIV status"), size = guide_legend(title = "Sample size")) + # \n
  theme(legend.position = c(0.85, 0.75), legend.title = element_text(face= "bold")) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) 

#Fig 2C overall carriage positivity
pvalue <- 
  pneumov_5 %>%
  ungroup() %>%
  mutate(new = if_else(hivst == "VT, HIV-", "pos",
                       if_else(hivst == "VT, PLHIV>1yr", "pos", "neg", NA_character_)))

table(pvalue$hiv, pvalue$new)
chisq.test(pvalue$new, pvalue$hiv)
a <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "NVT, HIV-",     "NVT, PLHIV>1yr", 0.002426,
  "VT, HIV-",     "VT, PLHIV>1yr", 0.7577)

C <-
  pneumov_5 %>%
  ungroup() %>%
  filter(!is.na(hiv)) %>%
  group_by(hiv, hivst) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2]) %>%
  filter(hivst != "None") %>%
  
  ggplot(aes(x = hivst, y = prev)) +  
  geom_bar(stat = "identity", aes(fill = hivst)) +
  stat_pvalue_manual(a, y.position = 0.65, label = "p = {scales::pvalue(p.adj)}", size = 8, bracket.size = 1) +
  #geom_text(aes(label = n), position = position_dodge(0.9), size = 7, vjust = -2.2) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.2, position = position_dodge(0.9), linewidth = 0.8) +
  ylim(0,1) +
  labs(title="C.", x = "", y = "Overall pneumococcal nasopharyngeal carriage positivity") +
  theme_bw(base_size = 25, base_family = "Lato") +
  theme(plot.title = element_text(face = "bold"), axis.title.x = element_blank(), axis.title.y = element_text(face = "bold")) +
  guides(fill = guide_legend(title = "Serotype, HIV status"), size = guide_legend(title = "Sample size")) + #\n
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position = c(0.85, 0.9), legend.title = element_text(face= "bold")) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) 

#Fig 2D _bargraph proportion of PCV13 serotypes carriage positivity
D <-
  rbind(
    pneumov_5 %>%
      ungroup() %>%
      group_by(hiv, serotype) %>%
      filter(serogroup != "NVT", serogroup != "None") %>%
      tally(),
    
    tibble(
      "hiv" = c("HIV-", "HIV-", "HIV-", "PLHIV>1yr", "HIV-"),
      "serotype" = c("6A", "6B", "9V  n 3", "23F", "4"),
      "n" = c(0, 0, 0, 0, 0),
    )) %>%
  
  filter(serotype != "9V  n 3")  %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[1], NA_real_), 
         obs_uci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[2], NA_real_)) %>%
  
  ggplot(aes(x = as_factor(serotype), y = prev, fill = hiv)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  #scale_y_continuous(labels = scales::percent) +
  ylim(0,1) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), linewidth = 0.8) +
  scale_x_discrete(limits=c("3","4","6A","6B","9V","18C","19A", "19F", "23F"))  +
  labs(title="D.", x="Serotypes", y = "Overall proportion of PCV13 serotypes") +
  theme_bw(base_size = 25, base_family = "Lato") + 
  theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
  guides(fill=guide_legend(title = "HIV status"))  + 
  theme(legend.position = c(0.9, 0.878), legend.title = element_text(face= "bold")) +
  scale_fill_manual("", values = c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9"))

#Output Fig2
combined_plot <- plot_grid(A, B, C, D)

ggsave(here("output", "Fig2_carriage_char.tiff"), 
           plot = combined_plot, width = 30, height = 20, dpi = 300)

#ggsave(here("output", "Fig2_carriage_char.tiff"),
 #      plot = (A | B)/
  #       (C|D), width = 25, height = 20, units="in", dpi = 300)


#Fig 3_boxplot of pneumococcal carriage density overtime (logCFU/ml)
A <- 
  pneumov_5 %>%
  ungroup() %>%
  filter(!is.na(dens)) %>%
  mutate(log10(dens)) %>%
  group_by(vday, hiv, dens) %>%
  select(vday, hiv, dens) %>%
  #group_by(vday, hiv) %>%
  #summarize(median_dens = median(dens), .groups = "drop")
  mutate(day = case_when(vday == 0 ~ "0", vday == 1 ~ "3", vday == 2 ~ "7", vday == 3 ~ "14", vday == 4 ~ "21", vday == 5 ~ "28", 
                         vday == 6 ~ "56", vday == 7 ~ "84", vday == 8 ~ "112", vday == 9 ~ "140")) %>%
  
  ggplot(aes(x=day, y=log10(dens), fill = hiv)) +
  geom_boxplot(width=0.5) +
  scale_x_discrete(limits=c("3","7","14","21","28","56","84","112","140")) +
  ylim(0, 10) +
  labs(title="A.", x = "Visit day", y = "Pneumococcal carriage density \n (logCFU/ml)") +
  theme_bw(base_size = 25, base_family = "Lato") +
  theme(plot.title = element_text(size = 25, face= "bold"), axis.text = element_text(size=25, colour =  'black'), axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  theme(legend.position = "none") + 
  scale_fill_manual(values=c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9")) +
  theme(legend.position = c(0.9, 0.878), legend.title = element_text(face= "bold")) +
  guides(fill=guide_legend(title = "HIV status"))

#
my_comparisons <- list( c("HIV-", "PLHIV>1yr"))

densityCI <- pneumov_5 %>% 
  filter(!is.na(dens), dens != 0) %>%
  mutate(dens = log10(dens)) %>%
  group_by(hiv) %>% 
  summarize(Median = median(dens), SD = sd(dens),
            CI_L = Median - (SD * 1.96)/sqrt(50),
            CI_U = Median + (SD * 1.96)/sqrt(50))
densityCI
#Z=1.96 - 95%CI
#inverse_log_trans
#10^4 and 10^4.61

a <- pneumov_5 %>% 
  filter(!is.na(dens)) %>%
  wilcox_test(dens ~ hiv, comparisons = my_comparisons) %>%
  #add_xy_position(y = "hiv") %>%
  mutate(myformatted.p = paste0("p = ", p))

B <- 
  pneumov_5 %>% 
  filter(!is.na(dens)) %>%
  mutate(log10(dens)) %>%
  
  ggplot(aes(x=hiv, y=log10(dens))) +
  geom_violin(aes(fill = hiv)) +
  geom_boxplot(width=0.1) +
  ylim(0,10) +
  #scale_y_log10(labels = scales :: trans_format("log10",scales::math_format(10^.x)), limits = c(10^1, 10^10)) +
  labs(title="B.",x="Study group", y = "Pneumococcal carriage density \n (logCFU/ml)", fill = "") +
  theme_bw(base_size = 25, base_family = "Lato") +
  theme(plot.title = element_text(size = 25, face= "bold"), axis.text = element_text(size=25, colour =  'black'), axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  theme(legend.position = "none") + 
  scale_fill_manual(values=c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9")) +
  scale_x_discrete(label =c("HIV-"= "HIV-","PLHIV" = "PLHIV>1yr")) + #\n
  stat_pvalue_manual(a, y.position = 9.5, label = "p = {scales::pvalue(p)}", 
                     size = 8, bracket.size = 1)

X <- 
  pneumov_5 %>% 
  filter(!is.na(dens), vday != 0) %>%
  select(hiv,dens) %>%
  #group_by(hiv) %>%
  #tally()
  tbl_summary(by = hiv, missing = "no") %>% 
  add_p() %>%
  bold_labels()

#Fig 3_output
combined_plot <- plot_grid(A, B)

ggsave(here("output", "Fig3_carriage_char.tiff"), 
           plot = combined_plot, width = 30, height = 10, dpi = 300)

#ggsave(here("output", "Fig3_carriage_char.svg"),
 #      plot = (A), 
  #     width = 15, height = 10, unit="in", dpi = 300)
