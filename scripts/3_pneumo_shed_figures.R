#Fig 4A_visit_day 3, 21 and 28 overall shedding stratified by HIV status
pvalue <- 
  pneumoshed %>%
  group_by(all_shed, chiv_status)

table(pvalue$all_shed, pvalue$chiv_status)
chisq.test(pvalue$all_shed, pvalue$chiv_status)

a <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "HIV-",     "PLHIV>1yr", 0.008992)

A<- 
  pneumoshed %>% 
  group_by(chiv_status, all_shed) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2]) %>%
  filter(all_shed != "not shedding") %>%
  
  
  ggplot(aes(x = chiv_status, y = prev)) +
  geom_bar(stat = "identity", aes(fill = chiv_status)) + 
  ylim(0,1) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), size = 0.8) + 
  #geom_text(aes(label = n), vjust = 5, position = position_dodge(.9), size = 8) +
  labs(title="A.", x="Study group", y = "Overall pneumococcal shedding positivity") +
  theme_bw(base_size = 25, base_family = "Lato") + 
  theme(plot.title = element_text(size = 25, face= "bold"), axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  theme(legend.position = "none") + 
  scale_fill_manual(values=c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9")) +
  scale_x_discrete(label =c("HIV-"= "HIV-","PLHIV" = "PLHIV>1yr")) + #\n
  stat_pvalue_manual(a, y.position = 0.8, label = "p = {scales::pvalue(p.adj)}", size = 10, bracket.size = 0.9, position = position_dodge(0.9))

#Fig 4B_visit_day 3, 21, 28 overall shedding stratified by HIV status and serogroup
pvalue <- 
  pneumoshed_AMR %>%
  filter(Method != "Nasopharyngeal carriage")  %>%
  ungroup() %>%
  mutate(new = if_else(hivst == "VT, HIV-", "pos",
                       if_else(hivst == "VT, PLHIV>1yr", "pos", "neg", NA_character_)))

table(pvalue$HIV, pvalue$new)
chisq.test(pvalue$new, pvalue$HIV)
a <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "NVT, HIV-",     "NVT, PLHIV>1yr", 0.0004783,
  "VT, HIV-",     "VT, PLHIV>1yr", 0.8337)

B <-
  pneumoshed_AMR %>%
  ungroup() %>%
  filter(Method != "Nasopharyngeal carriage") %>% #, Carriage == "carriage"
  group_by(HIV, hivst) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2]) %>%
  filter(hivst != "None") %>%
  
  ggplot(aes(x = hivst, y = prev)) +  
  geom_bar(stat = "identity", position = position_dodge(), aes(fill = hivst)) +
  #geom_text(aes(label = n), position = position_dodge(0.9), size = 7, vjust = -2.2) +
  ylim(0,1) +
  stat_pvalue_manual(a, y.position = 0.55, label = "p = {scales::pvalue(p.adj)}", size = 10, bracket.size = 0.9) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), linewidth = 0.8) +
  labs(title="B.", x = "", y = "Pneumococcal shedding positivity") +
  theme_bw(base_size = 25, base_family = "Lato") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(face = "bold", size = 25), axis.ticks.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 25))  + 
  theme(legend.position = "none") + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  guides(fill=guide_legend(title = "Serogroup, HIV status"))  + #\n
  theme(legend.position = c(0.65, 0.83),legend.text=element_text(size = 25), legend.title = element_text(size = 25, face= "bold"))

#Fig4c_visit_day 3, 21 and 28 overall shedding prevalence stratified by HIV status and serotyopes
C <-
  rbind(
    pneumoshed_AMR %>%
      ungroup() %>%
      filter(Method != "Nasopharyngeal carriage") %>%
      group_by(HIV, serotype) %>%
      filter(serogroup != "NVT", serogroup != "None") %>%
      tally(),
    
    tibble(
      "HIV" = c("HIV-", "HIV-"),
      "serotype" = c("23F", "6A"),
      "n" = c(0, 0)
    )) %>%
  
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[1], NA_real_), 
         obs_uci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[2], NA_real_)) %>%
  
  ggplot(aes(x = as_factor(serotype), y = prev, fill = HIV)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  #scale_y_continuous(labels = scales::percent) +
  ylim(0,1) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), size = 0.8) +
  scale_x_discrete(limits=c("3","6A", "19F", "23F"))  +
  labs(title="C.", x="Serotypes", y = "Overall proportion of PCV13 shedding serotypes") +
  theme_bw(base_size = 25, base_family = "Lato") + 
  theme(plot.title = element_text(face = "bold", size = 25), axis.title.x = element_text(face = "bold", size = 25), axis.title.y = element_text(face = "bold", size = 25)) +
  guides(fill=guide_legend(title = "HIV status"))  + 
  theme(legend.position = c(0.4, 0.9),legend.text=element_text(size = 25), legend.title = element_text(size = 25, face= "bold")) +
  scale_fill_manual("", values = c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9"))

#Fig 4D_visit_day 3, 21 and 28 mechanical shedding stratified by HIV status
pvalue <- 
  pneumoshed %>%
  group_by(Poke_shed, chiv_status)  %>%
  filter(!is.na(Poke_shed)) 

table(pvalue$Poke_shed, pvalue$chiv_status)
chisq.test(pvalue$Poke_shed, pvalue$chiv_status)

a <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "HIV-",     "PLHIV>1yr", 0.006297)

D<- 
  pneumoshed %>% 
  group_by(chiv_status, Poke_shed) %>%
  filter(!is.na(Poke_shed)) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2]) %>%
  filter(Poke_shed != "not shedding") %>%
  
  ggplot(aes(x = chiv_status, y = prev)) +
  geom_bar(stat = "identity", aes(fill = chiv_status)) + 
  ylim(0,1) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), size = 0.8) +
  labs(title="D.", x="Study group", y = "Mechanical shedding positivity") +
  theme_bw(base_size = 25, base_family = "Lato") + 
  theme(plot.title = element_text(size = 25, face= "bold"), axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  theme(legend.position = "none") + 
  scale_fill_manual(values=c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9")) +
  scale_x_discrete(label =c("HIV-"= "HIV-","PLHIV" = "PLHIV>1yr")) + #\n
  stat_pvalue_manual(a, y.position = 0.7, label = "p = {scales::pvalue(p.adj)}", size = 10, bracket.size = 0.9, position = position_dodge(0.9))

#Fig 4E_visit_day 3, 21 and 28 aerosol shedding stratified by HIV status
pvalue <- 
  pneumoshed %>%
  ungroup() %>%
  group_by(aero_shed, chiv_status) %>%
  filter(!is.na(aero_shed)) 

table(pvalue$aero_shed, pvalue$chiv_status)
chisq.test(pvalue$aero_shed, pvalue$chiv_status)
a <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "HIV-",     "PLHIV>1yr", 0.0326)

E<- 
  pneumoshed %>% 
  group_by(chiv_status, aero_shed) %>%
  filter(!is.na(aero_shed)) %>%
  tally() %>%
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = exactci(n, N, 0.95)$conf.int[1], 
         obs_uci = exactci(n, N, 0.95)$conf.int[2]) %>%
  filter(aero_shed != "not shedding") %>%
  
  ggplot(aes(x = chiv_status, y = prev)) +
  geom_bar(stat = "identity", aes(fill = chiv_status)) + 
  ylim(0,1) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), size = 0.8) +
  labs(title="E.", x="Study group", y = "Aerosol shedding positivity") +
  theme_bw(base_size = 25, base_family = "Lato") + 
  theme(plot.title = element_text(size = 25, face= "bold"), axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
  theme(legend.text=element_text(size = 25), legend.title = element_text(size = 25, face= "bold")) + 
  theme(legend.position = "none") + 
  scale_fill_manual(values=c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9")) +
  scale_x_discrete(label =c("HIV-"= "HIV-","PLHIV" = "PLHIV>1yr")) +
  stat_pvalue_manual(a, y.position = 0.7, label = "p = {scales::pvalue(p.adj)}", size = 10, bracket.size = 0.9, position = position_dodge(0.9))

#Fig4c_visit_day 3, 21 and 28 overall shedding prevalence stratified by Method and serotyopes
F <-
  rbind(
    pneumoshed_AMR %>%
      ungroup() %>%
      filter(Method != "Nasopharyngeal carriage") %>%
      group_by(Method, serotype) %>%
      filter(serogroup != "NVT", serogroup != "None") %>%
      tally(),
    
    tibble(
      "Method" = c("Aerosol shedding", "Aerosol shedding"),
      "serotype" = c("23F", "6A"),
      "n" = c(0, 0)
    )) %>%
  
  mutate(N = sum(n), prev = n/sum(n)) %>%
  rowwise() %>%
  mutate(obs_lci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[1], NA_real_), 
         obs_uci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[2], NA_real_)) %>%
  
  ggplot(aes(x = as_factor(serotype), y = prev, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  #scale_y_continuous(labels = scales::percent) +
  ylim(0,1) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), size = 0.8) +
  scale_x_discrete(limits=c("3","6A", "19F", "23F"))  +
  labs(title="F.", x="Serotypes", y = "Overall proportion of PCV13 shedding serotypes") +
  theme_bw(base_size = 25, base_family = "Lato") + 
  theme(plot.title = element_text(face = "bold", size = 25), axis.title.x = element_text(face = "bold", size = 25), axis.title.y = element_text(face = "bold", size = 25)) +
  guides(fill=guide_legend(title = ""))  + 
  theme(legend.position = c(0.6, 0.8),legend.text=element_text(size = 25), legend.title = element_text(size = 25, face= "bold")) #+
#scale_fill_manual("", values = c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9"))

#Fig 4 output
combined_plot <- plot_grid(A, B, C, D, E, F)

ggsave(here("output", "Fig4_shedding_new.tiff"), 
       plot = combined_plot, width = 30, height = 20, dpi = 300)
                           
#OLD
#ggsave(here("output", "Fig4_shedding_new.tiff"),
 #   plot = ((A | B|C | plot_layout(width = c(1,1,1))))/
  #    (D|E|F), width = 30, height = 20, unit="in", dpi = 300)

