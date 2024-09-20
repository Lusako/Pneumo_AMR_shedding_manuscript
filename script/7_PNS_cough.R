#metadata
meta_data_6995 <- 
  sw_sequence_metadata %>%
  rename("Lane_id" = "Lane name") %>%
  select(-`Lane accession`, -`SAMPLE DESCRIPTION`, -`CONC. (ng/ul)`,
         -`Sample name`, -`Sample accession`)

Kraken_platesweep_6995 <- 
  import(here("../", "Kraken_qc_summary_6995.csv")) %>%
  rename("Lane_id" = "Species") %>%
  select(-Total) %>%
  pivot_longer(!Lane_id, names_to = "specie", values_to = "percentage")

Kraken_platesweep_6995_metadata <- full_join(Kraken_platesweep_6995,meta_data_6995)

PNS <- 
  Kraken_platesweep_6995_metadata %>% 
  filter(SAMPLE_TYPE == "PNS") %>%
  ungroup() %>%
  select(`SUPPLIER SAMPLE NAME`) %>% 
  distinct()

COUGH <-
  Kraken_platesweep_6995_metadata %>% 
  filter(SAMPLE_TYPE == "COUGH") %>%
  ungroup() %>%
  select(`SUPPLIER SAMPLE NAME`) %>%  
  distinct()

joined_data <- inner_join(COUGH, PNS)

PNS_COUGH_metadata_kraken <- left_join(joined_data, Kraken_platesweep_6995_metadata)  %>% 
  filter(SAMPLE_TYPE != "FM" & SAMPLE_TYPE != "POKE", hiv != "HIV-")


#Fig 1A
A <- 
  PNS_COUGH_metadata_kraken %>%
  filter(hiv == "PLHIV>1yr", percentage >= 1)  %>%
  group_by(`SUPPLIER SAMPLE NAME`, SAMPLE_TYPE,specie, percentage) %>%
  tally() %>%
  summarise(percentage = median(percentage)) %>%
  
  ggplot(aes(x = reorder(specie, -percentage,na.rm = TRUE), y = percentage, fill = SAMPLE_TYPE)) +
  geom_boxplot() +
  ylim(0,100) +
  labs(title="", x = "Nasopharyngeal Species", y = "Percentage of reads") +
  guides(fill = guide_legend(reverse=TRUE))  +
  theme_classic(base_size = 55) + 
  guides(fill = guide_legend(title = "SAMPLE TYPE")) +
  stat_compare_means(aes(group = SAMPLE_TYPE), method = "wilcox.test",
                     label.x = 0.6, label.y = 95, label = "p.signif" , hide.ns = TRUE, size = 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(here("output", "Fig 1A_PNS_cough_kraken.svg"),
       plot = (C), 
       width = 35, height = 30, unit="in", dpi = 300)

#carriage data
Serocall_6995 <- 
  import(here("../Genomic_data/Genome_projects/Sanger_analysis/Plate_sweep_analysis/Plate_sweeps_output/Output/","Serocall_results_6995.xlsx"))

#all sample types
Serocall_6995_meta_data <- left_join(Serocall_6995,meta_data_6995)  %>%
  filter(hiv != "PLHIV and ART_unknown") %>% 
  mutate(SEROGROUP = if_else(SEROTYPE %in% c(1, 3, 4, 5, "06A", "06B", "07F ", "09V",
                                             14, "18C", "19A", "19F", "23F") == TRUE, "VT", 
                             if_else(!is.na(SEROTYPE), "NVT", "None")),
         hiv = if_else(hiv == "HIV-", "HIV-", 
                       if_else(hiv == "PLHIV on ART >1yr", "PLHIV>1yr", NA_character_)))

#COUGH_PNS
PNS <- 
  Serocall_6995_meta_data %>% 
  filter(SAMPLE_TYPE == "PNS") %>%
  ungroup() %>%
  select(`SUPPLIER SAMPLE NAME`) %>% 
  distinct()

COUGH <-
  Serocall_6995_meta_data %>% 
  filter(SAMPLE_TYPE == "COUGH") %>%
  ungroup() %>%
  select(`SUPPLIER SAMPLE NAME`) %>%  
  distinct()

joined_data <- inner_join(COUGH, PNS)

PNS_COUGH_metadata_serocall <- left_join(joined_data, Serocall_6995_meta_data)  %>% 
  filter(SAMPLE_TYPE != "FM" & SAMPLE_TYPE != "POKE", hiv != "HIV-")

#Fig 1B
A_1 <- 
  PNS_COUGH_metadata_serocall %>%
  group_by(pid, hiv, `SUPPLIER SAMPLE NAME`, SAMPLE_TYPE, SEROTYPE) %>%
  tally() %>%
  ungroup() %>%
  
  group_by(SAMPLE_TYPE, SEROTYPE) %>%
  tally() %>%
  
  mutate(N = sum(n), prev = n/sum(n)) %>% 
  rowwise() %>%
  mutate(obs_lci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[1], NA_real_), 
         obs_uci = if_else(prev !=0, exactci(n, N, 0.95)$conf.int[2], NA_real_)) %>%
  
  ggplot(aes(x = prev, y = reorder(SEROTYPE, prev), fill = SAMPLE_TYPE)) + #-prev
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
  facet_wrap(~SAMPLE_TYPE) +
  labs(title="", x="Pneumococcal carriage serotypes prevalence", y = "Serotypes") +
  theme_classic(base_size = 55)

ggsave(here("output", "Fig 2B_PNS_COUGH_serotype.svg"),
       plot = (A_1), 
       width = 25, height = 40, unit="in", dpi = 300)