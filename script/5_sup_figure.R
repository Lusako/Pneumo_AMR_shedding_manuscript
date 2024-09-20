#Histogram PLHIV on ART >1yr and HIV- pnrumococcal carriage density (<2010/3.3 CFU/ml Low carriage density)
my_binwidth3 <- 0.55  
D <-
  pneumov_5 %>%
  filter(!is.na(dens)) %>%
  mutate(dens = log10(dens)) %>%
  
  ggplot(aes(dens, fill = hiv)) + 
  geom_histogram(colour="black", binwidth = my_binwidth3, alpha=.4, position = position_dodge()) +
  geom_density(aes(y = after_stat(density) * (nrow(pneumov_5) * my_binwidth3)), col = 4, alpha=.4) + 
  geom_vline(aes(xintercept = 3.3), colour="red") +
  theme_bw(base_size = 25, base_family = "Lato") +
  scale_fill_manual(values=c("HIV-" = "#E69F00", "PLHIV>1yr" = "#56B4E9")) +
  #theme(legend.position = c(0.78, 0.89)) +
  guides(fill=guide_legend(title = "HIV status"))  + 
  labs(title="", x = "Pneumococcal carriage density (LogCFU/ml)", y = "Frequency of pneumococcal carriage density", fill = "")

ggsave(here("output", "SFig2_supplementary.tiff"),
       plot = (D), 
       width = 20, height = 10, unit="in", dpi = 300)