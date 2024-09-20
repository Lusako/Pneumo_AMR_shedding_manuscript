#Load packages
pacman::p_load(char = c("ggpubr",'lubridate',"gtsummary", 'tidyverse', "dplyr", "here", "rio", "scales", 
                        "boot", "magrittr", "mvtnorm", "zoo", "patchwork", "mgcv", "PropCIs","sjstats",
                        "sjPlot", "oddsratio", "cowplot","lattice","ggcorrplot", "effects","glmmTMB","lme4",
                        "stringr", "broom", "ggsignif", "ggcorrplot", "rstatix", "stats", "gt", "webshot", "irr",
                        "writexl", "gt", "webr", "moonBook"))

#importing data for all participants
pneumo <- import(here("data", "pneumo.xlsx")) %>%
  rowwise() %>%
  mutate(ses = sum(watch1, radio1, bank1, ironic1, sew1, mattress1, bed1, bike1, moto1, car1, mobil1, cd1, fanelec1, netyn1, tv1, fridge1), na.rm = TRUE)

VL <- import(here("data", "VL.xlsx"))

pneumo <- full_join(pneumo,VL)

#baseline data manipulation for all participants
pneumob <- pneumo %>%
  select(ParticipantID, visit_day0, visit_date0, ART_start_date0, HIV_status0, serotype0, sex0, no_und0, density1, cd4_count1, ses, age0, Takenabx1, HIV_Viral_Load_GXP) %>% 
  mutate(visit = date(visit_date0), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit - ART_start_date0)/365.25), 
         carr = 1L) %>%
  ungroup() %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density1", 
         "cd4"= "cd4_count1", 
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype0",
         "age" = "age0",
         "date" = "visit_date0",
         "abx" = "Takenabx1") %>%
  select(pid, dens, sex, age, nochild, ses, hiv, artdur, cd4, serotype, abx, HIV_Viral_Load_GXP)

#define serotype and serogroup
pneumob <- 
  pneumob %>%
  ungroup() %>%
  mutate(serotype = if_else(serotype == 99, NA_character_, 
                            if_else(!is.na(serotype), serotype, NA_character_)),
         serogroup = if_else(serotype %in% c("1", "3", "4", "5", "6A", "6B", "7F ", "9V", "9V  n 3", "14", "18C", "19A", "19F", "23F") == TRUE, "VT", 
                             if_else(!is.na(serotype), "NVT", "None")),
         hiv = if_else(hiv == "HIV-", "HIV-",
                       if_else(hiv == "HIV+ART+", "PLHIV>1yr", NA_character_)))

#importing data (participant's that completed 5 months followup)
pneumo_5 <- import(here("data", "pneumodude_data_5m.xlsx"))

pneumo_5 <- left_join(pneumo_5,VL)

#data manupulating_5month followup change data format
v0 <- pneumo_5 %>%
  select(ParticipantID, visit_day0, visit_date0, ART_start_date0, HIV_status0, serotype0, sex0, no_und0, age0, lab_ID0, Takenabx1, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(density0 = NA,
         Takenabx0 = NA,
         cd4 = NA, 
         visit = date(visit_date0), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit - ART_start_date0)/365.25), 
         carr = 1L, 
         day = 0L,
         vday = 0L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density0",
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype0",
         "age" = "age0",
         "date" = "visit",
         "abx" = "Takenabx0",
         "Lab" = "lab_ID0") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v1 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date1, ART_start_date0, HIV_status0, serotype1, density1, sex0, no_und0,  age0, lab_ID1,Takenabx1, cd4_count1, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(visit_date0 = date(visit_date0), 
         visit_date1 = date(visit_date1), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date1 - ART_start_date0)/365.25), 
         carr = if_else(serotype1 == "99", 0L, 1L, NA_integer_), 
         day = as.integer(visit_date1 - visit_date0),
         vday = 1L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density1",
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype1",
         "age" = "age0",
         "date" = "visit_date1",
         "abx" = "Takenabx1",
         "Lab" = "lab_ID1",
         "cd4" = "cd4_count1") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v2 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date2, ART_start_date0, HIV_status0, serotype2, density2, sex0, no_und0,  age0, lab_ID2, Takenabx2, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(cd4 = NA,
         cd4_count2 = NA,
         visit_date0 = date(visit_date0), 
         visit_date2 = date(visit_date2), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date2 - ART_start_date0)/365.25), 
         carr = if_else(serotype2 == "99", 0L, 1L, NA_integer_), 
         day = as.integer(visit_date2 - visit_date0),
         vday = 2L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density2", 
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype2",
         "age" = "age0",
         "date" = "visit_date2",
         "abx" = "Takenabx2",
         "Lab" = "lab_ID2") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v3 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date3, ART_start_date0, HIV_status0, serotype3, density3, sex0, no_und0,  age0, lab_ID3, Takenabx3, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(cd4 = NA,
         visit_date0 = date(visit_date0), 
         visit_date3 = date(visit_date3), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date3 - ART_start_date0)/365.25), 
         carr = if_else(serotype3 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date3 - visit_date0),
         vday = 3L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density3", 
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype3",
         "age" = "age0",
         "date" = "visit_date3",
         "abx" = "Takenabx3",
         "Lab" = "lab_ID3") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v4 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date4, ART_start_date0, HIV_status0, serotype4, density4, sex0, no_und0,  age0, lab_ID4, Takenabx4, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(cd4 = NA,
         visit_date0 = date(visit_date0), 
         visit_date4 = date(visit_date4), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date4 - ART_start_date0)/365.25), 
         carr = if_else(serotype4 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date4 - visit_date0),
         vday = 4L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density4", 
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype4",
         "age" = "age0",
         "date" = "visit_date4",
         "abx" = "Takenabx4",
         "Lab" = "lab_ID4") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v5 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date5, ART_start_date0, HIV_status0, serotype5, density5, sex0, no_und0,  age0, lab_ID5, Takenabx5, cd4_count5, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(visit_date0 = date(visit_date0), 
         visit_date5 = date(visit_date5), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date5 - ART_start_date0)/365.25), 
         carr = if_else(serotype5 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date5 - visit_date0),
         vday = 5L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density5",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "hiv"= "HIV_status0",
         "serotype" = "serotype5",
         "age" = "age0",
         "date" = "visit_date5",
         "abx" = "Takenabx5",
         "Lab" = "lab_ID5",
         "cd4" = "cd4_count5") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v6 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date6, ART_start_date0, HIV_status0, serotype6, density6, sex0, no_und0,  age0, lab_ID6, Takenabx6, cd4_count6, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(visit_date0 = date(visit_date0), 
         visit_date6 = date(visit_date6), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date6 - ART_start_date0)/365.25), 
         carr = if_else(serotype6 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date6 - visit_date0),
         vday = 6L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density6",
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype6",
         "age" = "age0",
         "date" = "visit_date6",
         "abx" = "Takenabx6",
         "Lab" = "lab_ID6",
         "cd4" = "cd4_count6") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v7 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date7, ART_start_date0, HIV_status0, serotype7, density7, sex0, no_und0,  age0, lab_ID7, Takenabx7, cd4_count7, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(visit_date0 = date(visit_date0), 
         visit_date7 = date(visit_date7), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date7 - ART_start_date0)/365.25), 
         carr = if_else(serotype7 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date7 - visit_date0),
         vday = 7L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density7",
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype7",
         "age" = "age0",
         "date" = "visit_date7",
         "abx" = "Takenabx7",
         "Lab" = "lab_ID7",
         "cd4" = "cd4_count7") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v8 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date8, ART_start_date0, HIV_status0, serotype8, density8, sex0, no_und0,  age0, lab_ID8, Takenabx8, cd4_count8, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(visit_date0 = date(visit_date0), 
         visit_date8 = date(visit_date8), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date8 - ART_start_date0)/365.25), 
         carr = if_else(serotype8 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date8 - visit_date0),
         vday = 8L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density8",
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype8",
         "age" = "age0",
         "date" = "visit_date8",
         "abx" = "Takenabx8",
         "Lab" = "lab_ID8",
         "cd4" = "cd4_count8") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

v9 <- pneumo_5 %>%
  select(ParticipantID, visit_date0, visit_date9, ART_start_date0, HIV_status0, serotype9, density9, sex0, no_und0,  age0, lab_ID9, Takenabx9, cd4_count9, no_posse, HIV_Viral_Load_GXP) %>% 
  mutate(visit_date0 = date(visit_date0), 
         visit_date9 = date(visit_date9), 
         ART_start_date0 = date(ART_start_date0), 
         artdur = as.numeric((visit_date9 - ART_start_date0)/365.25), 
         carr = if_else(serotype9 == "99", 0L,  1L, NA_integer_), 
         day = as.integer(visit_date9 - visit_date0),
         vday = 9L) %>%
  rename("pid" = "ParticipantID", 
         "dens" = "density9",
         "hiv"= "HIV_status0",
         "nochild" = "no_und0",
         "sex" = "sex0",
         "serotype" = "serotype9",
         "age" = "age0",
         "date" = "visit_date9",
         "abx" = "Takenabx9",
         "Lab" = "lab_ID9",
         "cd4" = "cd4_count9") %>%
  select(pid, vday, day, date, carr, dens, sex, age,  nochild,  hiv, artdur, serotype, abx, cd4, no_posse, HIV_Viral_Load_GXP)

pneumo_5 <- rbind(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9)
rm (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9)

#define serotype_serogroup_hiv_agegrp_numchild_ses_carriage_hivst_densgrp
pneumov_5 <- 
  pneumo_5 %>% 
  mutate(serotype = if_else(serotype == 99, NA_character_, 
                            if_else(!is.na(serotype), serotype, NA_character_)),
         serogroup = if_else(serotype %in% c("1", "3", "4", "5", "6A", "6B", "7F ", "9V", "9V  n 3", "14", "18C", "19A", "19F", "23F") == TRUE, "VT", 
                             if_else(!is.na(serotype), "NVT", "None")),
         hiv = if_else(hiv == "HIV-", "HIV-",
                       if_else(hiv == "HIV+ART+", "PLHIV>1yr", NA_character_)),
         agegrp = if_else(age >= 18 & age <= 25, "18-25", 
                          if_else(age > 25 & age <= 35, "26-35", 
                                  if_else(age > 35, "36-45", NA_character_))),
         ses = if_else(no_posse <= 3, 1L,
                       if_else(no_posse > 3, 0L, NA_integer_)),
         carriage = if_else(serogroup == "None", 0L,
                            if_else(serogroup == "VT", 1L, 
                                    if_else(serogroup == "NVT", 1L, NA_integer_))),
         hivst = if_else(hiv=="HIV-" & serogroup == "VT", "VT, HIV-",
                         if_else(hiv=="HIV-" & serogroup == "NVT", "NVT, HIV-",
                                 if_else(hiv=="PLHIV>1yr" & serogroup == "VT", "VT, PLHIV>1yr", 
                                         if_else(hiv=="PLHIV>1yr" & serogroup == "NVT", "NVT, PLHIV>1yr", "None")))),
         densgrp = if_else(dens <= 2010, 0L, 
                           if_else(dens > 2010, 1L, NA_integer_)),
         artdurgrp = if_else(artdur <= 3.5, 1L, 
                             if_else(artdur > 3.5, 0L, NA_integer_)),
         cd4grp = if_else(cd4 <= 435, 1L, 
                          if_else(cd4 > 435, 0L, NA_integer_)),
         season = if_else(month(date) >= 5 & month(date) <= 10, "Cool dry", 
                          if_else(month(date) <= 4, "Hot wet", 
                                  if_else(month(date) > 10, "Hot wet", NA_character_))))

#Group data 
pneumov_5$carriage <- factor(pneumov_5$carriage, 
                             levels = c("0", "1"), 
                             labels = c("no carriage", "Carriage"))
pneumov_5$densgrp <- factor(pneumov_5$densgrp, 
                            levels = c("0", "1"), 
                            labels = c("Low density", "Medium/high density > 2010"))
pneumov_5$ses <- factor(pneumov_5$ses, 
                        levels = c("0", "1"), 
                        labels = c("Medium/high ses (> 3)", "Low ses (<= 3)"))
pneumov_5$artdurgrp <- factor(pneumov_5$artdurgrp, 
                              levels = c("0", "1"), 
                              labels = c("Medium/Long ART duration (> 3.5)", "Short ART duration (<= 3.5)"))
pneumov_5$cd4grp <- factor(pneumov_5$cd4grp, 
                           levels = c("0", "1"), 
                           labels = c("Medium/High cd4 (>= 435)", "Low cd4 (<435)"))

#for NVT and VT prevalence data analysis
pneumovC_5 <- 
  pneumov_5 %>%
  mutate(NVTprev = if_else(serogroup == "NVT", "pos",
                           if_else(serogroup != "NVT", "neg", NA_character_)),
         VTprev = if_else(serogroup == "VT", "pos",
                          if_else(serogroup != "VT", "neg", NA_character_)),
         NVTprev = as.factor(NVTprev),
         VTprev = as.factor(VTprev))

#AMR_5months
#import data
pneumoAMR_5 <- import(here("data", "month_AMR_5m.xlsx"))

#data manipulation
pneumoAMR_5 <- 
  pneumoAMR_5 %>%
  filter(visit_day != 168) %>%
  mutate(MDR = if_else(MDR < 3, 0L,
                       if_else(MDR >= 3, 1L, NA_integer_)))

pneumoAMR_5$cHIV_status <- factor(pneumoAMR_5$cHIV_status, 
                                  levels = c("0", "1"), 
                                  labels = c("HIV-", "PLHIV>1yr"))
pneumoAMR_5$cotrimoxazole_inter <- factor(pneumoAMR_5$cotrimoxazole_inter, 
                                          levels = c("0", "1"), 
                                          labels = c("Susceptible", "non-susceptible"))
pneumoAMR_5$oxacillin_inter <- factor(pneumoAMR_5$oxacillin_inter, 
                                      levels = c("0", "1"), 
                                      labels = c("Susceptible", "non-susceptible"))
pneumoAMR_5$tetracycline_inter <- factor(pneumoAMR_5$tetracycline_inter, 
                                         levels = c("0", "1"), 
                                         labels = c("Susceptible", "non-susceptible"))
pneumoAMR_5$erythromycin_inter <- factor(pneumoAMR_5$erythromycin_inter, 
                                         levels = c("0", "1"), 
                                         labels = c("Susceptible", "non-susceptible"))
pneumoAMR_5$MDR <- factor(pneumoAMR_5$MDR,
                          levels = c("0", "1"), 
                          labels = c("non MDR", "MDR"))

#importing data
pneumoshed <- import(here("data", "pneumoshed2.xlsx"))

pneumoshed <- 
  pneumoshed %>%
  filter(visit_day != 168, visit_day != 5) %>% 
  mutate(density_group = if_else(pns_density <= 2010, 0L, if_else(pns_density > 2010, 1L, NA_integer_)),
         numchild = if_else(no_und == 1, 0L, if_else(no_und > 1, 1L, NA_integer_)),
         csex = if_else(csex == 0, "Male", if_else(csex == 1, "Female", NA_character_)),
         age_group = if_else(age >= 18 & age <= 25, "18-25", 
                             if_else(age > 25 & age <= 35, "26-35", 
                                     if_else(age > 35, "36-45", NA_character_))),
         aero_shed = if_else(aero_shed == 0, "not shedding", if_else(aero_shed == 1, "shedding", NA_character_)),
         cough_shed = if_else(cough_shed == 0, "not shedding", 
                              if_else(cough_shed == 1, "shedding", NA_character_)),
         fm_shed = if_else(fm_shed == 0, "not shedding", 
                           if_else(fm_shed == 1, "shedding", NA_character_)),
         Poke_shed = if_else(Poke_shed == 0, "not shedding", 
                             if_else(Poke_shed == 1, "shedding", NA_character_)),
         all_shed_MDR = if_else(all_shed_MDR == 0, "not MDR",
                                if_else(all_shed_MDR == 1, "MDR", NA_character_)),
         pns_oxacillin_inter = if_else(pns_oxacillin_inter == 0, "Susceptible", 
                                       if_else(pns_oxacillin_inter == 1, "non-susceptible", NA_character_)),
         pns_tetracycline_inter = if_else(pns_tetracycline_inter == 0, "Susceptible",
                                          if_else(pns_tetracycline_inter == 1, "non-susceptible", NA_character_)),
         pns_erythromycin_inter = if_else(pns_erythromycin_inter == 0, "Susceptible",
                                          if_else(pns_erythromycin_inter == 1, "non-susceptible", NA_character_)),
         p_oxacillin_inter = if_else(p_oxacillin_inter == 0, "Susceptible",
                                     if_else(p_oxacillin_inter == 1, "non-susceptible", NA_character_)),
         p_tetracycline_inter = if_else(p_tetracycline_inter == 0, "Susceptible",
                                        if_else(p_tetracycline_inter == 1, "non-susceptible", NA_character_)),
         p_erythromycin_inter = if_else(p_erythromycin_inter == 0, "Susceptible",
                                        if_else(p_erythromycin_inter == 1, "non-susceptible", NA_character_)),
         c_oxacillin_inter = if_else(c_oxacillin_inter == 0, "Susceptible",
                                     if_else(c_oxacillin_inter == 1, "non-susceptible", NA_character_)),
         c_tetracycline_inter = if_else(c_tetracycline_inter == 0, "Susceptible",
                                        if_else(c_tetracycline_inter == 1, "non-susceptible", NA_character_)),
         c_erythromycin_inter = if_else(c_erythromycin_inter == 0, "Susceptible",
                                        if_else(c_erythromycin_inter == 1, "non-susceptible", NA_character_)),
         pns_benzylpenicillin = if_else(pns_PG_mg_L <= 0.06, "Susceptible",
                                        if_else(pns_PG_mg_L > 0.06, "non-susceptible", NA_character_)),
         pns_ceftriaxone = if_else(pns_TX_mg_L <= 0.5, "Susceptible",
                                   if_else(pns_TX_mg_L > 0.5, "non-susceptible", NA_character_)),
         p_benzylpenicillin = if_else(p_PG_mg_L <= 0.06, "Susceptible",
                                      if_else(p_PG_mg_L > 0.06, "non-susceptible", NA_character_)),
         p_ceftriaxone = if_else(p_TX_mg_L <= 0.5, "Susceptible",
                                 if_else(p_TX_mg_L > 0.5, "non-susceptible", NA_character_)),
         c_benzylpenicillin = if_else(c_PG_mg_L <= 0.06, "Susceptible",
                                      if_else(c_PG_mg_L > 0.06, "non-susceptible", NA_character_)),
         c_ceftriaxone = if_else(c_TX_mg_L <= 0.5, "Susceptible",
                                 if_else(c_TX_mg_L > 0.5, "non-susceptible", NA_character_)),
         all_shed_MDR = if_else(all_shed_MDR == 0, "non MDR",
                                if_else(all_shed_MDR == 1, "MDR", NA_character_)),
         Cough_MDR = if_else(Cough_MDR < 3, "non MDR",
                             if_else(Cough_MDR >= 1, "MDR", NA_character_)),
         Poke_MDR = if_else(Poke_MDR < 3, "non MDR",
                            if_else(Poke_MDR >= 3, "MDR", NA_character_)),
         pns_MDR = if_else(pns_MDR < 3, "non MDR",
                           if_else(pns_MDR >= 3, "MDR", NA_character_)),
         ses = if_else(no_posse <= 3, 1L,
                       if_else(no_posse > 3, 0L, NA_integer_)))


pneumoshed$chiv_status <- factor(pneumoshed$chiv_status, 
                                 levels = c("0", "1"), 
                                 labels = c("HIV-", "PLHIV>1yr"))
pneumoshed$density_group <- factor(pneumoshed$density_group, 
                                   levels = c("0", "1"), 
                                   labels = c("Low density <=2010", "Medium/High density >2010"))
pneumoshed$all_shed <- factor(pneumoshed$all_shed, 
                              levels = c("0", "1"), 
                              labels = c("not shedding", "shedding"))
pneumoshed$Carriage <- factor(pneumoshed$Carriage, 
                              levels = c("0", "1"), 
                              labels = c("no carriage", "carriage"))
pneumoshed$pns_cotrimoxazole_inter <- factor(pneumoshed$pns_cotrimoxazole_inter, 
                                             levels = c("0", "1"), 
                                             labels = c("Susceptible", "non-susceptible"))
pneumoshed$p_cotrimoxazole_inter <- factor(pneumoshed$p_cotrimoxazole_inter, 
                                           levels = c("0", "1"), 
                                           labels = c("Susceptible", "non-susceptible"))
pneumoshed$c_cotrimoxazole_inter <- factor(pneumoshed$c_cotrimoxazole_inter, 
                                           levels = c("0", "1"), 
                                           labels = c("Susceptible", "non-susceptible"))
pneumoshed$ses <- factor(pneumoshed$ses, 
                         levels = c("0", "1"), 
                         labels = c("Medium/high ses (> 3)", "Low ses (<= 3)"))

pneumoshed$season <- factor(pneumoshed$season, 
                            levels = c("1", "0"), 
                            labels = c("Cool dry", "Hot wet"))
pneumoshed$numchild <- factor(pneumoshed$numchild, 
                              levels = c("1", "0"), 
                              labels = c("2+ children", "1 child"))

#% months only
pneumoshed <- pneumoshed %>% 
  filter(Carriage == "carriage")

#shedding antibiogram data
PNS <- 
  pneumoshed %>%
  filter(Carriage == "carriage", visit_day != 5) %>%
  select(participantid, visit_day, pns_tetracycline_inter, pns_cotrimoxazole_inter, pns_erythromycin_inter, 
         pns_MDR, pns_oxacillin_inter,Poke_shed, cough_shed, chiv_status, pns_serotype_1, Carriage, pns_benzylpenicillin, pns_ceftriaxone, 
         pns_PG_mg_L, pns_TX_mg_L, lab_id) %>% 
  mutate(Method = 2L) %>%
  #filter(all_shed == "shedding") %>%
  ungroup() %>%
  rename("pid" = "participantid",
         "Tetracycline" = "pns_tetracycline_inter",
         "Erythromycin" = "pns_erythromycin_inter",
         "Cotrimoxazole" = "pns_cotrimoxazole_inter",
         "Oxacillin" = "pns_oxacillin_inter",
         "Benzylpenicillin" = "pns_benzylpenicillin",
         "Ceftriaxone" = "pns_ceftriaxone",
         "PG_mg_L" = "pns_PG_mg_L",
         "TX_mg_L" = "pns_TX_mg_L",
         "MDR" = "pns_MDR",
         "HIV" = "chiv_status",
         "serotype" = "pns_serotype_1") %>%
  select(pid, visit_day, Method, lab_id, Tetracycline, Erythromycin, Cotrimoxazole, Oxacillin, MDR, Benzylpenicillin, Ceftriaxone, PG_mg_L, TX_mg_L, HIV, serotype, Carriage, Poke_shed, cough_shed)

POKE <-
  pneumoshed %>%
  filter(Carriage == "carriage", visit_day != 5) %>%
  select(participantid, visit_day, p_tetracycline_inter, p_cotrimoxazole_inter, p_erythromycin_inter, 
         Poke_MDR, p_oxacillin_inter,Poke_shed, cough_shed, chiv_status, poke_serotype_1, Carriage, p_benzylpenicillin, p_ceftriaxone, 
         p_PG_mg_L, p_TX_mg_L, lab_id) %>% 
  mutate(Method = 1L) %>%
  #filter(all_shed == "shedding") %>%
  ungroup() %>%
  rename("pid" = "participantid",
         "Tetracycline" = "p_tetracycline_inter",
         "Erythromycin" = "p_erythromycin_inter",
         "Cotrimoxazole" = "p_cotrimoxazole_inter",
         "Oxacillin" = "p_oxacillin_inter",
         "Benzylpenicillin" = "p_benzylpenicillin",
         "Ceftriaxone" = "p_ceftriaxone",
         "PG_mg_L" = "p_PG_mg_L",
         "TX_mg_L" = "p_TX_mg_L",
         "MDR" = "Poke_MDR",
         "HIV" = "chiv_status",
         "serotype" = "poke_serotype_1") %>%
  select(pid, visit_day, Method, lab_id, Tetracycline, Erythromycin, Cotrimoxazole, Oxacillin, MDR, Benzylpenicillin, Ceftriaxone, PG_mg_L, TX_mg_L, HIV, serotype, Carriage, Poke_shed, cough_shed)

COUGH <-
  pneumoshed %>%
  filter(Carriage == "carriage", visit_day != 5) %>%
  select(participantid, visit_day, c_tetracycline_inter, c_cotrimoxazole_inter, c_erythromycin_inter, 
         Cough_MDR, c_oxacillin_inter, Poke_shed, cough_shed, chiv_status, cough_serotype_1, Carriage, c_benzylpenicillin, c_ceftriaxone, 
         c_PG_mg_L, c_TX_mg_L, lab_id) %>% 
  mutate(Method = 0L) %>%
  #filter(all_shed == "shedding") %>%
  ungroup() %>%
  rename("pid" = "participantid",
         "Tetracycline" = "c_tetracycline_inter",
         "Erythromycin" = "c_erythromycin_inter",
         "Cotrimoxazole" = "c_cotrimoxazole_inter",
         "Oxacillin" = "c_oxacillin_inter",
         "Benzylpenicillin" = "c_benzylpenicillin",
         "Ceftriaxone" = "c_ceftriaxone",
         "PG_mg_L" = "c_PG_mg_L",
         "TX_mg_L" = "c_TX_mg_L",
         "MDR" = "Cough_MDR",
         "HIV" = "chiv_status",
         "serotype" = "cough_serotype_1") %>%
  select(pid, visit_day, Method, lab_id, Tetracycline, Erythromycin, Cotrimoxazole, Oxacillin, MDR, Benzylpenicillin, Ceftriaxone, PG_mg_L, TX_mg_L, HIV, serotype, Carriage, Poke_shed, cough_shed)

pneumoshed_AMR <- rbind(PNS,POKE, COUGH)
rm(PNS,POKE,COUGH)

pneumoshed_AMR <- 
  pneumoshed_AMR %>%
  mutate(Method = if_else(Method == 2, "Nasopharyngeal carriage", 
                          if_else(Method == 1, "Mechanical shedding",
                                  if_else(Method == 0, "Aerosol shedding", NA_character_))),
         serotype = if_else(!is.na(serotype), serotype, NA_character_),
         serogroup = if_else(serotype %in% c("1", "3", "4", "5", "6A", "6B", "7F ", "9V", "9V  n 3", "14", "18C", "19A", "19F", "23F") == TRUE, "VT", 
                             if_else(!is.na(serotype), "NVT", "None")),
         hivst = if_else(HIV=="HIV-" & serogroup == "VT", "VT, HIV-",
                         if_else(HIV=="HIV-" & serogroup == "NVT", "NVT, HIV-",
                                 if_else(HIV=="PLHIV>1yr" & serogroup == "VT", "VT, PLHIV>1yr", 
                                         if_else(HIV=="PLHIV>1yr" & serogroup == "NVT", "NVT, PLHIV>1yr", "None")))))