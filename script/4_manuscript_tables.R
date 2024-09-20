#Theme
theme_gtsummary_compact(set_theme = TRUE, font_size = 25)

#Table 1_baseline demographics (5months follow-up)
pneumov_5 %>%
  filter(vday == 1)  %>%
  mutate(nochild = as.character(nochild))  %>%
  select(sex, agegrp, nochild, no_posse, hiv, cd4, artdur, HIV_Viral_Load_GXP) %>%
  rename("Number of U5 children per household" = "nochild",
         "Socioeconomic status" = "no_posse",
         "CD4 count (Cells/µl)" = "cd4",
         "Age group" = "agegrp",
         "Sex" = "sex",
         "ART duration (Years)" = "artdur",
         "HIV viral load (Copies/ml)" = "HIV_Viral_Load_GXP") %>%
  
  tbl_summary(by = hiv, missing = "no", type = list(where(is.numeric) ~ "continuous2")) %>% 
  add_p() %>%
  bold_labels() %>% 
  bold_p(t = 0.06) %>%
  modify_table_styling(
    columns = label,
    rows = label == "HIV viral load (Copies/ml)",
    footnote = "i.e. Only 10 PLHIV>1yr had a detectable Viral load"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Socioeconomic status",
    footnote = "Socioeconomic status score based a possession index which is calculated as a sum of positive responses for 
    household ownership of each of the fifteen different functioning items such as watch, radio, 
    bank account, iron (charcoal), sewing machine (electric), mobile phone, CD player, fan (electric), 
    bednet, mattress, bed, bicycle, motorcycle, car, and television"
  ) %>%
  
  as_gt() %>%
  tab_header(title = "Table 1. Baseline characteristics")

#Table 2_Univariate and multivariate analysis pneumococcal carriage density
pneumode_5 <- 
  pneumov_5

uv_tab1 <- 
  pneumode_5 %>%
  filter(!is.na(densgrp)) %>%
  dplyr::select(densgrp,hiv,sex,agegrp,pid,vday,season,ses) %>%
  tbl_uvregression(method = glmer,
                   y = densgrp,
                   hide_n = TRUE,
                   method.args = list(family = binomial),
                   exponentiate = TRUE,
                   formula = "{y} ~ {x}+ (1|pid:vday)",
                   label = c(hiv ~ "HIV status", sex ~ "Sex", agegrp ~ "Age group", 
                             ses = "Socioeconomic status", season = "Season")) %>%
  bold_labels()%>% 
  bold_p(t = 0.06)

fit1 <- glmer(formula = densgrp ~ hiv + sex + agegrp + season +  ses + (1|pid:vday),
              data=pneumode_5, family=binomial) 
summary(fit1)

mv_tab1 <- 
  tbl_regression(fit1, exponentiate = TRUE,
                 label = c(hiv ~ "HIV status", sex ~ "Sex", agegrp ~ "Age group",
                           ses = "Socioeconomic status", season = "Season")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)  %>% 
  modify_column_hide(columns = N)

density_uv_mv <- 
  tbl_merge(tbls = list (uv_tab1,mv_tab1),
            tab_spanner = c("**Univariate**", "**Multivariable**")) %>% 
  modify_table_styling(
    columns = label,
    rows = label == "Socioeconomic status",
    footnote = "Socioeconomic status score based a possession index which is calculated as a sum of positive responses for 
    household ownership of each of the fifteen different functioning items such as watch, radio, 
    bank account, iron (charcoal), sewing machine (electric), mobile phone, CD player, fan (electric), 
    bednet, mattress, bed, bicycle, motorcycle, car, and television. Middle and high SES were combined",
  ) %>%
  
  
  as_gt() %>%
  gt::tab_header(title = "Table 2. Pneumococcal density <=2010CFU/ml vs Pneumococcal density >2010CFU/ml")

#Table 3_Univariate and multivariate analysis pneumococcal shedding_ visit_day 3, 21 and 28
uv_tab2 <- 
  pneumoshed %>%
  select(all_shed, chiv_status, csex, age_group,participantid, visit_day,density_group, season, ses) %>%
  tbl_uvregression(method = glmer,
                   y = all_shed,
                   hide_n = TRUE,
                   method.args = list(family = binomial),
                   exponentiate = TRUE,
                   formula = "{y} ~ {x} + (1|participantid:visit_day)",
                   label = c(chiv_status ~ "HIV status", csex ~ "Sex", age_group ~ "Age group",
                             density_group ~ "Nasopharyngeal carriage density (CFU/ml)", season = "Season", 
                             ses = "Socioeconomic status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

fit2 <- glmer(formula = all_shed ~ chiv_status + csex + age_group + density_group + season + ses + (1|participantid:visit_day),
              data=pneumoshed, family=binomial)
summary(fit2)

mv_tab2 <- 
  tbl_regression(fit2, exponentiate = TRUE,
                 label = c(chiv_status ~ "HIV status", csex ~ "Sex", age_group ~ "Age group",
                           density_group ~ "Nasopharyngeal carriage density (CFU/ml)", season = "Season", 
                           ses = "Socioeconomic status")) %>%
  bold_labels()%>% 
  bold_p(t = 0.06) 

shedding_uv_mv <- 
  tbl_merge(tbls = list (uv_tab2,mv_tab2),
            tab_spanner = c("**Univariate**", "**Multivariable**")) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Socioeconomic status",
    footnote = "Socioeconomic status score based a possession index which is calculated as a sum of positive responses for 
    household ownership of each of the fifteen different functioning items such as watch, radio, 
    bank account, iron (charcoal), sewing machine (electric), mobile phone, CD player, fan (electric), 
    bednet, mattress, bed, bicycle, motorcycle, car, and television. Middle and high SES were combined",
  ) %>%
  
  # add table captions
  as_gt() %>%
  gt::tab_header(title = "Table 3. Factors associated with pneumococcal shedding")

#Table 4_carriage antibiogram
A <- 
  pneumoAMR_5 %>%
  filter(!is.na(cotrimoxazole_inter), visit_day == 1) %>%
  select(cotrimoxazole_inter, oxacillin_inter, tetracycline_inter, erythromycin_inter, MDR, cHIV_status)  %>%
  rename("Cotrimoxazole" = "cotrimoxazole_inter",
         "Oxacillin" = "oxacillin_inter",
         "Tetracycline" = "tetracycline_inter",
         "Erythromycin" = "erythromycin_inter",
         "MDR" = "MDR") %>%
  tbl_summary(by = cHIV_status, missing = "no")  %>%
  add_p  %>% 
  bold_labels() %>% 
  bold_p(t = 0.05) %>%
  #add_ci()
  modify_table_styling(
    columns = label,
    rows = label == "MDR",
    footnote = "i.e. Oxacillin is excluded from MDR definition"
  ) %>%
  
  as_gt() %>%
  gt::tab_header(title = "Table 4. Baseline pneumococcal carriage isolates antibiogram")

#Table 5_pneumoshed_AMR_PLHIV_5months_followup
A<- 
  pneumoshed_AMR  %>%
  mutate(Benzylpenicillin = case_when(Benzylpenicillin == "Susceptible" ~ "Susceptible",
                                      Benzylpenicillin == "non-susceptible" ~ "non-susceptible",
                                      Oxacillin == "Susceptible" ~ "Susceptible",
                                      TRUE ~ "NA")) %>%
  filter(Method != "Mechanical shedding", HIV == "PLHIV>1yr", cough_shed == "shedding", serogroup != "None",
         pid != "PD071X", pid != "PD072Y", pid != "PD099C", pid != "PD1253", pid != "PD1261", pid != "PD128Y", pid != "PD129W",
         pid != "PD1309", pid != "PD1317", pid != "PD1325", pid != "PD1421", pid != "PD143X", pid != "PD144Y", pid != "PD145W",
         pid != "PD088I", pid != "PD1077", pid != "PD0961", visit_day != 3 | pid != "PD103F") %>%
  #group_by(pid, HIV, Method, serotype, Cotrimoxazole, Benzylpenicillin, Tetracycline, Erythromycin, MDR) %>%
  #tally() %>%
  #ungroup() %>%
  select(Method, Cotrimoxazole, Benzylpenicillin, Tetracycline, Erythromycin, MDR) %>%
  tbl_summary(by = Method, missing = "no",
              value = list(Tetracycline ~ "non-susceptible",
                           Erythromycin ~ "non-susceptible",
                           Cotrimoxazole ~ "non-susceptible",
                           Benzylpenicillin ~ "non-susceptible",
                           MDR ~ "MDR"))  %>%
  add_p %>%
  bold_labels()%>% 
  bold_p(t = 0.06) #%>% 
#add_ci()

B<- 
  pneumoshed_AMR  %>%
  mutate(Benzylpenicillin = case_when(Benzylpenicillin == "Susceptible" ~ "Susceptible",
                                      Benzylpenicillin == "non-susceptible" ~ "non-susceptible",
                                      Oxacillin == "Susceptible" ~ "Susceptible",
                                      TRUE ~ "NA")) %>%
  filter(Method != "Aerosol shedding", HIV == "PLHIV>1yr",  Poke_shed == "shedding", serogroup != "None",
         pid != "PD071X", pid != "PD072Y", pid != "PD099C", pid != "PD1253", pid != "PD1261", pid != "PD128Y", pid != "PD129W",
         pid != "PD1309", pid != "PD1317", pid != "PD1325", pid != "PD1421", pid != "PD143X", pid != "PD144Y", pid != "PD145W",
         pid != "PD088I", pid != "PD1077") %>%
  #group_by(pid, HIV, Method, serotype, Cotrimoxazole, Benzylpenicillin, Tetracycline, Erythromycin, MDR) %>%
  #tally() %>%
  #ungroup() %>%
  select(Method, Cotrimoxazole, Benzylpenicillin, Tetracycline, Erythromycin, MDR) %>%
  tbl_summary(by = Method, missing = "no",
              value = list(Tetracycline ~ "non-susceptible",
                           Erythromycin ~ "non-susceptible",
                           Cotrimoxazole ~ "non-susceptible",
                           Benzylpenicillin ~ "non-susceptible",
                           MDR ~ "MDR"))  %>%
  add_p %>%
  bold_labels()%>% 
  bold_p(t = 0.06)

PLHIV <-
  tbl_merge(tbls = list (A,B),
            tab_spanner = c("**Aerosol shedding vs Nasopharyngeal carriage**", "**Mechanical shedding vs Nasopharyngeal carriage**")) %>% 
  modify_table_styling(
    columns = label,
    rows = label == "Benzylpenicillin",
    footnote = "i.e. Minimum inhibitory concentration"
  ) %>%
  
  as_gt() %>%
  gt::tab_header(title = "Table 5. Pneumococcal shedding isolates antibiogram among PLHIV")


