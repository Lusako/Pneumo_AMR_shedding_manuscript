#theme
theme_gtsummary_compact(set_theme = TRUE, font_size = 25)

#Sup_Table1_overall_baseline demographics
pneumob %>%
  mutate(agegp = if_else(age >= 18 & age <= 25, "18-25", 
                         if_else(age > 25 & age <= 35, "26-35", 
                                 if_else(age > 35, "36-45", NA_character_))))%>%
  mutate(nochild = as.character(nochild))  %>%
  select(sex, agegp, nochild, ses, hiv, cd4, artdur, HIV_Viral_Load_GXP) %>%
  rename("Number of children" = "nochild",
         "Socioeconomic status" = "ses",
         "CD4 count (Cells/µl)" = "cd4",
         "Age group" = "agegp",
         "Sex" = "sex",
         "ART duration (Years)" = "artdur",
         "HIV viral load (Copies/ml)" = "HIV_Viral_Load_GXP") %>%
  tbl_summary(by = hiv, missing = "no", type = list(where(is.numeric) ~ "continuous2")) %>% 
  add_p() %>%
  bold_p(t = 0.06) %>%
  bold_labels() %>%
  modify_table_styling(
    columns = label,
    rows = label == "HIV viral load (Copies/ml)",
    footnote = "i.e. Only 13 PLHIV on ART>1yr had a detectable Viral load"
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
  gt::tab_header(title = "Supplementary Table 1. Overall Baseline characteristics")

#Sup_Table 2A_logistic regression NVT
uv_tab <- 
  pneumovC_5 %>%
  dplyr::select(NVTprev,hiv,sex,agegrp,season,ses,pid,vday) %>%
  tbl_uvregression(method = glmer,
                   y = NVTprev,
                   hide_n = TRUE,
                   method.args = list(family = binomial),
                   exponentiate = TRUE,
                   formula = "{y} ~ {x}+ (1|pid:vday)",
                   label = c(sex ~ "Sex", agegrp ~ "Age group (Years)",season = "Season", 
                             ses = "Socioeconomic status",hiv = "HIV status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

fita <- glmer(formula = NVTprev ~ hiv + sex + agegrp + season + ses + (1|pid:vday),
              data=pneumovC_5, family=binomial) 
summary(fita)

mv_tab <- 
  tbl_regression(fita, exponentiate = TRUE,
                 label = c(sex ~ "Sex", agegrp ~ "Age group (Years)",season = "Season", 
                           ses = "Socioeconomic status", hiv = "HIV status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

pneumovnvt <- 
  tbl_merge(tbls = list (uv_tab,mv_tab),
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
  gt::tab_header(title = "Supplementary Table 2a. NVT carriage vs NVT no carriage")

#Sup_Table 2B_logistic regression VT
uv_tab <- 
  pneumovC_5 %>%
  dplyr::select(VTprev,hiv,sex,agegrp,season,ses,pid,vday) %>%
  tbl_uvregression(method = glmer,
                   y = VTprev,
                   hide_n = TRUE,
                   method.args = list(family = binomial),
                   exponentiate = TRUE,
                   formula = "{y} ~ {x}+ (1|pid:vday)",
                   label = c(sex ~ "Sex", agegrp ~ "Age group (Years)",season = "Season", 
                             ses = "Socioeconomic status", hiv = "HIV status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

fitb <- glmer(formula = VTprev ~ hiv + sex + agegrp + season + ses + (1|pid:vday),
              data=pneumovC_5, family=binomial) 
summary(fitb)

mv_tab <- 
  tbl_regression(fitb, exponentiate = TRUE,
                 label = c(sex ~ "Sex", agegrp ~ "Age group (Years)",season = "Season", 
                           ses = "Socioeconomic status", hiv = "HIV status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

pneumovt <- 
  tbl_merge(tbls = list (uv_tab,mv_tab),
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
  gt::tab_header(title = "Supplementary Table 2b. VT carriage vs VT no carriage")


uv_tab <- 
  pneumovC_5 %>%
  dplyr::select(VTprev,hiv,sex,agegrp,season,ses,pid,vday) %>%
  tbl_uvregression(method = glmer,
                   y = VTprev,
                   hide_n = TRUE,
                   method.args = list(family = binomial),
                   exponentiate = TRUE,
                   formula = "{y} ~ {x}+ (1|pid:vday)",
                   label = c(sex ~ "Sex", agegrp ~ "Age group (Years)",season = "Season", 
                             ses = "Socioeconomic status",hiv = "HIV status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

fita <- glmer(formula = VTprev ~ hiv + sex + agegrp + season + ses + (1|pid:vday),
              data=pneumovC_5, family=binomial) 
summary(fita)

mv_tab <- 
  tbl_regression(fita, exponentiate = TRUE,
                 label = c(sex ~ "Sex", agegrp ~ "Age group (Years)",season = "Season", 
                           ses = "Socioeconomic status", hiv = "HIV status")) %>%
  bold_labels() %>% 
  bold_p(t = 0.06)

pneumovvt <- 
  tbl_merge(tbls = list (uv_tab,mv_tab),
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
  gt::tab_header(title = "Supplementary Table 2a. NVT carriage vs NVT no carriage")

#Sup_Table 3_HIV-
A<- 
  pneumoshed_AMR  %>%
  mutate(Benzylpenicillin = case_when(Benzylpenicillin == "Susceptible" ~ "Susceptible",
                                      Benzylpenicillin == "non-susceptible" ~ "non-susceptible",
                                      Oxacillin == "Susceptible" ~ "Susceptible",
                                      TRUE ~ "NA")) %>%
  filter(Method != "Mechanical shedding", HIV == "HIV-",  cough_shed == "shedding", serogroup != "None", Carriage == "carriage",
         pid != "PD071X", pid != "PD072Y", pid != "PD099C", pid != "PD1253", pid != "PD1261", pid != "PD128Y", pid != "PD129W",
         pid != "PD1309", pid != "PD1317", pid != "PD1325", pid != "PD1421", pid != "PD143X", pid != "PD144Y", pid != "PD145W",
         pid != "PD088I") %>%
  #group_by(pid, HIV, Method, serotype, Cotrimoxazole, Benzylpenicillin, Tetracycline, Erythromycin, MDR) %>%
  #tally() %>%
  #ungroup() %>%
  select(Method, Tetracycline, Erythromycin, Cotrimoxazole, Benzylpenicillin, MDR) %>%
  tbl_summary(by = Method,
              value = list(Tetracycline ~ "non-susceptible",
                           Erythromycin ~ "non-susceptible",
                           Cotrimoxazole ~ "non-susceptible",
                           Benzylpenicillin ~ "non-susceptible",
                           MDR ~ "MDR"))  %>%
  add_p %>%
  bold_labels()%>% 
  bold_p(t = 0.06)

B<- 
  pneumoshed_AMR  %>%
  mutate(Benzylpenicillin = case_when(Benzylpenicillin == "Susceptible" ~ "Susceptible",
                                      Benzylpenicillin == "non-susceptible" ~ "non-susceptible",
                                      Oxacillin == "Susceptible" ~ "Susceptible",
                                      TRUE ~ "NA")) %>%
  filter(Method != "Aerosol shedding", HIV == "HIV-",  Poke_shed == "shedding", serogroup != "None", Carriage == "carriage",
         pid != "PD071X", pid != "PD072Y", pid != "PD099C", pid != "PD1253", pid != "PD1261", pid != "PD128Y", pid != "PD129W",
         pid != "PD1309", pid != "PD1317", pid != "PD1325", pid != "PD1421", pid != "PD143X", pid != "PD144Y", pid != "PD145W",
         pid != "PD088I") %>%
  #group_by(pid, HIV, Method, serotype, Cotrimoxazole, Benzylpenicillin, Tetracycline, Erythromycin, MDR) %>%
  #tally() %>%
  #ungroup() %>%
  select(Method, Tetracycline, Erythromycin, Cotrimoxazole, Benzylpenicillin, MDR) %>%
  tbl_summary(by = Method, missing = "no",
              value = list(Tetracycline ~ "non-susceptible",
                           Erythromycin ~ "non-susceptible",
                           Cotrimoxazole ~ "non-susceptible",
                           Benzylpenicillin ~ "non-susceptible",
                           MDR ~ "MDR"))  %>%
  add_p %>%
  bold_labels()%>% 
  bold_p(t = 0.06)

HIV_neg <-
  tbl_merge(tbls = list (A,B),
            tab_spanner = c("**Aerosol shedding vs Nasopharyngeal carriage**", "**Mechanical shedding vs Nasopharyngeal carriage**")) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Benzylpenicillin",
    footnote = "i.e. Minimum inhibitory concentration"
  ) %>%
  
  as_gt() %>%
  gt::tab_header(title = "Supplementary Table 3. Pneumococcal shedding isolates antibiogram among HIV uninfected adults")

