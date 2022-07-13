# Setting-up ----------------------------------------------------------------

packages = c("tidyverse",
             "here",
             "readxl",
             "data.table",
             "lubridate",
             "epitools",
             "pastecs",
             "margins",
             "questionr",
             "modmarg",
             "fmsb",
             "psych",
             "arsenal",
             "tableone",
             "naniar",
             "ggplot2",
             "ggplotgui",
             "cowplot",
             "survival",
             "caret",
             "pracma",
             "broom")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

rm(list=ls())

# Data cleaning -----------------------------------------------------------

df <- read_csv("analysis_data_update.csv", 
               col_types = cols(
                 id = col_double(),
                 adm_date = col_character(),
                 sex = col_double(),
                 birth_date = col_character(),
                 diag_date = col_character(),
                 disc_date = col_character(),
                 disc_outcome = col_double(),
                 death_24h = col_double(),
                 height = col_double(),
                 weight = col_double(),
                 adm_jcs = col_double(),
                 adm_adl = col_double(),
                 hughjones = col_double(),
                 openm = col_character(),
                 opek = col_character(),
                 opedt = col_character(),
                 sbp = col_double(),
                 dbp = col_double(),
                 hr = col_double(),
                 rr = col_double(),
                 sop2 = col_double(),
                 o2 = col_double(),
                 blood_tp = col_double(),
                 blood_alb = col_double(),
                 blood_ldh = col_double(),
                 blood_bun = col_double(),
                 blood_cre = col_double(),
                 blood_glucose = col_double(),
                 pleural_pH = col_double(),
                 pleural_ldh = col_double(),
                 pleural_tp = col_double(),
                 pleural_alb = col_double(),
                 pleural_glucose = col_double(),
                 dev_place = col_double(),
                 last_date = col_character(),
                 last_condition = col_double(),
                 fever = col_double(),
                 cough = col_double(),
                 sputum = col_double(),
                 chest_pain = col_double(),
                 weight_loss = col_double(),
                 surgery_3m = col_double(),
                 damage_3m = col_double(),
                 copd = col_integer(),
                 diabetes = col_integer(),
                 renal = col_integer(),
                 malignancy = col_integer(),
                 hot = col_double(),
                 hot_ryou_ansei = col_integer(),
                 hot_ryou_rousa = col_integer(),
                 pleural_look = col_double(),
                 hospid = col_double(),
                 dcom = col_character(),
                 dcomnm = col_character(),
                 opedt = col_character(),
                 age = col_integer(),
                 los = col_integer(),
                 follow = col_integer(),
                 pleural_max = col_double(),
                 loculation = col_double(),
                 inter_pleural_effusion = col_double(),
                 lung_abscess = col_double()
               ),
               guess_max = 1000,
               na = c("NA"))

df_cat <- df %>% 
  select(id, sex, height, weight, adm_jcs, copd, diabetes, renal, malignancy,
         adm_adl, hughjones, blood_bun, age, 
         sbp, hr, rr, o2, hot, pleural_look,
         dev_place, blood_alb, ope, opedt, uk, disc_outcome, last_date, last_condition, diag_date, los, follow,
         pleural_max, loculation, inter_pleural_effusion, lung_abscess) %>% 
  mutate(across(blood_bun:disc_outcome, .fns = ~{as.numeric(.)})) %>%
  mutate(bmi = 10000*weight/height^2,
         adm_jcs = if_else(adm_jcs > 0, 1, 0),
         blood_bun_cat = case_when(blood_bun <= 14 ~ 0,
                                   14 < blood_bun & blood_bun <= 23 ~ 1,
                                   23 < blood_bun ~ 2),
         age_cat = case_when(age < 50 ~ 0,
                             50 <= age & age <= 70 ~ 1,
                             70 < age ~ 2),
         sbp_cat = if_else(sbp < 90, 1, 0),
         pleural_look_cat = case_when(pleural_look == 0 ~ 0,
                                      pleural_look == 1 ~ 1),
         dev_place_cat = case_when(dev_place == 0 ~ 0,
                                   dev_place == 1 ~ 1),
         blood_alb_cat = case_when(blood_alb < 2.7 ~ 0,
                                   blood_alb >= 2.7 ~ 1),
         score = blood_bun_cat + age_cat + pleural_look_cat + dev_place_cat + blood_alb_cat,
         death90 = case_when(((ymd(last_date)- ymd(diag_date) < 90) & last_condition == 0) ~ "NA",
                             ((ymd(last_date)- ymd(diag_date) < 90) & last_condition == 1) ~ "1",
                             (ymd(last_date)- ymd(diag_date) >= 90) ~ "0"),
         death365 = case_when(((ymd(last_date)- ymd(diag_date) < 365) & last_condition == 0) ~ "NA",
                              ((ymd(last_date)- ymd(diag_date) < 365) & last_condition == 1) ~ "1",
                              (ymd(last_date)- ymd(diag_date) >= 365) ~ "0"),
         death90 = as.numeric(death90),
         death365 = as.numeric(death365),
         early_ope = if_else((ymd(opedt)- ymd(diag_date) < 7), "1", "0"),
         early_ope = as.numeric(early_ope),
         pleural_max_cat = if_else(pleural_max > 2, 1, 0),
         loculation_cat = if_else(loculation > 1, 1, 0),
         inter_pleural_effusion_cat = if_else(inter_pleural_effusion > 1, 1, 0),
         lung_abscess_cat = if_else(lung_abscess > 1, 1, 0)) %>% 
  select(-height, -weight) %>% 
  drop_na(pleural_max_cat, loculation_cat, inter_pleural_effusion_cat, lung_abscess_cat, death90)

df_cat %>% glimpse()

df_cat %>% write.csv("stata.complete.case.csv")

# Table 1 and 2 -----------------------------------------------------------------

vars <- c("los", "age", "sex", "bmi","follow", "disc_outcome", "death_24h", "height", "weight", "adm_jcs",
          "copd", "diabetes", "renal", "malignancy",
          "adm_adl", "hughjones", "sbp", "dbp", "hr", "rr", "spo2", "o2", "sbp_cat",
          "blood_tp", "blood_alb", "blood_ldh",
          "blood_bun", "blood_cre", "blood_glucose", 
          "dev_place", "last_condition", "fever", "cough", "sputum", "chest_pain", "weight_loss", "surgery_3m", "damage_3m",
          "hot", "hot_ryou_ansei", "hot_ryou_rousa", "pleural_look", "hospid", "ope", "early_ope", "uk","drainage", "death30", "death90", "death365",
          "pleural_max_cat", "loculation_cat", "inter_pleural_effusion_cat", "lung_abscess_cat")

factorVars <- c("sex", "disc_outcome", "death_24h", "adm_jcs",
                "copd", "diabetes", "renal", "malignancy",
                "hughjones",  "o2", "sbp_cat",
                "dev_place", "last_condition", "fever", "cough", "sputum", "chest_pain", "weight_loss", "surgery_3m", "damage_3m",
                "hot", "hot_ryou_ansei", "hot_ryou_rousa", "pleural_look", "hospid", "ope", "early_ope", "uk", "drainage", "death30", "death90", "death365",
                "pleural_max_cat", "loculation_cat", "inter_pleural_effusion_cat", "lung_abscess_cat")

table1 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         factorVars = factorVars)
table1 <- table1 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

table2 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         strata = "death90",
                         factorVars = factorVars)
table2 <- table2 %>% 
  print(nonnormal = c("los", "follow"))

table2 %>% write.csv("table2.csv")

table3 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         strata = "ope",
                         factorVars = factorVars)
table3 <- table3 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

table3 %>% write.csv("table3.csv")

table4 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         strata = "early_ope",
                         factorVars = factorVars)
table4 <- table4 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

table5 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         strata = "uk",
                         factorVars = factorVars)
table5 <- table5 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

# Primary and secondary analyses ------------------------------------------

## primary outcomes

fit1_1 <- glm(death90 ~ pleural_max_cat,
              family = binomial(link = "logit"),
              data = df_cat)
ame1_1 <- margins(fit1_1, variables = "pleural_max_cat")
summary(ame1_1)

fit1_2 <- glm(death90 ~ loculation_cat,
              family = binomial(link = "logit"),
              data = df_cat)
ame1_2 <- margins(fit1_2, variables = "loculation_cat")
summary(ame1_2)

fit1_3 <- glm(death90 ~ inter_pleural_effusion_cat,
              family = binomial(link = "logit"),
              data = df_cat)
ame1_3 <- margins(fit1_3, variables = "inter_pleural_effusion_cat")
summary(ame1_3)

fit1_4 <- glm(death90 ~ lung_abscess_cat,
              family = binomial(link = "logit"),
              data = df_cat)
ame1_4 <- margins(fit1_4, variables = "lung_abscess_cat")
summary(ame1_4)
