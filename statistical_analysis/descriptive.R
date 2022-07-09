
# Cleaning ----------------------------------------------------------------

rm(list=ls())
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
             "pracma")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

kumamoto <- read_csv(here("data","final", "kumamoto.csv"),
                     locale = locale(encoding = "SHIFT-JIS")) %>% 
  mutate_all(.funs = ~ as.character(.)) 

kumamoto %>% glimpse()
kumamoto <- kumamoto %>% 
  unite(dcomnm, contains("dcom"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(ddev, contains("ddev"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(opek, contains("opek"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(openm, contains("openm"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(opedt, contains("opedt"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  mutate(age = trunc(time_length(interval(as.Date(birth_date), as.Date(adm_date)),"year")),
         los = disc_date - adm_date + 1,
         follow = last_date - diag_date + 1) %>% 
  mutate_all(.funs = ~ as.character(.))

kobe <- read_csv(here("data","final", "kobe.csv"),
                     locale = locale(encoding = "SHIFT-JIS")) %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  mutate(age = trunc(time_length(interval(as.Date(birth_date), as.Date(adm_date)),"year")),
         los = disc_date - adm_date + 1,
         follow = last_date - diag_date + 1) %>% 
  mutate_all(.funs = ~ as.character(.)) 

kobe %>% glimpse()

## adding image data

imaging <- read_csv(here("data","kobe", "cleaned","imaging.csv"),
                    locale = locale(encoding = "SHIFT-JIS")) %>% 
  rename(id = "ptid") %>% 
  select(id, pleural_max, loculation, inter_pleural_effusion, lung_abscess) %>% 
  mutate_all(.funs = ~ as.character(.)) 
imaging %>% glimpse()
kobe <- left_join(imaging, kobe, key = "id")

kobe %>% write.csv(here("data","final", "kobe.csv")) 
###

ichinomiya <- read_csv(here("data","final", "ichinomiya.csv"),
                     locale = locale(encoding = "SHIFT-JIS")) %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  mutate_all(.funs = ~ as.character(.)) 

ichinomiya <- ichinomiya %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  unite(dcom, contains("dcom"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(los = disc_date - adm_date + 1,
         follow = last_date - diag_date + 1) %>% 
  mutate_all(.funs = ~ as.character(.)) 
ichinomiya %>% glimpse()

yokohama <- read_csv(here("data","final", "yokohama.csv"),
                     locale = locale(encoding = "SHIFT-JIS")) %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  mutate(age = trunc(time_length(interval(as.Date(birth_date), as.Date(adm_date)),"year")),
         los = disc_date - adm_date + 1,
         follow = last_date - diag_date + 1) %>% 
  mutate_all(.funs = ~ as.character(.)) 

yokohama %>% glimpse()

tohoku <- read_csv(here("data","final", "tohoku.csv"),
                     locale = locale(encoding = "SHIFT-JIS")) %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  mutate(los = disc_date - adm_date + 1,
         follow = last_date - diag_date + 1) %>% 
  mutate_all(.funs = ~ as.character(.)) 

tohoku %>% glimpse()

tokyo <- read_csv(here("data","final", "tokyo.csv"),
                   locale = locale(encoding = "SHIFT-JIS")) %>% 
  mutate_at(vars(ends_with("date")), ymd) %>% 
  mutate(los = disc_date - adm_date + 1,
         follow = last_date - diag_date + 1) %>% 
  mutate_all(.funs = ~ as.character(.)) 

tokyo %>% glimpse()

df <- bind_rows(kumamoto, ichinomiya, yokohama, kobe, tohoku, tokyo)
df %>% glimpse()
df %>% colnames()
df %>% dim()

df_desc <- df %>%  
  select(id, adm_date, sex, birth_date, diag_date, disc_date, disc_outcome, death_24h, height, weight, adm_jcs,
         adm_adl, hughjones, openm, opek, sbp, dbp, hr, rr, spo2, o2, blood_tp, blood_alb, blood_ldh,
         blood_bun, blood_cre, blood_glucose, pleural_pH, pleural_ldh, pleural_tp, pleural_alb, pleural_glucose,
         dev_place, last_date, last_condition, fever, cough, sputum, chest_pain, weight_loss, surgery_3m, damage_3m,
         hot, hot_ryou_ansei, hot_ryou_rousa, pleural_look, hospid, dcom, dcomnm, openm, opek, opedt, drainage, age, los, follow,
         pleural_max, loculation, inter_pleural_effusion, lung_abscess) 

df_desc %>% write.csv("data/final/analysis_data.csv")


# Constructing analysis data ----------------------------------------------

df <- read_csv("data/final/analysis_data.csv",
               locale = locale(encoding = "SHIFT-JIS"), 
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
                 pleural_max = col_integer(),
                 loculation = col_integer(),
                 inter_pleural_effusion = col_integer(),
                 lung_abscess = col_integer()
               ),
               guess_max = 1500, #default: 1000
               na = c("NA"))

df_cat <- df %>% 
  select(id, sex, height, weight, adm_jcs, adm_adl, hughjones, blood_bun, age, pleural_look,
         dev_place, blood_alb, disc_outcome, last_date, last_condition, diag_date, los, follow,
         pleural_max, loculation, inter_pleural_effusion, lung_abscess) %>% 
  mutate(across(blood_bun:disc_outcome, .fns = ~{as.numeric(.)})) %>%
  mutate(bmi = 10000*weight/height^2,
         blood_bun_cat = case_when(blood_bun <= 14 ~ 0,
                               14 < blood_bun & blood_bun <= 23 ~ 1,
                               23 < blood_bun ~ 2),
         age_cat = case_when(age < 50 ~ 0,
                         50 <= age & age <= 70 ~ 1,
                         70 < age ~ 2),
         pleural_look_cat = if_else(pleural_look == 0, 0, 1),
         dev_place_cat = if_else(dev_place == 0, 0, 1),
         blood_alb_cat = if_else(blood_alb < 2.7, 1, 0),
         score = blood_bun_cat + age_cat + pleural_look_cat + dev_place_cat + blood_alb_cat,
         death30 = case_when(((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 0) ~ "NA",
                            ((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 1) ~ "1",
                            (ymd(last_date)- ymd(diag_date) >= 30) ~ "0"),
         death30 = as.numeric(death30),
         death90 = case_when(((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 0) ~ "NA",
                             ((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 1) ~ "1",
                             (ymd(last_date)- ymd(diag_date) >= 30) ~ "0"),
         death30 = as.numeric(death90)) %>% 
  select(-height, -weight) 

df_cat %>% glimpse()
df_cat %>% write.csv("data/final/analysis_data_cat.csv")


# Table 1 -----------------------------------------------------------------

df_cat %>% glimpse()

vars <- c("los", "sex", "follow", "disc_outcome", "death_24h", "height", "weight", "adm_jcs",
         "adm_adl", "hughjones", "sbp", "dbp", "hr", "rr", "spo2", "o2", "blood_tp", "blood_alb", "blood_ldh",
         "blood_bun", "blood_cre", "blood_glucose", 
         "dev_place", "last_condition", "fever", "cough", "sputum", "chest_pain", "weight_loss", "surgery_3m", "damage_3m",
         "hot", "hot_ryou_ansei", "hot_ryou_rousa", "pleural_look", "hospid", "drainage", "death30", "death90",
         "pleural_max", "loculation", "inter_pleural_effusion", "lung_abscess")

factorVars <- c("sex", "disc_outcome", "death_24h", "adm_jcs",
                "adm_adl", "hughjones",  "o2", 
                "dev_place", "last_condition", "fever", "cough", "sputum", "chest_pain", "weight_loss", "surgery_3m", "damage_3m",
                "hot", "hot_ryou_ansei", "hot_ryou_rousa", "pleural_look", "hospid", "drainage", "death30", "death90",
                "pleural_max", "loculation", "inter_pleural_effusion", "lung_abscess")

table1 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 <- table1 %>% 
  print(nonnormal = c("los", "follow"))

# RAPID score -------------------------------------------------------------

rapid <- df %>% 
  select(id, blood_bun, age, pleural_look, dev_place, blood_alb, disc_outcome, last_date, last_condition, diag_date) %>% 
  mutate(across(blood_bun:disc_outcome, .fns = ~{as.numeric(.)})) %>%
  drop_na() %>% 
  mutate(blood_bun = case_when(blood_bun <= 14 ~ 0,
                         14 < blood_bun & blood_bun <= 23 ~ 1,
                         23 < blood_bun ~ 2),
         age = case_when(age < 50 ~ 0,
                         50 <= age & age <= 70 ~ 1,
                         70 < age ~ 2),
         pleural_look = if_else(pleural_look == 0, 0, 1),
         dev_place = if_else(dev_place == 0, 0, 1),
         blood_alb = if_else(blood_alb < 2.7, 1, 0),
         score = blood_bun + age + pleural_look + dev_place + blood_alb) 

rapid %>% 
  filter((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 0) # check missing of outcome

rapid %>% 
  filter((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 1) 

rapid %>% 
  filter(ymd(last_date)- ymd(diag_date) >= 30)

rapid <- rapid %>% 
  mutate(death30 = case_when(((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 0) ~ "NA",
                             ((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 1) ~ "1",
                             (ymd(last_date)- ymd(diag_date) >= 30) ~ "0"),
         death30 = as.numeric(death30))

cat_vars <- c("blood_bun", "age", "pleural_look", "dev_place", "blood_alb", "disc_outcome", "score", "death30")
cat_factorVars <- c("blood_bun", "age", "pleural_look", "dev_place", "blood_alb", "disc_outcome", "score", "death30")
table_cat <- CreateTableOne(vars = cat_vars,
                         data = rapid,
                         includeNA = TRUE,
                         factorVars = cat_factorVars)
table_cat 
