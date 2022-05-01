
# Cleaning ----------------------------------------------------------------

rm(list=ls())
packages = c("tidyverse",
             "here",
             "readxl",
             "data.table",
             "lubridate",
             "epitools",
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

df <- bind_rows(kumamoto, ichinomiya, yokohama, kobe)
df %>% glimpse()
df %>% colnames()
df %>% dim()

df_desc <- df %>%  
  select(id, adm_date, sex, birth_date, diag_date, disc_date, disc_outcome, death_24h, height, weight, adm_jcs,
         adm_adl, hughjones, openm, opek, sbp, dbp, hr, rr, spo2, o2, blood_tp, blood_alb, blood_ldh,
         blood_bun, blood_cre, blood_glucose, pleural_pH, pleural_ldh, pleural_tp, pleural_alb, pleural_glucose,
         dev_place, last_date, last_condition, fever, cough, sputum, chest_pain, weight_loss, surgery_3m, damage_3m,
         hot, hot_ryou_ansei, hot_ryou_rousa, pleural_look, hospid, dcom, dcomnm, openm, opek, opedt, drainage, age, los, follow) %>%  
  mutate(id = row_number()) 

df_desc %>% write.csv("data/final/analysis_data.csv")

df <- read_csv("data/final/analysis_data_updated.csv",
               locale = locale(encoding = "SHIFT-JIS"), 
               col_types = cols(
                 id = col_double(),
                 adm_date = col_character(),
                 sex = col_factor(),
                 birth_date = col_character(),
                 diag_date = col_character(),
                 disc_date = col_character(),
                 disc_outcome = col_factor(),
                 death_24h = col_factor(),
                 height = col_double(),
                 weight = col_double(),
                 adm_jcs = col_factor(),
                 adm_adl = col_factor(),
                 hughjones = col_factor(),
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
                 dev_place = col_factor(),
                 last_date = col_character(),
                 last_condition = col_factor(),
                 fever = col_factor(),
                 cough = col_factor(),
                 sputum = col_factor(),
                 chest_pain = col_factor(),
                 weight_loss = col_factor(),
                 surgery_3m = col_factor(),
                 damage_3m = col_factor(),
                 hot = col_factor(),
                 hot_ryou_ansei = col_integer(),
                 hot_ryou_rousa = col_integer(),
                 pleural_look = col_factor(),
                 hospid = col_factor(),
                 dcom = col_character(),
                 dcomnm = col_character(),
                 opedt = col_character(),
                 age = col_integer(),
                 los = col_integer(),
                 follow = col_integer()
               ),
               guess_max = 1500, #default: 1000
               na = c("NA"))

# Table 1 -----------------------------------------------------------------

df %>% glimpse()

vars <- c("los", "sex", "follow", "disc_outcome", "death_24h", "height", "weight", "adm_jcs",
         "adm_adl", "hughjones", "sbp", "dbp", "hr", "rr", "spo2", "o2", "blood_tp", "blood_alb", "blood_ldh",
         "blood_bun", "blood_cre", "blood_glucose", 
         "dev_place", "last_condition", "fever", "cough", "sputum", "chest_pain", "weight_loss", "surgery_3m", "damage_3m",
         "hot", "hot_ryou_ansei", "hot_ryou_rousa", "pleural_look", "hospid", "drainage")

factorVars <- c("sex", "disc_outcome", "death_24h", "adm_jcs",
                "adm_adl", "hughjones",  "o2", 
                "dev_place", "last_condition", "fever", "cough", "sputum", "chest_pain", "weight_loss", "surgery_3m", "damage_3m",
                "hot", "hot_ryou_ansei", "hot_ryou_rousa", "pleural_look", "hospid", "drainage")

table1 <- CreateTableOne(vars = vars,
                         data = df,
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
                             (ymd(last_date)- ymd(diag_date) >= 30) ~ "0"))

cat_vars <- c("blood_bun", "age", "pleural_look", "dev_place", "blood_alb", "disc_outcome", "score", "death30")
cat_factorVars <- c("blood_bun", "age", "pleural_look", "dev_place", "blood_alb", "disc_outcome", "score", "death30")
table_cat <- CreateTableOne(vars = cat_vars,
                         data = rapid,
                         includeNA = TRUE,
                         factorVars = cat_factorVars)
table_cat 

### Sensitivity and Specificity at different cut-off points

rapid %>% glimpse()

# Cut-off = 0
rapid <- rapid %>% 
  rename(death = "disc_outcome") %>% 
  mutate(death = if_else((death == 6 | death == 7), 1, 0))

predicted_values0 <- ifelse(rapid$score >= 0, 1, 0)
actual_values <- rapid$death
conf_matrix0 <- table(factor(predicted_values0, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr0 = conf_matrix0[2,2]/(conf_matrix0[2,1] + conf_matrix0[2,2])
fpr0 = conf_matrix0[2,1]/(conf_matrix0[2,1] + conf_matrix0[2,2])
x0 <- sensitivity(conf_matrix0)
y0 <- 1-specificity(conf_matrix0)

# Cut-off = 1
predicted_values1 <- ifelse(rapid$score >= 1, 1, 0)
actual_values <- rapid$death
conf_matrix1 <- table(factor(predicted_values1, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr1 = conf_matrix1[2,2]/(conf_matrix1[2,1] + conf_matrix1[2,2])
fpr1 = conf_matrix1[2,1]/(conf_matrix1[2,1] + conf_matrix1[2,2])
x1 <- sensitivity(conf_matrix1)
y1 <- 1-specificity(conf_matrix1)

# Cut-off = 2
predicted_values2 <- ifelse(rapid$score >= 2, 1, 0)
actual_values <- rapid$death
conf_matrix2 <- table(factor(predicted_values2, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr2 = conf_matrix2[2,2]/(conf_matrix2[2,1] + conf_matrix2[2,2])
fpr2 = conf_matrix2[2,1]/(conf_matrix2[2,1] + conf_matrix2[2,2])
x2 <- sensitivity(conf_matrix2)
y2 <- 1-specificity(conf_matrix2)

# Cut-off = 3
predicted_values3 <- ifelse(rapid$score >= 3, 1, 0)
actual_values <- rapid$death
conf_matrix3 <- table(factor(predicted_values3, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr3 = conf_matrix3[2,2]/(conf_matrix3[2,1] + conf_matrix3[2,2])
fpr3 = conf_matrix3[2,1]/(conf_matrix3[2,1] + conf_matrix3[2,2])
x3 <- sensitivity(conf_matrix3)
y3 <- 1-specificity(conf_matrix3)

# Cut-off = 4
predicted_values4 <- ifelse(rapid$score >= 4, 1, 0)
actual_values <- rapid$death
conf_matrix4 <- table(factor(predicted_values4, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr4 = conf_matrix4[2,2]/(conf_matrix4[2,1] + conf_matrix4[2,2])
fpr4 = conf_matrix4[2,1]/(conf_matrix4[2,1] + conf_matrix4[2,2])
x4 <- sensitivity(conf_matrix4)
y4 <- 1- specificity(conf_matrix4)

# Cut-off = 5
predicted_values5 <- ifelse(rapid$score >= 5, 1, 0)
actual_values <- rapid$death
conf_matrix5 <- table(factor(predicted_values5, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr5 = conf_matrix5[2,2]/(conf_matrix5[2,1] + conf_matrix5[2,2])
fpr5 = conf_matrix5[2,1]/(conf_matrix5[2,1] + conf_matrix5[2,2])
x5 <- sensitivity(conf_matrix5)
y5 <- 1-specificity(conf_matrix5)

# Cut-off = 6
predicted_values6 <- ifelse(rapid$score >= 6, 1, 0)
actual_values <- rapid$death
conf_matrix6 <- table(factor(predicted_values6, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr6 = conf_matrix6[2,2]/(conf_matrix6[2,1] + conf_matrix6[2,2])
fpr6 = conf_matrix6[2,1]/(conf_matrix6[2,1] + conf_matrix6[2,2])
x6 <- sensitivity(conf_matrix6)
y6 <- 1-specificity(conf_matrix6)

# Cut-off = 7
predicted_values7 <- ifelse(rapid$score >= 7, 1, 0)
actual_values <- rapid$death
conf_matrix7 <- table(factor(predicted_values7, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr7 = conf_matrix7[2,2]/(conf_matrix7[2,1] + conf_matrix7[2,2])
fpr7 = conf_matrix7[2,1]/(conf_matrix7[2,1] + conf_matrix7[2,2])
x7 <- sensitivity(conf_matrix7)
y7 <- 1-specificity(conf_matrix7)

# Describing ROC curve
roc <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                      Sensitivity = c(x0, x1, x2, x3, x4, x5, x6, x7),
                      Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
trapz(roc$Specificity, roc$Sensitivity) # Trapezoid integration
plot(Sensitivity ~ Specificity,
     data = roc,
     xlim = c(0,1),
     ylim = c(0,1),
     type="b",
     bg = "black") 


## 95% confidence interval
boot_num <- 2000
roc_bap_list <- c()
for(i in seq_len(boot_num)){
  set.seed(i)
  bap_data_bootstrap <-
    rapid[sample(dim(rapid)[1],dim(rapid)[1],replace=TRUE),]  
  # Cut-off = 0
  predicted_values0 <- ifelse(bap_data_bootstrap$score >= 0, 1, 0)
  actual_values <- bap_data_bootstrap$death
  conf_matrix0 <- table(factor(predicted_values0, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr0 = conf_matrix0[2,2]/(conf_matrix0[2,1] + conf_matrix0[2,2])
  fpr0 = conf_matrix0[2,1]/(conf_matrix0[2,1] + conf_matrix0[2,2])
  x0 <- sensitivity(conf_matrix0)
  y0 <- 1-specificity(conf_matrix0)
  # Cut-off = 1
  predicted_values1 <- ifelse(bap_data_bootstrap$score >= 1, 1, 0)
  conf_matrix1 <- table(factor(predicted_values1, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr1 = conf_matrix1[2,2]/(conf_matrix1[2,1] + conf_matrix1[2,2])
  fpr1 = conf_matrix1[2,1]/(conf_matrix1[2,1] + conf_matrix1[2,2])
  x1 <- sensitivity(conf_matrix1)
  y1 <- 1-specificity(conf_matrix1)
  # Cut-off = 2
  predicted_values2 <- ifelse(bap_data_bootstrap$score >= 2, 1, 0)
  conf_matrix2 <- table(factor(predicted_values2, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr2 = conf_matrix2[2,2]/(conf_matrix2[2,1] + conf_matrix2[2,2])
  fpr2 = conf_matrix2[2,1]/(conf_matrix2[2,1] + conf_matrix2[2,2])
  x2 <- sensitivity(conf_matrix2)
  y2 <- 1-specificity(conf_matrix2)
  # Cut-off = 3
  predicted_values3 <- ifelse(bap_data_bootstrap$score >= 3, 1, 0)
  conf_matrix3 <- table(factor(predicted_values3, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr3 = conf_matrix3[2,2]/(conf_matrix3[2,1] + conf_matrix3[2,2])
  fpr3 = conf_matrix3[2,1]/(conf_matrix3[2,1] + conf_matrix3[2,2])
  x3 <- sensitivity(conf_matrix3)
  y3 <- 1-specificity(conf_matrix3)
  # Cut-off = 4
  predicted_values4 <- ifelse(bap_data_bootstrap$score >= 4, 1, 0)
  conf_matrix4 <- table(factor(predicted_values4, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr4 = conf_matrix4[2,2]/(conf_matrix4[2,1] + conf_matrix4[2,2])
  fpr4 = conf_matrix4[2,1]/(conf_matrix4[2,1] + conf_matrix4[2,2])
  x4 <- sensitivity(conf_matrix4)
  y4 <- 1- specificity(conf_matrix4)
  # Cut-off = 5
  predicted_values5 <- ifelse(bap_data_bootstrap$score >= 5, 1, 0)
  conf_matrix5 <- table(factor(predicted_values5, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr5 = conf_matrix5[2,2]/(conf_matrix5[2,1] + conf_matrix5[2,2])
  fpr5 = conf_matrix5[2,1]/(conf_matrix5[2,1] + conf_matrix5[2,2])
  x5 <- sensitivity(conf_matrix5)
  y5 <- 1-specificity(conf_matrix5)
  # AUC
  roc_bap <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5),
                        Sensitivity = c(x0, x1, x2, x3, x4, x5),
                        Specificity = c(y0, y1, y2, y3, y4, y5))
  roc_bap_list <- c(roc_bap_list, trapz(roc_bap$Specificity, roc_bap$Sensitivity)) 
}
quantile(roc_bap_list,c(0+(1-0.95)/2, .5, 1-(1-0.95)/2))
