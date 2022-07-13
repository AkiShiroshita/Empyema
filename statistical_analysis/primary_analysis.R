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
         sbp, hr, rr, o2, hot, pleural_look, mrsa, pseudo, esbl, ntm, tb, fungi,
         dev_place, blood_alb, ope, opedt, uk, disc_outcome, last_date, last_condition, diag_date, los, follow,
         pleural_max, loculation, inter_pleural_effusion, lung_abscess, fistula) %>% 
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
         death30 = if_else(((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 1), "1", "0"),
         death90 = if_else(((ymd(last_date)- ymd(diag_date) < 90) & last_condition == 1), "1", "0"),
         death365 = if_else(((ymd(last_date)- ymd(diag_date) < 365) & last_condition == 1), "1", "0"),
         #death30 = case_when(((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 0) ~ "NA",
         #                    ((ymd(last_date)- ymd(diag_date) < 30) & last_condition == 1) ~ "1",
         #                    (ymd(last_date)- ymd(diag_date) >= 30) ~ "0"),
         death30 = as.numeric(death30),
         #death90 = case_when(((ymd(last_date)- ymd(diag_date) < 90) & last_condition == 0) ~ "NA",
         #                    ((ymd(last_date)- ymd(diag_date) < 90) & last_condition == 1) ~ "1",
         #                    (ymd(last_date)- ymd(diag_date) >= 90) ~ "0"),
         death90 = as.numeric(death90),
         death365 = as.numeric(death365),
         early_ope = if_else((ymd(opedt)- ymd(diag_date) < 7), "1", "0"),
         early_ope = as.numeric(early_ope),
         pleural_max_cat = if_else(pleural_max > 2, 1, 0),
         loculation_cat = if_else(loculation > 1, 1, 0),
         inter_pleural_effusion_cat = if_else(inter_pleural_effusion > 1, 1, 0),
         lung_abscess_cat = if_else(lung_abscess > 1, 1, 0)) %>% 
  select(-height, -weight) %>% 
  drop_na(pleural_max_cat, loculation_cat, inter_pleural_effusion_cat, lung_abscess_cat) 

df_cat <- df_cat %>% 
  replace_na(list(early_ope = 0))

# MRSA: 9, pseudo: 5, esbl: 2, ntm: 1

df_cat %>% glimpse()
df_cat %>% arrange(diag_date) %>% select(diag_date) 
df_cat %>% arrange(diag_date) %>% select(diag_date) %>% filter(diag_date>2015/1/1)
df_cat %>% arrange(desc(diag_date)) %>% select(diag_date)

df_cat %>% write.csv("stata.complete.case.csv")


# Descriptive figures -----------------------------------------------------

library("ggplotgui")

df_cat_fig <- df_cat %>% 
  mutate(death90 = factor(death90, 
                          levels = c(0, 1),
                          labels = c("Alive", "Death")),
         pleural_max_cat = factor(pleural_max_cat, 
                          levels = c(0, 1),
                          labels = c("Present", "Not")),
         loculation_cat = factor(loculation_cat, 
                          levels = c(0, 1),
                          labels = c("Present", "Not")),
         inter_pleural_effusion_cat = factor(inter_pleural_effusion_cat, 
                          levels = c(0, 1),
                          labels = c("Present", "Not")),
         lung_abscess_cat = factor(lung_abscess_cat, 
                          levels = c(0, 1),
                          labels = c("Present", "Not")))

#ggplotgui::ggplot_shiny(data = df_cat_fig)

graph1 <- ggplot(df_cat_fig, aes(x = pleural_max)) +
  geom_density(position = 'identity', alpha = 0.8, adjust = 1) +
  facet_grid( death90 ~ . ) +
  labs(x = 'Maximum thickness of pleura (mm)', y = 'Density') +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    text = element_text(family = 'Helvetica'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph1

graph2 <- ggplot(df_cat_fig, aes(x = loculation)) +
  geom_density(position = 'identity', alpha = 0.8, adjust = 1) +
  facet_grid( death90 ~ . ) +
  labs(x = 'Maximum short-axis diameter of loculation (mm)', y = 'Density') +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    text = element_text(family = 'Helvetica'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph2

graph3 <- ggplot(df_cat_fig, aes(x = inter_pleural_effusion)) +
  geom_density(position = 'identity', alpha = 0.8, adjust = 1) +
  facet_grid( death90 ~ . ) +
  labs(x = 'Maximum short-axis diameter of interpleural effusion (mm)', y = 'Density') +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    text = element_text(family = 'Helvetica'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph3

# Table 1 and 2 -----------------------------------------------------------------

vars <- c("age", "sex", "adm_adl", "bmi", "hot", "renal", "copd", "diabetes", "malignancy",
          "adm_jcs", "o2", "sbp_cat", 
          "bun_cat", "age_cat", "pleural_look_cat", "dev_place_cat", "blood_alb_cat",
           "ope", "early_ope", "uk", "death90", "death365",
          "pleural_max_cat", "loculation_cat", "inter_pleural_effusion_cat", "lung_abscess_cat", "fistula")

factorVars <- c("sex", "hot", "renal", "copd", "diabetes", "malignancy",
                "adm_jcs", "o2", "sbp_cat", 
                "bun_cat", "age_cat", "pleural_look_cat", "dev_place_cat", "blood_alb_cat",
                "ope", "early_ope", "uk", "death90", "death365",
                "pleural_max_cat", "loculation_cat", "inter_pleural_effusion_cat", "lung_abscess_cat", "fistula")

table1 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         factorVars = factorVars)
table1 <- table1 %>% 
  print(showAllLevels = FALSE, formatOptions = list(big.mark = ","),
        nonnormal = c("adm_adl", "los", "follow"))

table1 %>% write.csv("table1.csv")

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

table4 %>% write.csv("table4.csv")

table5 <- CreateTableOne(vars = vars,
                         data = df_cat,
                         includeNA = TRUE,
                         strata = "uk",
                         factorVars = factorVars)
table5 <- table5 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

table5 %>% write.csv("table5.csv")
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

fit1_5 <- glm(death90 ~ fistula,
              family = binomial(link = "logit"),
              data = df_cat)
ame1_5 <- margins(fit1_5, variables = "fistula")
summary(ame1_5)

## Post-hoc analysis

fit3_1 <- glm(death90 ~ pleural_max_cat + early_ope,
              family = binomial(link = "logit"),
              data = df_cat)
ame3_1 <- margins(fit3_1, variables = "pleural_max_cat")
summary(ame3_1)

fit3_2 <- glm(death90 ~ loculation_cat + early_ope,
              family = binomial(link = "logit"),
              data = df_cat)
ame3_2 <- margins(fit3_2, variables = "loculation_cat")
summary(ame3_2)

fit3_3 <- glm(death90 ~ inter_pleural_effusion_cat + early_ope,
              family = binomial(link = "logit"),
              data = df_cat)
ame3_3 <- margins(fit3_3, variables = "inter_pleural_effusion_cat")
summary(ame3_3)

fit3_4 <- glm(death90 ~ lung_abscess_cat + early_ope,
              family = binomial(link = "logit"),
              data = df_cat)
ame3_4 <- margins(fit3_4, variables = "lung_abscess_cat")
summary(ame3_4)

fit3_5 <- glm(death90 ~ fistula + early_ope,
              family = binomial(link = "logit"),
              data = df_cat)
ame3_5 <- margins(fit3_5, variables = "fistula")
summary(ame3_5)

## secondary outcomes

fit2_1 <- glm(los ~ pleural_max_cat,
              family = Gamma(link = "log"),
              data = df_cat)
ame2_1 <- margins(fit2_1, variables = "pleural_max_cat")
summary(ame2_1)

fit2_2 <- glm(los ~ loculation_cat,
              family = Gamma(link = "log"),
              data = df_cat)
ame2_2 <- margins(fit2_2, variables = "loculation_cat")
summary(ame2_2)

fit2_3 <- glm(los ~ inter_pleural_effusion_cat,
              family = Gamma(link = "log"),
              data = df_cat)
ame2_3 <- margins(fit2_3, variables = "inter_pleural_effusion_cat")
summary(ame2_3)

fit2_4 <- glm(los ~ lung_abscess_cat,
              family = Gamma(link = "log"),
              data = df_cat)
ame2_4 <- margins(fit2_4, variables = "lung_abscess_cat")
summary(ame2_4)

fit2_5 <- glm(los ~ fistula,
              family = Gamma(link = "log"),
              data = df_cat)
ame2_5 <- margins(fit2_5, variables = "fistula")
summary(ame2_5)

# RAPID score -------------------------------------------------------------

cat_vars <- c("blood_bun_cat", "age_cat", "pleural_look_cat", "dev_place_cat", "blood_alb_cat", "disc_outcome", "score", "death90")
cat_factorVars <- c("blood_bun_cat", "age_cat", "pleural_look_cat", "dev_place_cat", "blood_alb_cat", "disc_outcome", "score", "death90")
table_cat <- CreateTableOne(vars = cat_vars,
                            data = df_cat,
                            includeNA = TRUE,
                            factorVars = cat_factorVars)
table_cat 


table_cat2 <- CreateTableOne(vars = cat_vars,
                            data = df_cat,
                            includeNA = TRUE,
                            strata = "death90",
                            factorVars = cat_factorVars)
table_cat2

table_cat3 <- CreateTableOne(vars = cat_vars,
                             data = df_cat,
                             includeNA = TRUE,
                             strata = "ope",
                             factorVars = cat_factorVars)
table_cat3 <- table_cat3 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

table_cat3 %>% write.csv("stable3.csv")

table_cat4 <- CreateTableOne(vars = cat_vars,
                             data = df_cat,
                             includeNA = TRUE,
                             strata = "early_ope",
                             factorVars = cat_factorVars)
table_cat4 <- table_cat4 %>% 
  print(nonnormal = c("adm_adl", "los", "follow"))

table_cat4 %>% write.csv("stable3.csv")

# Cut-off = 0
predicted_values0 <- ifelse(df_cat$score >= 0, 1, 0)
actual_values <- df_cat$death90
conf_matrix0 <- table(factor(predicted_values0, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr0 = conf_matrix0[2,2]/(conf_matrix0[2,1] + conf_matrix0[2,2])
fpr0 = conf_matrix0[2,1]/(conf_matrix0[2,1] + conf_matrix0[2,2])
x0 <- sensitivity(conf_matrix0)
y0 <- 1-specificity(conf_matrix0)
# Cut-off = 1
predicted_values1 <- ifelse(df_cat$score >= 1, 1, 0)
actual_values <- df_cat$death90
conf_matrix1 <- table(factor(predicted_values1, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr1 = conf_matrix1[2,2]/(conf_matrix1[2,1] + conf_matrix1[2,2])
fpr1 = conf_matrix1[2,1]/(conf_matrix1[2,1] + conf_matrix1[2,2])
x1 <- sensitivity(conf_matrix1)
y1 <- 1-specificity(conf_matrix1)
# Cut-off = 2
predicted_values2 <- ifelse(df_cat$score >= 2, 1, 0)
actual_values <- df_cat$death90
conf_matrix2 <- table(factor(predicted_values2, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr2 = conf_matrix2[2,2]/(conf_matrix2[2,1] + conf_matrix2[2,2])
fpr2 = conf_matrix2[2,1]/(conf_matrix2[2,1] + conf_matrix2[2,2])
x2 <- sensitivity(conf_matrix2)
y2 <- 1-specificity(conf_matrix2)
# Cut-off = 3
predicted_values3 <- ifelse(df_cat$score >= 3, 1, 0)
actual_values <- df_cat$death90
conf_matrix3 <- table(factor(predicted_values3, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr3 = conf_matrix3[2,2]/(conf_matrix3[2,1] + conf_matrix3[2,2])
fpr3 = conf_matrix3[2,1]/(conf_matrix3[2,1] + conf_matrix3[2,2])
x3 <- sensitivity(conf_matrix3)
y3 <- 1-specificity(conf_matrix3)
# Cut-off = 4
predicted_values4 <- ifelse(df_cat$score >= 4, 1, 0)
actual_values <- df_cat$death90
conf_matrix4 <- table(factor(predicted_values4, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr4 = conf_matrix4[2,2]/(conf_matrix4[2,1] + conf_matrix4[2,2])
fpr4 = conf_matrix4[2,1]/(conf_matrix4[2,1] + conf_matrix4[2,2])
x4 <- sensitivity(conf_matrix4)
y4 <- 1- specificity(conf_matrix4)
# Cut-off = 5
predicted_values5 <- ifelse(df_cat$score >= 5, 1, 0)
actual_values <- df_cat$death90
conf_matrix5 <- table(factor(predicted_values5, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr5 = conf_matrix5[2,2]/(conf_matrix5[2,1] + conf_matrix5[2,2])
fpr5 = conf_matrix5[2,1]/(conf_matrix5[2,1] + conf_matrix5[2,2])
x5 <- sensitivity(conf_matrix5)
y5 <- 1-specificity(conf_matrix5)
# Cut-off = 6
predicted_values6 <- ifelse(df_cat$score >= 6, 1, 0)
actual_values <- df_cat$death90
conf_matrix6 <- table(factor(predicted_values6, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr6 = conf_matrix6[2,2]/(conf_matrix6[2,1] + conf_matrix6[2,2])
fpr6 = conf_matrix6[2,1]/(conf_matrix6[2,1] + conf_matrix6[2,2])
x6 <- sensitivity(conf_matrix6)
y6 <- 1-specificity(conf_matrix6)
# Cut-off = 7
predicted_values7 <- ifelse(df_cat$score >= 7, 1, 0)
actual_values <- df_cat$death90
conf_matrix7 <- table(factor(predicted_values7, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr7 = conf_matrix7[2,2]/(conf_matrix7[2,1] + conf_matrix7[2,2])
fpr7 = conf_matrix7[2,1]/(conf_matrix7[2,1] + conf_matrix7[2,2])
x7 <- sensitivity(conf_matrix7)
y7 <- 1-specificity(conf_matrix7)
# Describing ROC curve
rapid <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                    Sensitivity = c(x0, x1, x2, x3, x4, x5, x6, x7),
                    Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
trapz(rapid$Specificity, rapid$Sensitivity) 

rapid <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                    Sensitivity = c(1-x0, 1-x1, 1-x2, 1-x3, 1-x4, 1-x5, 1-x6, 1-x7),
                    Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
plot(Sensitivity ~ Specificity,
     data = rapid,
     xlim = c(0,1),
     ylim = c(0,1),
     type="b",
     bg = "black") #Just for confirmation!! Look at the x-axis.


# Confidence interval -----------------------------------------------------

boot_num <- 2000
roc_list <- c()
for(i in seq_len(boot_num)){
  set.seed(i)
  bootstrap <-
    df_cat[sample(dim(df_cat)[1],dim(df_cat)[1],replace=TRUE),]  
  # Cut-off = 0
  predicted_values0 <- ifelse(bootstrap$score >= 0, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix0 <- table(factor(predicted_values0, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr0 = conf_matrix0[2,2]/(conf_matrix0[2,1] + conf_matrix0[2,2])
  fpr0 = conf_matrix0[2,1]/(conf_matrix0[2,1] + conf_matrix0[2,2])
  x0 <- sensitivity(conf_matrix0)
  y0 <- 1-specificity(conf_matrix0)
  # Cut-off = 1
  predicted_values1 <- ifelse(bootstrap$score >= 1, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix1 <- table(factor(predicted_values1, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr1 = conf_matrix1[2,2]/(conf_matrix1[2,1] + conf_matrix1[2,2])
  fpr1 = conf_matrix1[2,1]/(conf_matrix1[2,1] + conf_matrix1[2,2])
  x1 <- sensitivity(conf_matrix1)
  y1 <- 1-specificity(conf_matrix1)
  # Cut-off = 2
  predicted_values2 <- ifelse(bootstrap$score >= 2, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix2 <- table(factor(predicted_values2, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr2 = conf_matrix2[2,2]/(conf_matrix2[2,1] + conf_matrix2[2,2])
  fpr2 = conf_matrix2[2,1]/(conf_matrix2[2,1] + conf_matrix2[2,2])
  x2 <- sensitivity(conf_matrix2)
  y2 <- 1-specificity(conf_matrix2)
  # Cut-off = 3
  predicted_values3 <- ifelse(bootstrap$score >= 3, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix3 <- table(factor(predicted_values3, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr3 = conf_matrix3[2,2]/(conf_matrix3[2,1] + conf_matrix3[2,2])
  fpr3 = conf_matrix3[2,1]/(conf_matrix3[2,1] + conf_matrix3[2,2])
  x3 <- sensitivity(conf_matrix3)
  y3 <- 1-specificity(conf_matrix3)
  # Cut-off = 4
  predicted_values4 <- ifelse(bootstrap$score >= 4, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix4 <- table(factor(predicted_values4, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr4 = conf_matrix4[2,2]/(conf_matrix4[2,1] + conf_matrix4[2,2])
  fpr4 = conf_matrix4[2,1]/(conf_matrix4[2,1] + conf_matrix4[2,2])
  x4 <- sensitivity(conf_matrix4)
  y4 <- 1- specificity(conf_matrix4)
  # Cut-off = 5
  predicted_values5 <- ifelse(bootstrap$score >= 5, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix5 <- table(factor(predicted_values5, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr5 = conf_matrix5[2,2]/(conf_matrix5[2,1] + conf_matrix5[2,2])
  fpr5 = conf_matrix5[2,1]/(conf_matrix5[2,1] + conf_matrix5[2,2])
  x5 <- sensitivity(conf_matrix5)
  y5 <- 1-specificity(conf_matrix5)
  # Cut-off = 6
  predicted_values6 <- ifelse(bootstrap$score >= 6, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix6 <- table(factor(predicted_values6, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr6 = conf_matrix6[2,2]/(conf_matrix6[2,1] + conf_matrix6[2,2])
  fpr6 = conf_matrix6[2,1]/(conf_matrix6[2,1] + conf_matrix6[2,2])
  x6 <- sensitivity(conf_matrix6)
  y6 <- 1-specificity(conf_matrix6)
  # Cut-off = 7
  predicted_values7 <- ifelse(bootstrap$score >= 7, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix7 <- table(factor(predicted_values7, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr7 = conf_matrix7[2,2]/(conf_matrix7[2,1] + conf_matrix7[2,2])
  fpr7 = conf_matrix7[2,1]/(conf_matrix7[2,1] + conf_matrix7[2,2])
  x7 <- sensitivity(conf_matrix7)
  y7 <- 1-specificity(conf_matrix7)
  # AUC
  rapid <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                      Sensitivity = c(x0, x1, x2, x3, x4, x5, x6, x7),
                      Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
  roc_list <- c(roc_list, trapz(rapid$Specificity, rapid$Sensitivity)) 
}
quantile(roc_list,c(0+(1-0.95)/2, .5, 1-(1-0.95)/2))

# Modified RAPID score ----------------------------------------------------

df_cat <- df_cat %>% 
  mutate(mod_score = score + 0.5*loculation_cat + inter_pleural_effusion_cat)

fit_mod <- glm(death90 ~ score + loculation_cat + inter_pleural_effusion_cat,
              family = binomial(link = "logit"),
              data = df_cat)
tidy(fit_mod, exponentiate = FALSE, conf.int = TRUE)

# Cut-off = 0
predicted_values0 <- ifelse(df_cat$mod_score >= 0, 1, 0)
actual_values <- df_cat$death90
conf_matrix0 <- table(factor(predicted_values0, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr0 = conf_matrix0[2,2]/(conf_matrix0[2,1] + conf_matrix0[2,2])
fpr0 = conf_matrix0[2,1]/(conf_matrix0[2,1] + conf_matrix0[2,2])
x0 <- sensitivity(conf_matrix0)
y0 <- 1-specificity(conf_matrix0)
# Cut-off = 1
predicted_values1 <- ifelse(df_cat$mod_score >= 1, 1, 0)
actual_values <- df_cat$death90
conf_matrix1 <- table(factor(predicted_values1, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr1 = conf_matrix1[2,2]/(conf_matrix1[2,1] + conf_matrix1[2,2])
fpr1 = conf_matrix1[2,1]/(conf_matrix1[2,1] + conf_matrix1[2,2])
x1 <- sensitivity(conf_matrix1)
y1 <- 1-specificity(conf_matrix1)
# Cut-off = 2
predicted_values2 <- ifelse(df_cat$mod_score >= 2, 1, 0)
actual_values <- df_cat$death90
conf_matrix2 <- table(factor(predicted_values2, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr2 = conf_matrix2[2,2]/(conf_matrix2[2,1] + conf_matrix2[2,2])
fpr2 = conf_matrix2[2,1]/(conf_matrix2[2,1] + conf_matrix2[2,2])
x2 <- sensitivity(conf_matrix2)
y2 <- 1-specificity(conf_matrix2)
# Cut-off = 3
predicted_values3 <- ifelse(df_cat$mod_score >= 3, 1, 0)
actual_values <- df_cat$death90
conf_matrix3 <- table(factor(predicted_values3, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr3 = conf_matrix3[2,2]/(conf_matrix3[2,1] + conf_matrix3[2,2])
fpr3 = conf_matrix3[2,1]/(conf_matrix3[2,1] + conf_matrix3[2,2])
x3 <- sensitivity(conf_matrix3)
y3 <- 1-specificity(conf_matrix3)
# Cut-off = 4
predicted_values4 <- ifelse(df_cat$mod_score >= 4, 1, 0)
actual_values <- df_cat$death90
conf_matrix4 <- table(factor(predicted_values4, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr4 = conf_matrix4[2,2]/(conf_matrix4[2,1] + conf_matrix4[2,2])
fpr4 = conf_matrix4[2,1]/(conf_matrix4[2,1] + conf_matrix4[2,2])
x4 <- sensitivity(conf_matrix4)
y4 <- 1- specificity(conf_matrix4)
# Cut-off = 5
predicted_values5 <- ifelse(df_cat$mod_score >= 5, 1, 0)
actual_values <- df_cat$death90
conf_matrix5 <- table(factor(predicted_values5, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr5 = conf_matrix5[2,2]/(conf_matrix5[2,1] + conf_matrix5[2,2])
fpr5 = conf_matrix5[2,1]/(conf_matrix5[2,1] + conf_matrix5[2,2])
x5 <- sensitivity(conf_matrix5)
y5 <- 1-specificity(conf_matrix5)
# Cut-off = 6
predicted_values6 <- ifelse(df_cat$mod_score >= 6, 1, 0)
actual_values <- df_cat$death90
conf_matrix6 <- table(factor(predicted_values6, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr6 = conf_matrix6[2,2]/(conf_matrix6[2,1] + conf_matrix6[2,2])
fpr6 = conf_matrix6[2,1]/(conf_matrix6[2,1] + conf_matrix6[2,2])
x6 <- sensitivity(conf_matrix6)
y6 <- 1-specificity(conf_matrix6)
# Cut-off = 7
predicted_values7 <- ifelse(df_cat$mod_score >= 7, 1, 0)
actual_values <- df_cat$death90
conf_matrix7 <- table(factor(predicted_values7, levels = 0:1),
                      factor(actual_values, levels = 0:1))
tpr7 = conf_matrix7[2,2]/(conf_matrix7[2,1] + conf_matrix7[2,2])
fpr7 = conf_matrix7[2,1]/(conf_matrix7[2,1] + conf_matrix7[2,2])
x7 <- sensitivity(conf_matrix7)
y7 <- 1-specificity(conf_matrix7)
# Describing ROC curve
rapid <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                    Sensitivity = c(x0, x1, x2, x3, x4, x5, x6, x7),
                    Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
trapz(rapid$Specificity, rapid$Sensitivity) 

rapid <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                    Sensitivity = c(1-x0, 1-x1, 1-x2, 1-x3, 1-x4, 1-x5, 1-x6, 1-x7),
                    Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
plot(Sensitivity ~ Specificity,
     data = rapid,
     xlim = c(0,1),
     ylim = c(0,1),
     type="b",
     bg = "black") #Just for confirmation!! Look at the x-axis.


## Confidence interval

boot_num <- 2000
roc_list <- c()
for(i in seq_len(boot_num)){
  set.seed(i)
  bootstrap <-
    df_cat[sample(dim(df_cat)[1],dim(df_cat)[1],replace=TRUE),]  
  # Cut-off = 0
  predicted_values0 <- ifelse(bootstrap$mod_score >= 0, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix0 <- table(factor(predicted_values0, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr0 = conf_matrix0[2,2]/(conf_matrix0[2,1] + conf_matrix0[2,2])
  fpr0 = conf_matrix0[2,1]/(conf_matrix0[2,1] + conf_matrix0[2,2])
  x0 <- sensitivity(conf_matrix0)
  y0 <- 1-specificity(conf_matrix0)
  # Cut-off = 1
  predicted_values1 <- ifelse(bootstrap$mod_score >= 1, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix1 <- table(factor(predicted_values1, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr1 = conf_matrix1[2,2]/(conf_matrix1[2,1] + conf_matrix1[2,2])
  fpr1 = conf_matrix1[2,1]/(conf_matrix1[2,1] + conf_matrix1[2,2])
  x1 <- sensitivity(conf_matrix1)
  y1 <- 1-specificity(conf_matrix1)
  # Cut-off = 2
  predicted_values2 <- ifelse(bootstrap$mod_score >= 2, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix2 <- table(factor(predicted_values2, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr2 = conf_matrix2[2,2]/(conf_matrix2[2,1] + conf_matrix2[2,2])
  fpr2 = conf_matrix2[2,1]/(conf_matrix2[2,1] + conf_matrix2[2,2])
  x2 <- sensitivity(conf_matrix2)
  y2 <- 1-specificity(conf_matrix2)
  # Cut-off = 3
  predicted_values3 <- ifelse(bootstrap$mod_score >= 3, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix3 <- table(factor(predicted_values3, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr3 = conf_matrix3[2,2]/(conf_matrix3[2,1] + conf_matrix3[2,2])
  fpr3 = conf_matrix3[2,1]/(conf_matrix3[2,1] + conf_matrix3[2,2])
  x3 <- sensitivity(conf_matrix3)
  y3 <- 1-specificity(conf_matrix3)
  # Cut-off = 4
  predicted_values4 <- ifelse(bootstrap$mod_score >= 4, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix4 <- table(factor(predicted_values4, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr4 = conf_matrix4[2,2]/(conf_matrix4[2,1] + conf_matrix4[2,2])
  fpr4 = conf_matrix4[2,1]/(conf_matrix4[2,1] + conf_matrix4[2,2])
  x4 <- sensitivity(conf_matrix4)
  y4 <- 1- specificity(conf_matrix4)
  # Cut-off = 5
  predicted_values5 <- ifelse(bootstrap$mod_score >= 5, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix5 <- table(factor(predicted_values5, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr5 = conf_matrix5[2,2]/(conf_matrix5[2,1] + conf_matrix5[2,2])
  fpr5 = conf_matrix5[2,1]/(conf_matrix5[2,1] + conf_matrix5[2,2])
  x5 <- sensitivity(conf_matrix5)
  y5 <- 1-specificity(conf_matrix5)
  # Cut-off = 6
  predicted_values6 <- ifelse(bootstrap$mod_score >= 6, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix6 <- table(factor(predicted_values6, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr6 = conf_matrix6[2,2]/(conf_matrix6[2,1] + conf_matrix6[2,2])
  fpr6 = conf_matrix6[2,1]/(conf_matrix6[2,1] + conf_matrix6[2,2])
  x6 <- sensitivity(conf_matrix6)
  y6 <- 1-specificity(conf_matrix6)
  # Cut-off = 7
  predicted_values7 <- ifelse(bootstrap$mod_score >= 7, 1, 0)
  actual_values <- bootstrap$death90
  conf_matrix7 <- table(factor(predicted_values7, levels = 0:1),
                        factor(actual_values, levels = 0:1))
  tpr7 = conf_matrix7[2,2]/(conf_matrix7[2,1] + conf_matrix7[2,2])
  fpr7 = conf_matrix7[2,1]/(conf_matrix7[2,1] + conf_matrix7[2,2])
  x7 <- sensitivity(conf_matrix7)
  y7 <- 1-specificity(conf_matrix7)
  # AUC
  rapid <- data.frame(cutpoint = c(0, 1, 2, 3, 4, 5, 6, 7),
                      Sensitivity = c(x0, x1, x2, x3, x4, x5, x6, x7),
                      Specificity = c(y0, y1, y2, y3, y4, y5, y6, y7))
  roc_list <- c(roc_list, trapz(rapid$Specificity, rapid$Sensitivity)) 
}
quantile(roc_list,c(0+(1-0.95)/2, .5, 1-(1-0.95)/2))


# ROS curve ---------------------------------------------------------------

plot(1-Sensitivity ~ Specificity,
     data = rapid,
     xlim = c(0,1),
     xlab = "1-Specificity",
     ylim = c(0,1),
     ylab = "Sensitivity",
     type="b",
     pch = 16,
     bg = "black")


# Interobserver variability ------------------------------------------------

library(fmsb)
int_var <- read_csv("interobserver_var.csv")

int_var %>% glimpse()
int_var <- int_var %>% 
  mutate(thickness1 = if_else(thickness1 > 2, 1, 0),
         thickness2 = if_else(thickness2 > 2, 1, 0),
         loculation2 = if_else(loculation2 > 2, 1, 0),
         inter_pleural_effusion2 = if_else(inter_pleural_effusion2 > 2, 1, 0)) %>% 
  drop_na()
Kappa.test(int_var$thickness1, int_var$thickness2)   
Kappa.test(int_var$loculation1, int_var$loculation2)
Kappa.test(int_var$inter_pleural_effusion1, int_var$inter_pleural_effusion2)
Kappa.test(int_var$lung_abscess1, int_var$lung_abscess2)