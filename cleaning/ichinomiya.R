
# Data import -------------------------------------------------------------

dpc_2019 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/dpc_2019.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
dpc_2020 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/dpc_2020.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
dpc_2021 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/dpc_2021.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
act_2019 <- read_tsv("data/ichinomiya/uncleaned/ichinomiya/download_acts_2018.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
act_2020 <- read_tsv("data/ichinomiya/uncleaned/ichinomiya/download_acts_2019.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
act_2021 <- read_tsv("data/ichinomiya/uncleaned/ichinomiya/download_acts_2020.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
cases_2019 <- read_tsv("data/ichinomiya/uncleaned/ichinomiya/download_cases_2018.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
cases_2020 <- read_tsv("data/ichinomiya/uncleaned/ichinomiya/download_cases_2019.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
cases_2021 <- read_tsv("data/ichinomiya/uncleaned/ichinomiya/download_cases_2020.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
culture2019 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/culture2019.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
culture2020 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/culture2020.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
culture2021 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/culture2021.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
lab_2019 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/lab_data/ichinomiya_lab/2019.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
lab_2020 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/lab_data/ichinomiya_lab/2020.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
lab_2021 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/lab_data/ichinomiya_lab/2021.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
chart <- read_csv("data/ichinomiya/cleaned/chart_review/ichinomiya_chart.csv", 
                  locale = locale(encoding = "SHIFT-JIS"),
                  na = "")

# Chart -------------------------------------------------------------------

chart <- read_csv("data/ichinomiya/cleaned/chart_review/ichinomiya_chart.csv",
                 locale = locale(encoding = "SHIFT-JIS"))
chart <- chart %>% 
  mutate_all(.funs = ~ as.character(.)) %>%  
  filter(str_detect(empyema_or_not, "1")) %>% 
  arrange(id, adm_date)

key <- chart %>% 
  select(id, adm_date, diag_date)

# DPC ---------------------------------------------------------------------

dpc_2019 %>% dim() # not suitable
dpc_2020 %>% dim() # not suitable
dpc_2021 %>% dim() # not suitable 
cases_2019 %>% dim()
cases_2020 %>% dim()
cases_2021 %>% dim()

dpc <- c()
uniter(cases_2019)
uniter(cases_2020)
uniter(cases_2021)

dpc %>% glimpse()
dpc %>% colnames()

dpc <- dpc %>%
  separate(id,
           into = c("id", "non_id"),
           sep = "_") %>% 
  mutate(id = str_sub(id, start = 5)) %>% 
  separate(icd_all,
           into = c("dmain", "non_demain"),
           sep = "_",
           remove = "FALSE") %>% 
  separate(sex,
           into = c("sex", "non-sex"),
           sep = "_") %>% 
  separate(age,
           into = c("age", "non_age"),
           sep = "_") %>% 
  separate(adm_date,
           into = c("adm_date", "non_adm_date"),
           sep = "_") %>% 
  separate(los,
           into = c("los", "non_los"),
           sep = "_") %>% 
  separate(death,
           into = c("death", "non_death"),
           sep = "_") %>% 
  distinct(id, adm_date, .keep_all=TRUE)
dpc$adm_adl <- sapply(strsplit(dpc$adm_adl,""), function(x) sum(as.numeric(x))) 
dpc$disc_adl <- sapply(strsplit(dpc$disc_adl,""), function(x) sum(as.numeric(x))) 

dpc <- dpc %>% 
  filter(str_detect(dmain, "J86"))

dpc <- left_join(chart, dpc, by=c("id", "adm_date"))
dpc %>% glimpse()

dpc <- dpc %>% 
  select(-starts_with("non")) %>% 
  separate(icd_all,
           into = c("dmain", "dadm", "dres", "dcom1", "dcom2", "dcom3", "dcom4", "dcom5", "dcom6", "dcom7", "dcom8", "dcom9"),
           sep = "_",
           remove = "FALSE")
# Please note we cannot distinguish "ddev" from "dcom"
dpc %>% write.csv("data/ichinomiya/cleaned/dpc.csv")

screen <- str_c(dpc$id, collapse = "|") # for selecting variables

# Procedures --------------------------------------------------------------

## abx 2019
abx_2019 <- act_2019 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## abx 2020
abx_2020 <- act_2020 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## abx 2021
abx_2021 <- act_2021 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## Combine the datasets
abx <- bind_rows(abx_2019, abx_2020)
abx <- bind_rows(abx, abx_2021)
abx <- abx %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  arrange(id, adm, day) %>% 
  distinct(id, adm, name, .keep_all=TRUE)
abx$id <- str_sub(abx$id, start = 5)
abx <- abx %>% 
  filter(str_detect(id, screen))
abx$day <- str_replace_all(abx$day, pattern = "-", replacement="")
abx %>% write.csv("data/ichinomiya/cleaned/abx.csv")

# drainage 2019
# J019:持続的胸腔ドレナージ,J008:胸腔穿刺, J002：ドレーン法  

drainage_2019 <- act_2019 %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

# drainage 2020
drainage_2020 <- act_2020 %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

# drainage 2021
drainage_2021 <- act_2021 %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## Combine the datasets
drainage <- bind_rows(drainage_2019, drainage_2020)
drainage <- bind_rows(drainage, drainage_2021)
drainage <- drainage %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  arrange(id, adm, day) %>% 
  distinct(id, adm, name, .keep_all=TRUE)
drainage$id <- str_sub(drainage$id, start = 5)
drainage <- drainage %>% 
  filter(str_detect(id, screen))
drainage$day <- str_replace_all(drainage$day, pattern = "-", replacement="")
drainage %>% write.csv("data/ichinomiya/cleaned/drainage.csv")

# Culture -----------------------------------------------------------------

## culture 2019
culture2019 %>% colnames()
culture2019 <- culture2019 %>% 
  rename(id = "...2",
         day = "...3",
         organ = "材料名称...203") %>%
  filter(!is.na(organ)) %>% 
  select_if(~sum(!is.na(.)) > 0) 
culture2019 %>% dim()
culture2019 <- culture2019 %>% 
  select(1:385, -1, -(4:57))
culture2019$id <- str_sub(culture2019$id, start = 5)
culture2019 <- culture2019 %>% 
  filter(str_detect(id, screen)) %>% 
  filter(organ == "胸水")
culture2019$day <- str_replace_all(culture2019$day, pattern = "-", replacement="")

## culture 2020
culture2020 %>% colnames()
culture2020 <- culture2020 %>% 
  rename(id = "...2",
         day = "...3",
         organ = "材料名称...203") %>%
  filter(!is.na(organ)) %>% 
  select_if(~sum(!is.na(.)) > 0) 
culture2020 %>% dim()
culture2020 <- culture2020 %>% 
  select(1:367, -1, -(4:54))
culture2020$id <- str_sub(culture2020$id, start = 5)
culture2020 <- culture2020 %>% 
  filter(str_detect(id, screen)) %>% 
  filter(organ == "胸水")
culture2020$day <- str_replace_all(culture2020$day, pattern = "-", replacement="")

## culture 2021
culture2021 %>% colnames()
culture2021 <- culture2021 %>% 
  rename(id = "...2",
         day = "...3",
         organ = "材料名称...203") %>%
  filter(!is.na(organ)) %>% 
  select_if(~sum(!is.na(.)) > 0) 
culture2021 %>% dim()
culture2021 <- culture2021 %>% 
  select(1:378, -1, -(4:56))
culture2021$id <- str_sub(culture2021$id, start = 5)
culture2021 <- culture2021 %>% 
  filter(str_detect(id, screen)) %>% 
  filter(organ == "胸水")
culture2021$day <- str_replace_all(culture2021$day, pattern = "-", replacement="")

## output
culture2019 %>% write.csv("data/ichinomiya/cleaned/culture1.csv")
culture2020 %>% write.csv("data/ichinomiya/cleaned/culture2.csv")
culture2021 %>% write.csv("data/ichinomiya/cleaned/culture3.csv")

# Lab ---------------------------------------------------------------------

## lab 2019
lab_2019 %>% colnames()
lab_2019 <- lab_2019 %>% 
  rename(id = "...2",
         date = "...3",
         name = "検査名称",
         value = "結果") %>%
  select(id, date, name, value)
lab_2019 <- lab_2019 %>% 
  mutate(date = date((ymd_hms(date))))

## lab 2020
lab_2020 %>% colnames()
lab_2020 <- lab_2020 %>% 
  rename(id = "...2",
         date = "...3",
         name = "検査名称",
         value = "結果") %>%
  select(id, date, name, value)
lab_2020 <- lab_2020 %>% 
  mutate(date = date((ymd_hms(date))))

## lab 2021
lab_2021 %>% colnames()
lab_2021 <- lab_2021 %>% 
  rename(id = "...2",
         date = "...3",
         name = "検査名称",
         value = "結果") %>%
  select(id, date, name, value)
lab_2021 <- lab_2021 %>% 
  mutate(date = date((ymd_hms(date))))

## Combine
lab <- bind_rows(lab_2019, lab_2020)
lab <- bind_rows(lab, lab_2021)
lab <- lab %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  arrange(id, desc(date)) 
lab$id <- str_sub(lab$id, start = 5)
lab <- lab %>% 
  filter(str_detect(id, screen)) 
lab <- lab %>% 
  mutate(date = ymd(date))

lab_key <- chart %>% 
  select(id, adm_date, diag_date)

# closest date before or after, but no more than 2 days prior to index

lab_combine <- key

select_lab <- function(lab_name, new_name){
  lab <- lab %>% 
    filter(name == lab_name)
  index1 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     lab$date,
                     best="prior")
  index1 <- ifelse((ymd(lab_key$diag_date) - ymd(lab$date[index1])) > 2, NA, index1)
  index2 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     lab$date)
  index2 <- ifelse((ymd(lab_key$diag_date) - ymd(lab$date[index2])) > 2, NA, index2)
  index3 <- ifelse(is.na(index1), index2, # none before, take after
                   ifelse(is.na(index2), index1, #none after
                          ifelse(abs(ymd(lab$date[index2]) - ymd(lab_key$diag_date)) <
                                   abs(ymd(lab$date[index1])- ymd(lab_key$diag_date)), index2, index1)))
  lab <- lab[index3, ]
  lab <- lab %>% filter(!is.na(value)) %>% 
    distinct(id, date, .keep_all=TRUE) %>%  
    select(-name, -date)
  names(lab)[which(names(lab)=="value" ) ] <- new_name
  lab_combine <<- left_join(lab_combine, lab, key = c("id", "adm"))
}

unique(lab$name)

select_lab("WBC(JCCLS)", "blood_wbc")
select_lab("ＷＢＣ", "blood_wbc2")
select_lab("Ｎｅｕｔ", "blood_neutro")
select_lab("Ｈｂ", "blood_hb")
select_lab("総タンパク", "blood_tp")
select_lab("Ａｌｂ", "blood_alb")
select_lab("ＬＤＨ", "blood_ldh")
select_lab("ＢＵＮ", "blood_bun")
select_lab("ＣＲＥ", "blood_cre")
select_lab("ＣＲＰ", "blood_crp")
select_lab("ＡＬＰ" ,"blood_alp")
select_lab("Ｇｌｕ", "blood_glucose")
select_lab("尿PH", "pleural_pH")
select_lab("穿ＬＤＨ", "pleural_ldh")
select_lab("穿刺蛋白", "pleural_tp")
select_lab("穿刺　糖", "pleural_glucose")
select_lab("ｾﾝｼ ｺｳﾁｭ", "pleural_neutro")
select_lab("穿細胞数", "pleural_cell")
select_lab("ｾﾝｼ ﾘﾝﾊﾟ", "pleural_lymph")


lab_combine %>% glimpse()

lab_combine <- lab_combine %>% 
  mutate(blood_wbc = as.character(as.numeric(blood_wbc)*1000),
         blood_wbc2 = as.character(as.numeric(blood_wbc2)*100)) 

lab_combine %>% write.csv("data/ichinomiya/cleaned/lab.csv")
