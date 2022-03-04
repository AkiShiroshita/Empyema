
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

uniter <- function(data){
  dpc_sub <<- data %>% 
    unite(id, contains("データ識別番号"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(sex, contains("性別"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(age, contains("年齢"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(adm_date, contains("入院日"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(disc_date, contains("退院日"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(los, contains("在院日数"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(death, contains("死亡の有無"),
          sep = "_",
          remove = FALSE,
          na.rm = TRUE) %>% 
    unite(k, contains("Kコード"),
          sep = "_",
          remove = FALSE,
          na.rm = TRUE) %>% 
    unite(opek, contains("手術Kコード"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(openm, contains("手術名称"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(adm_style, contains("予定・救急医療入院"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>%
    unite(adm_ambul, contains("救急車による搬送の有無"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(disc_to, contains("退院先"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(disc_outcome, contains("退院時転帰"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(death_24h, contains("24 時間以内の死亡の有無"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(height, contains("身長"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(weight, contains("体重"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(smokingidx, contains("喫煙指数"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(dmainnm, contains("主傷病名"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(icd_all, contains("ICD10 コード"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(dresnm, contains("医療資源を最も投入した傷病名"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(dcom1nm, contains("入院時併存症名1"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(dcom2nm, contains("入院時併存症名2"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(dcom3nm, contains("入院時併存症名3"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(dcom4nm, contains("入院時併存症名4"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(adm_before, contains("前回同一傷病で自院入院の有無"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(opedt, contains("手術日"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(adm_adl, contains("入院時のADLスコア"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>% 
    unite(disc_adl, contains("退院時のADLスコア"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>%
    unite(adm_jcs, contains("入院時意識障害がある場合のJCS"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>%
    unite(disc_jcs, contains("退院時意識障害がある場合のJCS"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>%
    unite(hughjones, contains("Hugh-Jones 分類"),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE) %>%
    select(id,
           sex,
           age,
           adm_date,
           disc_date,
           los,
           death,
           k,
           opek,
           openm,
           adm_style,
           adm_ambul,
           disc_to,
           disc_outcome,
           death_24h,
           height,
           weight,
           smokingidx,
           dmainnm,
           icd_all,
           dresnm,
           dcom1nm,
           dcom2nm,
           dcom3nm,
           dcom4nm,
           adm_before,
           opedt,
           adm_adl,
           disc_adl,
           adm_jcs,
           disc_jcs,
           hughjones) %>% 
    mutate_all(.funs = ~ as.character(.))
  dpc <<- bind_rows(dpc, dpc_sub)
}


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
           into = c("sex", "non_sex"),
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
  select(-starts_with("non")) %>% 
  separate(icd_all,
           into = c("dmain", "dadm", "dres", "dcom1", "dcom2", "dcom3", "dcom4", "dcom5", "dcom6", "dcom7", "dcom8", "dcom9"),
           sep = "_",
           remove = "FALSE")
# Please note we cannot distinguish "ddev" from "dcom"

complete <- left_join(key, dpc, by=c("id", "adm_date"))

screen <- str_c(dpc$id, collapse = "|") # for selecting variables

# Procedures --------------------------------------------------------------

## abx 2019
abx_2019 <- act_2019 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm_date = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm_date, day) %>%  
  select(id, adm_date, day, name) 

## abx 2020
abx_2020 <- act_2020 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm_date = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm_date, day) %>%  
  select(id, adm_date, day, name) 

## abx 2021
abx_2021 <- act_2021 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm_date = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm_date, day) %>%  
  select(id, adm_date, day, name)

## Combine the datasets
abx <- bind_rows(abx_2019, abx_2020)
abx <- bind_rows(abx, abx_2021)
abx <- abx %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate(id = str_sub(abx$id, start = 5)) %>% 
  filter(str_detect(id, screen))

abx_start <- abx %>%  
  arrange(id, adm_date, day) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(abx = name,
         abx_start = day)

abx_end <- abx %>%  
  arrange(id, adm_date, desc(day)) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(abx = name,
         abx_end = day)

abx <- left_join(abx_start, abx_end, by = c("id", "adm_date", "abx")) %>% 
  select(id, adm_date, abx, abx_start, abx_end) 

abx <- abx %>% 
  group_by(id, adm_date) %>% 
  summarise(abx_all_names = paste0(abx,
                                        collapse = "_"),
            abx_all_day = paste0(abx_start,
                                      abx_end,
                                      collapse = "_")) %>% 
  ungroup(id, adm_date) 

complete <- left_join(complete, abx, by=c("id", "adm_date"))

#abx$day <- str_replace_all(abx$day, pattern = "-", replacement="")
abx %>% write.csv("data/ichinomiya/cleaned/abx.csv")

# drainage 2019
# J019:持続的胸腔ドレナージ,J008:胸腔穿刺, J002：ドレーン法  

drainage_2019 <- act_2019 %>% 
  rename(id = "データ識別番号",
         adm_date = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm_date, day) %>%  
  select(id, adm_date, day, name)

# drainage 2020
drainage_2020 <- act_2020 %>% 
  rename(id = "データ識別番号",
         adm_date = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm_date, day) %>%  
  select(id, adm_date, day, name) 

# drainage 2021
drainage_2021 <- act_2021 %>% 
  rename(id = "データ識別番号",
         adm_date = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm_date, day) %>%  
  select(id, adm_date, day, name) 

## Combine the datasets
drainage <- bind_rows(drainage_2019, drainage_2020)
drainage <- bind_rows(drainage, drainage_2021)

drainage <- drainage %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate(id = str_sub(drainage$id, start = 5)) %>% 
  filter(str_detect(id, screen))

drainage_start <- drainage %>%  
  arrange(id, adm_date, day) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(drainage = name,
         drainage_start = day)

drainage_end <- drainage %>%  
  arrange(id, adm_date, desc(day)) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(drainage = name,
         drainage_end = day)

drainage <- left_join(drainage_start, drainage_end, by = c("id", "adm_date", "drainage")) %>% 
  select(id, adm_date, drainage, drainage_start, drainage_end) 

drainage <- drainage %>% 
  group_by(id, adm_date) %>% 
  summarise(drainage_all_names = paste0(drainage,
                                   collapse = "_"),
            drainage_all_day = paste0(drainage_start,
                                 drainage_end,
                                 collapse = "_")) %>% 
  ungroup(id, adm_date) 

complete <- left_join(complete, drainage, by=c("id", "adm_date"))

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
  lab_combine <<- left_join(lab_combine, lab, key = "id")
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
         blood_wbc2 = as.character(as.numeric(blood_wbc2)*100)) %>% 
  select(-diag_date) %>% 
  distinct(id, adm_date, .keep_all = TRUE)

complete <- left_join(complete, lab_combine, by=c("id", "adm_date"))

lab_combine %>% write.csv("data/ichinomiya/cleaned/lab.csv")
complete %>% write.csv("data/ichinomiya/cleaned/ichinomiya.csv")
