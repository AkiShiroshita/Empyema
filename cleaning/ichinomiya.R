
# Data import -------------------------------------------------------------

dpc_2019 <- read_csv("data/ichinomiya/uncleaned/ichinomiya/dpc_2019.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
dpc_2020 <- read_csv("data/ichinomiya/dpc_2020.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
dpc_2021 <- read_csv("data/ichinomiya/dpc_2021.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
act_2019 <- read_tsv("data/ichinomiya/download_acts_2018.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
act_2020 <- read_tsv("data/ichinomiya/download_acts_2019.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
act_2021 <- read_tsv("data/ichinomiya/download_acts_2020.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
cases_2019 <- read_tsv("data/ichinomiya/download_cases_2018.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
cases_2020 <- read_tsv("data/ichinomiya/download_cases_2019.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
cases_2021 <- read_tsv("data/ichinomiya/download_cases_2020.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
culture2019 <- read_csv("data/ichinomiya/culture2019.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
culture2020 <- read_csv("data/ichinomiya/culture2020.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
culture2021 <- read_csv("data/ichinomiya/culture2021.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
lab_2019 <- read_csv("data/ichinomiya/lab_data/ichinomiya_lab/2019.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
lab_2020 <- read_csv("data/ichinomiya/lab_data/ichinomiya_lab/2020.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
lab_2021 <- read_csv("data/ichinomiya/lab_data/ichinomiya_lab/2021.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
chart <- read_csv("cleaned_data/chart_review/ichinomiya.csv", 
                  locale = locale(encoding = "SHIFT-JIS"),
                  na = "")

# DPC ---------------------------------------------------------------------

dpc_2019 %>% dim() # not suitable
dpc_2020 %>% dim() # not suitable
dpc_2021 %>% dim() # not suitable 
cases_2019 %>% dim()
cases_2020 %>% dim()
cases_2021 %>% dim()

## DPC2019
cases_2019 <- cases_2019 %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  select_if(~sum(!is.na(.)) > 0)
cases_2019 %>% colnames() %>% 
  as.data.frame() %>% 
  write.csv("memo/dpc2019.csv",
            fileEncoding = "SHIFT-JIS") # edit in EXCEL
memo_2019 <- read_csv("memo/dpc2019_after.csv",
                      locale = locale(encoding = "SHIFT-JIS"))
num <- memo_2019 %>% 
  pull(1)
old <- memo_2019 %>% 
  pull(2)
new <- memo_2019 %>% 
  pull(5)
cases_2019 <- cases_2019 %>% 
  select(num)
cases_2019 <- cases_2019 %>% 
  rename_at(vars(old), ~ new)
cases_2019 <- cases_2019 %>% 
  unite(opek, starts_with("opek"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(route, starts_with("route"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(refer, starts_with("refer"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(clinic, starts_with("clinic"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(adm_style, starts_with("adm_style"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(adm_ambul, starts_with("adm_ambul"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_to, starts_with("disc_to"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_outcome, starts_with("disc_outcome"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(death_24h, starts_with("death_24h"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_before, starts_with("adm_before"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(height, starts_with("height"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(weight, starts_with("weight"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(smokingidx, starts_with("smokingidx"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(dmain, starts_with("dmain"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dadm, starts_with("dadm"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dres, starts_with("dres"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom1, starts_with("dcom1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom2, starts_with("dcom2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom3, starts_with("dcom3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom4, starts_with("dcom4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev1, starts_with("ddev1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev2, starts_with("ddev2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev3, starts_with("ddev3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev4, starts_with("ddev4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(opedt, starts_with("opedt"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_adl, starts_with("adm_adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(disc_adl, starts_with("disc_adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_jcs, starts_with("adm_jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_jcs, starts_with("disc_jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(hughjones, starts_with("hughjones"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE)
#cases_2019 <- cases_2019 %>% 
#  select(-matches("\\d+$"))
cases_2019 %>% write.csv("cleaned_data/ichinomiya/dpc_2019.csv")

# another way
# col_names_2019 <- cases_2019 %>%
#  names() %>% 
#  dput()

## DPC2020
cases_2020 <- cases_2020 %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  select_if(~sum(!is.na(.)) > 0)
cases_2020 %>% colnames() %>% 
  as.data.frame() %>% 
  write.csv("memo/dpc2020.csv",
            fileEncoding = "SHIFT-JIS") # edit in EXCEL
memo_2020 <- read_csv("memo/dpc2020_after.csv",
                      locale = locale(encoding = "SHIFT-JIS"))
num <- memo_2020 %>% 
  pull(1)
old <- memo_2020 %>% 
  pull(2)
new <- memo_2020 %>% 
  pull(5)
cases_2020 <- cases_2020 %>% 
  select(num)
cases_2020 <- cases_2020 %>% 
  rename_at(vars(old), ~ new)
cases_2020 <- cases_2020 %>%
  unite(id, starts_with("id"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(sex, starts_with("sex"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(opek, starts_with("opek"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_date, starts_with("adm_date"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(disc_date, starts_with("disc_date"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(route, starts_with("route"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(refer, starts_with("refer"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(clinic, starts_with("clinic"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(adm_style, starts_with("adm_style"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(adm_ambul, starts_with("adm_ambul"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_to, starts_with("disc_to"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_outcome, starts_with("disc_outcome"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(death_24h, starts_with("death_24h"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_before, starts_with("adm_before"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(height, starts_with("height"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(weight, starts_with("weight"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(smokingidx, starts_with("smokingidx"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(dmain, starts_with("dmain"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dadm, starts_with("dadm"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dres, starts_with("dres"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom1, starts_with("dcom1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom2, starts_with("dcom2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom3, starts_with("dcom3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom4, starts_with("dcom4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev1, starts_with("ddev1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev2, starts_with("ddev2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev3, starts_with("ddev3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev4, starts_with("ddev4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(opedt, starts_with("opedt"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_adl, starts_with("adm_adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(disc_adl, starts_with("disc_adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_jcs, starts_with("adm_jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_jcs, starts_with("disc_jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(hughjones, starts_with("hughjones"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE)
#cases_2020 <- cases_2020 %>% 
#  select(-matches("\\d+$"))
cases_2020 %>% write.csv("cleaned_data/ichinomiya/dpc_2020.csv")

## DPC2021
cases_2021 <- cases_2021 %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  select_if(~sum(!is.na(.)) > 0)
cases_2021 %>% colnames() %>% 
  as.data.frame() %>% 
  write.csv("memo/dpc2021.csv",
            fileEncoding = "SHIFT-JIS") # edit in EXCEL
memo_2021 <- read_csv("memo/dpc2021_after.csv",
                      locale = locale(encoding = "SHIFT-JIS"))
num <- memo_2021 %>% 
  pull(1)
old <- memo_2021 %>% 
  pull(2)
new <- memo_2021 %>% 
  pull(5)
cases_2021 <- cases_2021 %>% 
  select(num)
cases_2021 <- cases_2021 %>% 
  rename_at(vars(old), ~ new)
cases_2021 <- cases_2021 %>% 
  unite(id, starts_with("id"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(sex, starts_with("sex"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(opek, starts_with("opek"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_date, starts_with("adm_date"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(disc_date, starts_with("disc_date"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(route, starts_with("route"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(refer, starts_with("refer"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(clinic, starts_with("clinic"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(adm_style, starts_with("adm_style"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(adm_ambul, starts_with("adm_ambul"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_to, starts_with("disc_to"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_outcome, starts_with("disc_outcome"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(death_24h, starts_with("death_24h"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_before, starts_with("adm_before"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(height, starts_with("height"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(weight, starts_with("weight"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(smokingidx, starts_with("smokingidx"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(dmain, starts_with("dmain"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dadm, starts_with("dadm"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dres, starts_with("dres"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom1, starts_with("dcom1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom2, starts_with("dcom2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom3, starts_with("dcom3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(dcom4, starts_with("dcom4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev1, starts_with("ddev1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev2, starts_with("ddev2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev3, starts_with("ddev3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ddev4, starts_with("ddev4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(opedt, starts_with("opedt"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_adl, starts_with("adm_adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(disc_adl, starts_with("disc_adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adm_jcs, starts_with("adm_jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(disc_jcs, starts_with("disc_jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(hughjones, starts_with("hughjones"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE)
#cases_2021 <- cases_2021 %>% 
# select(-matches("\\d+$"))
cases_2021 %>% write.csv("cleaned_data/ichinomiya/dpc_2021.csv")

## Combine the datasets
cases_2019 %>% colnames() %>% length()
cases_2020 %>% colnames() %>% length()
cases_2021 %>% colnames() %>% length()
#dpc <- bind_rows(cases_2019, cases_2020)
#dpc <- bind_rows(dpc, cases_2021)
#dpc %>% glimpse()
#dpc %>% colnames()
dpc <- read_csv("cleaned_data/ichinomiya/ichinomiya_complete_sub1.csv",
                locale = locale(encoding = "SHIFT-JIS"))
dpc <- dpc %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  distinct(id, adm_date, .keep_all=TRUE) %>% 
  filter(str_detect(dmain, "J86")) %>% 
  arrange(id, adm_date)
dpc$adm_adl <- sapply(strsplit(dpc$adm_adl,""), function(x) sum(as.numeric(x))) 
dpc$disc_adl <- sapply(strsplit(dpc$disc_adl,""), function(x) sum(as.numeric(x))) 

dpc2 <- read_csv("cleaned_data/chart_review/ichinomiya_chart.csv",
                 locale = locale(encoding = "SHIFT-JIS"))
dpc2 <- dpc2 %>% 
  mutate_all(.funs = ~ as.character(.)) %>%  
  filter(str_detect(empyema_or_not, "1")) %>% 
  arrange(id, adm_date)

dpc <- left_join(dpc2, dpc, by=c("id", "adm_date"))
dpc %>% glimpse()

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
abx %>% write.csv("cleaned_data/ichinomiya/abx.csv")

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
drainage %>% write.csv("cleaned_data/ichinomiya/drainage.csv")

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
culture2019 %>% write.csv("cleaned_data/ichinomiya/culture2019.csv")

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
culture2020 %>% write.csv("cleaned_data/ichinomiya/culture2020.csv")

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
culture2021 %>% write.csv("cleaned_data/ichinomiya/culture2021.csv")

# Lab ---------------------------------------------------------------------

## lab 2019
lab_2019 %>% colnames()
lab_2019 <- lab_2019 %>% 
  rename(id = "...2",
         day = "...3",
         name = "検査名称",
         result = "結果") %>%
  select(id, day, name, result)
lab_2019 <- lab_2019 %>% 
  mutate(day = date((ymd_hms(day))))

## lab 2020
lab_2020 %>% colnames()
lab_2020 <- lab_2020 %>% 
  rename(id = "...2",
         day = "...3",
         name = "検査名称",
         result = "結果") %>%
  select(id, day, name, result)
lab_2020 <- lab_2020 %>% 
  mutate(day = date((ymd_hms(day))))

## lab 2021
lab_2021 %>% colnames()
lab_2021 <- lab_2021 %>% 
  rename(id = "...2",
         day = "...3",
         name = "検査名称",
         result = "結果") %>%
  select(id, day, name, result)
lab_2021 <- lab_2021 %>% 
  mutate(day = date((ymd_hms(day))))

## Combine
lab <- bind_rows(lab_2019, lab_2020)
lab <- bind_rows(lab, lab_2021)
lab <- lab %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  arrange(id, desc(day)) 
lab$id <- str_sub(lab$id, start = 5)
lab <- lab %>% 
  filter(str_detect(id, screen)) 
lab <- lab %>% 
  mutate(day = ymd(day))
lab %>% write.csv("cleaned_data/ichinomiya/lab.csv")

# each lab result
lab$name %>% unique()
lab_label <- c("blood_wbc",
               "blood_neutro",
               "blood_hb",
               "blood_tp",
               "blood_alb",
               "blood_ldh",
               "blood_bun",
               "blood_cre",
               "blood_crp",
               "pleural_ldh",
               "pleural_tp",
               "pleural_glucose"
)
blood_wbc <- lab %>% 
  filter(name == "WBC(JCCLS)"| name == "ＷＢＣ")
blood_neutro <- lab %>% 
  filter(name == "Ｎｅｕｔ")
blood_hb <- lab %>% 
  filter(name == "Ｈｂ")
blood_tp <- lab %>% 
  filter(name == "総タンパク")
blood_alb <- lab %>% 
  filter(name == "Ａｌｂ")
blood_ldh <- lab %>% 
  filter(name == "ＬＤＨ")
blood_bun <- lab %>% 
  filter(name == "ＢＵＮ")
blood_cre <- lab %>% 
  filter(name == "ＣＲＥ")
blood_crp <- lab %>% 
  filter(name == "ＣＲＰ")
#pleural_pH <- lab %>% 
#  filter(name == "尿PH") #caution
pleural_ldh <- lab %>% 
  filter(name == "穿ＬＤＨ")
pleural_tp <- lab %>% 
  filter(name == "穿刺蛋白")
#pleural_alb<- lab %>% 
#  filter(name == "XX")
pleural_glucose <- lab %>% 
  filter(name == "穿刺　糖")

# Combing -----------------------------------------------------------------

abx <- read_csv("data/ichinomiya/cleaned/ichinomiya/abx.csv", 
                locale = locale(encoding = "SHIFT-JIS"))
culture2019 <- read_csv("data/ichinomiya/cleaned/ichinomiya/culture2019.csv", 
                        locale = locale(encoding = "SHIFT-JIS"))
culture2020 <- read_csv("data/ichinomiya/cleaned/ichinomiya/culture2020.csv", 
                        locale = locale(encoding = "SHIFT-JIS"))
culture2021 <- read_csv("data/ichinomiya/cleaned/ichinomiya/culture2021.csv", 
                        locale = locale(encoding = "SHIFT-JIS"))

dpc <- read_csv("data/ichinomiya/cleaned/ichinomiya/ichinomiya_complete_sub1.csv", 
                locale = locale(encoding = "SHIFT-JIS"))
dpc <- dpc %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  distinct(id, adm_date, .keep_all=TRUE) %>% 
  filter(str_detect(dmain, "J86")) %>% 
  arrange(id, adm_date)
dpc$adm_adl <- sapply(strsplit(dpc$adm_adl,""), function(x) sum(as.numeric(x))) 
dpc$disc_adl <- sapply(strsplit(dpc$disc_adl,""), function(x) sum(as.numeric(x))) 

dpc2 <- read_csv("data/ichinomiya/cleaned/chart_review/ichinomiya_chart.csv",
                          locale = locale(encoding = "SHIFT-JIS"))
dpc2 <- dpc2 %>% 
  mutate_all(.funs = ~ as.character(.)) %>%  
  filter(str_detect(empyema_or_not, "1")) %>% 
  arrange(id, adm_date)

dpc <- left_join(dpc2, dpc, by=c("id", "adm_date"))
dpc %>% glimpse()
dpc %>% write.csv("data/ichinomiya/cleaned/ichinomiya/dpc.csv")

drainage <- read_csv("data/ichinomiya/cleaned/ichinomiya/drainage.csv", 
                     locale = locale(encoding = "SHIFT-JIS"))

lab <- read_csv("data/ichinomiya/cleaned/ichinomiya/lab.csv", 
                locale = locale(encoding = "SHIFT-JIS"))
