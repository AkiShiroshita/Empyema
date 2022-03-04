
# Import data -------------------------------------------------------------

dpcdwh <- read_csv("data/kobe/uncleaned/kobe.csv",
                   locale = locale(encoding = "SHIFT-JIS"))
chart <- read_excel("data/kobe/uncleaned/chart.xlsx",
                    sheet = "Sheet1")
lab <- read_excel("data/kobe/uncleaned/lab.xlsx")
ef <- read_excel("data/kobe/uncleaned/ef.xlsx")
yoshiki1 <- read_excel("data/kobe/uncleaned/yoshiki1.xlsx")
culture <- read_excel("data/kobe/uncleaned/culture.xlsx")

# Chart -------------------------------------------------------------------

chart %>% glimpse()
chart %>% colnames()
chart <- chart %>% 
  filter(電子カルテでの膿胸の診断 == "膿胸") %>% 
  mutate(hospid = 4) %>% 
  rename(id = "研究用ID")
chart <- chart %>% 
  select(1:20) %>% 
  rename(adm_date = "入院年月日",
         dev_place = "発症場所",
         diag_date = "膿胸診断日（推定）",
         last_date = "最終安否確認日",
         last_condition = "最終安否",
         fever = "発熱",
         cough = "咳嗽",
         sputum = "喀痰",
         chest_pain = "胸痛",
         weight_loss = "体重減少",
         surgery_3m = "入院前3ヶ月以内の外科手術",
         damage_3m = "入院前3ヶ月以内の胸部外傷歴",
         hot = "在宅酸素",
         hot_ryou_ansei = "在宅酸素安静時流量",
         hot_ryou_rousa = "在宅酸素労作時流量",
         pleural_look = "胸水肉眼所見") %>% 
  mutate(adm_date = ymd(adm_date),
         diag_date= ymd(diag_date)) %>% 
  mutate(dev_place = case_when(dev_place == "市中発症" ~ 0,
                               dev_place == "院内発症" ~ 1,
                               dev_place == "不明" ~ 2),
         last_condition = if_else(last_condition == "生存", 0, 1),
         fever = if_else(fever == "なし", 0, 1),
         cough = if_else(cough == "なし", 0, 1),
         sputum = if_else(sputum == "なし", 0, 1),
         chest_pain = if_else(chest_pain == "なし", 0, 1),
         weight_loss = if_else(weight_loss == "なし", 0, 1),
         surgery_3m = if_else(surgery_3m == "なし", 0, 1),
         damage_3m = if_else(damage_3m == "なし", 0, 1),
         hot = if_else(hot == "なし", 0, 1),
         pleural_look = case_when(pleural_look == "膿性ではない" ~ "0",
                                  pleural_look == "膿性" ~ "1",
                                  pleural_look == "不明" ~ "2")) %>% 
  select(-"データ入力者", -"電子カルテでの膿胸の診断", -ID)

key <- chart %>% 
  select(id, adm_date, diag_date)

screen <- str_c(key$id, collapse = "|") # for selecting variables

chart %>% write.csv("data/kobe/cleaned/chart.csv")

# DPC ---------------------------------------------------------------------

yoshiki1 %>% glimpse()
yoshiki1 %>% colnames()

dpc <- key %>% 
  mutate(adm_date = ymd(adm_date))

yoshiki1_select <- function(name, new_name, value){
  data <- yoshiki1 %>% 
    filter(コード == name) %>%
    rename(id = "研究用ID",
           adm_date = "入院日") %>% 
    mutate(adm_date = ymd(adm_date)) %>% 
    select(id, adm_date, !!value) %>% 
    rename(!!new_name := !!value) %>% 
    distinct(id, adm_date, .keep_all = TRUE)
  dpc <<- left_join(dpc, data, key = c("id", "adm_date"))
}

yoshiki1_select2 <- function(name, new_name, value){
  data <- yoshiki1 %>% 
    filter(コード == name) %>%
    rename(id = "研究用ID",
           adm_date = "入院日") %>% 
    mutate(adm_date = ymd(adm_date)) %>% 
    select(id, adm_date, !!value) %>% 
    rename(forget := !!value) 
  data <- data %>% 
    group_by(id, adm_date) %>% 
    summarise(all_names = paste0(forget,
                             collapse = "_")) %>% 
    ungroup(id, adm_date) %>% 
    rename(!!new_name := all_names)
  dpc <<- left_join(dpc, data, key = c("id", "adm_date"))
}

yoshiki1_select("A000010", "birth_date", "ペイロード1")
yoshiki1_select("A000010", "sex", "ペイロード2")
yoshiki1_select("A000020", "adm_style", "ペイロード5")
yoshiki1_select("A000020", "adm_ambul", "ペイロード6")
yoshiki1_select("A000030", "disc_date", "ペイロード1")
yoshiki1_select("A000030", "disc_to", "ペイロード2")
yoshiki1_select("A000030", "disc_outcome", "ペイロード3")
yoshiki1_select("A000030", "death_24h", "ペイロード4")
yoshiki1_select("A000070", "adm_before", "ペイロード2")
yoshiki1_select("A001010", "height", "ペイロード2")
yoshiki1_select("A001010", "weight", "ペイロード3")
yoshiki1_select("A001020", "smokingidx", "ペイロード2")
yoshiki1_select("A006010", "dmainnm", "ペイロード4")
yoshiki1_select("A006010", "dmain", "ペイロード2")
yoshiki1_select("A006020", "dadmnm", "ペイロード4")
yoshiki1_select("A006020", "dadm", "ペイロード2")
yoshiki1_select("A006030", "dresnm", "ペイロード4")
yoshiki1_select("A006030", "dres", "ペイロード2")

yoshiki1_select2("A006040", "dcomnm", "ペイロード9")
yoshiki1_select2("A006040", "dcom", "ペイロード2")
yoshiki1_select2("A006050", "ddevnm", "ペイロード9")
yoshiki1_select2("A006050", "ddev", "ペイロード2")

yoshiki1_select("ADL0010", "adm_adl", "ペイロード2")
yoshiki1_select("ADL0020", "disc_adl", "ペイロード2")
yoshiki1_select("M040010", "hughjones", "ペイロード2")

yoshiki1_select2("A007010", "opek", "ペイロード2")
yoshiki1_select2("A007010", "openm", "ペイロード9")
yoshiki1_select2("A007010", "opedt", "ペイロード1")

dpc <- dpc %>% 
  select(-diag_date)

complete <- left_join(chart, dpc, by = c("id", "adm_date"))
dpc %>% write.csv("data/kobe/cleaned/dpc.csv")

# Lab ---------------------------------------------------------------------

lab %>% glimpse()
lab %>% colnames()

lab <- lab %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "検査日",
         name = "検査項目名称",
         value = "結果値") %>% 
  mutate(date = as.Date(date)) %>% 
  select(id, adm_date, date, name, value)

lab_total <- lab %>% 
  filter(str_detect(id, screen))

lab_key <- lab_combine <- key

select_lab <- function(lab_name, new_name){
  lab <- lab_total %>% 
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
    distinct(id, adm_date, .keep_all=TRUE) %>%  
    select(-name, -date)
  names(lab)[which(names(lab)=="value" ) ] <- new_name
  lab_combine <<- left_join(lab_combine, lab, key = c("id", "adm_date"))
}

unique(lab_total$name)

select_lab("白血球数（ＷＢＣ）", "blood_wbc")
select_lab("ヘモグロビン量（ＨＧＢ）", "blood_hb")
select_lab("総蛋白", "blood_tp")
select_lab("アルブミン", "blood_alb")
select_lab("ＬＤ", "blood_ldh")
select_lab("尿素窒素", "blood_bun")
select_lab("クレアチニン", "blood_cre")
select_lab("ＣＲＰ", "blood_crp")
select_lab("ＡＬＰ" ,"blood_alp")
select_lab("グルコース（血清）", "blood_glucose")
select_lab("胸水－ｐＨ", "pleural_pH")
select_lab("胸水－ＬＤ", "pleural_ldh")
select_lab("胸水－総蛋白", "pleural_tp")
select_lab("胸水－アルブミン", "pleural_alb")
select_lab("胸水－グルコース", "pleural_glucose")

lab_combine %>% glimpse()

lab_combine <- lab_combine %>% 
  select(-diag_date)

complete <- left_join(complete, lab_combine, by = c("id", "adm_date"))
lab_combine %>% write.csv("data/kobe/cleaned/lab.csv")

# Culture -----------------------------------------------------------------

culture %>% glimpse()
culture %>% colnames()

culture <- culture %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "検体採取日時",
         name = "検体名",
         value = "菌名称") %>% 
  select(id, adm_date, date, name, value) 

culture <- culture %>% 
  filter(str_detect(id, screen))

culture <- culture %>% 
  filter(name == "胸水")
culture %>% write.csv("data/kobe/cleaned/culture.csv")

# Procedure ---------------------------------------------------------------

ef %>% glimpse()
ef %>% colnames()

ef <- ef %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "実施年月日",
         name = "レセプト電算処理システム用コード") %>% 
  select(id, adm_date, date, name)

ef_total <- ef %>% 
  filter(str_detect(id, screen))

ef_combine <- key %>% 
  mutate(adm_date = ymd(adm_date))

ef_key <- chart %>% 
  select(id, adm_date, diag_date)

select_ef <- function(ef_name, new_name){
  ef_demo <- ef_total %>% 
    filter(name == ef_name)
  start <- ef_demo %>% 
    arrange(adm_date) %>% 
    distinct(id, adm_date, .keep_all=TRUE) %>% 
    rename(start = "date")
  end <- ef_demo %>% 
    arrange(desc(adm_date)) %>% 
    distinct(id, adm_date, .keep_all=TRUE) %>% 
    rename(end = "date")
  var1 <- paste(new_name, "start", sep = "_")
  var2 <- paste(new_name, "end", sep = "_")
  data <- left_join(start, end, by = c("id", "adm_date")) %>% 
    select(id, adm_date, start, end) %>% 
    mutate(adm_date = ymd(adm_date),
           start = ymd(start),
           end = ymd(end)) %>% 
    rename(!!var1 := "start",
           !!var2 := "end")
  ef_combine <<- left_join(ef_combine, data, key = c("id", "adm_date"))
}

select_ef("140004110", "drainage")
select_ef("140032310", "drainage2")

ef_combine %>% glimpse()

complete <- left_join(complete, ef_combine, by = c("id", "adm_date"))

complete %>% write.csv("data/kobe/cleaned/kobe.csv")
