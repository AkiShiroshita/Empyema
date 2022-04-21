
# Data import -------------------------------------------------------------

yoshiki1 <- read_excel("data/yokohama/uncleaned/dpc.xlsx")
lab1 <- read_excel("data/yokohama/uncleaned/dwh/lab/lab1.xlsx")
lab2 <- read_excel("data/yokohama/uncleaned/dwh/lab/lab2.xlsx")
culture <- read_excel("data/yokohama/uncleaned/dwh/culture/culture.xlsx")
chart <- read_csv("data/yokohama/uncleaned/chart.csv", 
                  locale = locale(encoding = "SHIFT-JIS"), 
                  na = "empty")

source(here("cleaning","chart_clean.R"),
       encoding="UTF-8")
source(here("cleaning","dpc_rename.R"),
       encoding="UTF-8")
source(here("cleaning","dpc_select.R"),
       encoding="UTF-8")

# Chart ---------------------------------------------------------------------

chart %>% glimpse()
chart %>% colnames()
chart <- chart %>% 
  filter(電子カルテでの膿胸の診断 == "膿胸") %>% 
  mutate(hospid = 3) %>% 
  rename(id = "ID")

chart_clean <- function(chart){
  chart <<- chart %>% 
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
           pleural_look = "胸水肉眼所見",
           sbp = "診断日収縮期血圧",
           dbp = "診断日拡張期血圧",
           hr = "診断日脈拍",
           rr = "診断日呼吸数",
           spo2 = "診断日Spo2",
           o2 = "診断日酸素投与量") %>% 
    select(-3, -4) %>% 
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
    mutate_all(.funs = ~ as.character(.)) 
}

chart_clean(chart)
chart <- chart %>% 
  replace_na(replace = list(pleural_look = "2")) %>% 
  arrange(id, adm_date)

key <- chart %>% 
  select(id, adm_date, diag_date)

chart %>% write.csv("data/yokohama/cleaned/chart.csv")

# Lab ---------------------------------------------------------------------

lab1 <- lab1 %>% 
  mutate_all(.funs = ~ as.character(.)) 
lab1 %>% glimpse()
lab1 %>% colnames()

lab1 <- lab1 %>% 
  rename(id = "研究ID",
         adm_date = "入院日1",
         name = "検査名",
         date = "検査日",
         value = "結果") %>% 
  select(id, adm_date, name, date, value) %>% 
  mutate(id = as.numeric(id),
         adm_date = ymd(adm_date),
         date = ymd(date)) %>% 
  arrange(id, date) %>% 
  drop_na()

key <- key %>% 
  mutate(id = as.numeric(id),
         adm_date = ymd(adm_date),
         diag_date = ymd(diag_date)) %>% 
  arrange(id, diag_date)
  
lab1 <- inner_join(key, lab1, by = c("id", "adm_date"))
lab_key <- lab1 %>% 
  select(id, adm_date, diag_date) %>% 
  mutate(id = as.numeric(id),
         adm_date = ymd(adm_date),
         diag_date = ymd(diag_date)) %>% 
  arrange(id, diag_date)

unique(lab1$name)

lab_combine <- key
select_lab <- function(lab_name, new_name){
  lab <- lab1 %>% 
    filter(name == lab_name)
  index1 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     date(lab$date),
                     best="prior")
  index1 <- ifelse((ymd(lab_key$diag_date) - ymd(lab$date[index1])) > 2, NA, index1)
  index2 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     date(lab$date))
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

select_lab("ＷＢＣ", "blood_wbc")
select_lab("好中球", "blood_neutro")
select_lab("Ｈｂ", "blood_hb")
select_lab("Hb", "blood_hb1")
select_lab("ＴＰ", "blood_tp")
select_lab("Ａlb", "blood_alb")
select_lab("ＬＤＨ", "blood_ldh")
select_lab("ＵＮ", "blood_bun")
select_lab("ＣＲＥ", "blood_cre")
select_lab("ＣＲＰ", "blood_crp")
select_lab("ＡＬＰ" ,"blood_alp")
select_lab("　Ｇｌｕ","blood_glucose")
select_lab("ｐＨ(胸水)", "pleural_pH")
select_lab("ＬＤＨ(胸水)", "pleural_ldh")
select_lab("ＬＤＨ(穿刺液他)", "pleural_ldh1")
select_lab("蛋白定量(胸水)", "pleural_tp")
select_lab("ＡＬＢ(胸水)", "pleural_alb")
select_lab("ＧＬＵ(胸水)", "pleural_glucose")
select_lab("ＧＬＵ(穿刺液他)", "pleural_glucose1")
select_lab("[穿刺液細胞数胸水]", "pleural_cell")

lab_combine

lab2 <- lab2 %>% 
  rename(id = "研究ID",
         adm_date = "入院日",
         name = "検査名称",
         date = "採取日時",
         label = "標準材料名称",
         value = "結果") %>% 
  select(id, adm_date, label, name, date, value) %>% 
  mutate(id = as.numeric(id),
         adm_date = as.Date(adm_date),
         date = as.Date(date)) %>% 
  arrange(id, date) %>% 
  drop_na()

key <- key %>% 
  mutate(id = as.numeric(id),
         adm_date = ymd(adm_date),
         diag_date = ymd(diag_date)) %>% 
  arrange(id, diag_date)

lab2 <- inner_join(key, lab2, by = c("id", "adm_date"))
lab_key <- lab2 %>% 
  select(id, adm_date, diag_date) %>% 
  mutate(id = as.numeric(id),
         diag_date = ymd(diag_date)) %>% 
  arrange(id, diag_date)

unique(lab2$name)

select_lab <- function(lab_name, label_name, new_name){
  lab <- lab2 %>% 
    filter(label == label_name) %>% 
    filter(name == lab_name)
  index1 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     date(lab$date),
                     best="prior")
  index1 <- ifelse((ymd(lab_key$diag_date) - ymd(lab$date[index1])) > 2, NA, index1)
  index2 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     date(lab$date))
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

select_lab("WBC", "静脈血", "blood_wbc2")
select_lab("好中球", "静脈血", "blood_neutro2")
select_lab("Hb", "静脈血", "blood_hb2")
select_lab("ＴＰ", "静脈血", "blood_tp2")
select_lab("Alb", "静脈血", "blood_alb2")
select_lab("LD", "静脈血", "blood_ldh2")
select_lab("UN", "静脈血", "blood_bun2")
select_lab("CRE", "静脈血", "blood_cre2")
select_lab("CRP", "静脈血", "blood_crp2")
select_lab("ALP" ,"静脈血", "blood_alp2")
select_lab("Glu", "静脈血", "blood_glucose2")
select_lab("ALP", "静脈血", "blood_alp2")
select_lab("pH(胸水)", "胸水", "pleural_pH2")
select_lab("LDH胸水", "胸水", "pleural_ldh2")
select_lab("蛋白定量(胸水)", "胸水", "pleural_tp2")
select_lab("ALB(胸水)", "胸水", "pleural_alb2")
select_lab("GLU胸水", "胸水", "pleural_glucose2")
select_lab("好中球", "胸水", "pleural_neutro")
select_lab("ﾘﾝﾊﾟ球", "胸水", "pleural_lymph")
select_lab("好酸球", "胸水", "pleural_eosino")
select_lab("組織球", "胸水", "pleural_macro")

lab_combine <- mutate_all(lab_combine, ~str_replace(.,pattern="検査不能",replacement = "NA"))
lab_combine <- mutate_all(lab_combine, ~str_replace(.,pattern="検査中止",replacement = "NA"))
lab_combine <- mutate_all(lab_combine, ~str_replace(.,pattern="- ＊ -",replacement = "NA"))

combine_func <- function(name){
  name <- enquo(name)
  lab_combine <<- lab_combine %>% 
    unite(!!name, starts_with(!!name),
          sep = "_",
          remove = TRUE,
          na.rm = TRUE)
}
combine_func("blood_wbc")
combine_func("blood_neutro")
combine_func("blood_hb")
combine_func("blood_tp")
combine_func("blood_alb")
combine_func("blood_ldh")
combine_func("blood_bun")
combine_func("blood_cre")
combine_func("blood_crp")
combine_func("blood_glucose")
combine_func("blood_alp")
combine_func("pleural_pH")
combine_func("pleural_ldh")
combine_func("pleural_tp")
combine_func("pleural_alb")
combine_func("pleural_glucose")

lab_combine %>% glimpse()

lab_combine %>% write.csv("data/yokohama/cleaned/lab_combine.csv")
lab_combine <- read_csv("data/yokohama/cleaned/lab_combine_update.csv")

lab_combine <- lab_combine %>% 
  mutate(id = as.character(id),
         adm_date = as.character(ymd(adm_date)),
         blood_wbc = as.character(as.numeric(blood_wbc)*1000)) %>% 
  arrange(id, adm_date)

complete <- left_join(chart, lab_combine, key = c("id", "adm_date"))
  
complete %>% glimpse()

lab_combine %>% write.csv("data/yokohama/cleaned/lab.csv")

complete %>% write.csv("data/yokohama/cleaned/yokohama.csv")

# Culture -----------------------------------------------------------------

screen <- str_c(key$id, collapse = "|") # for selecting variables

culture %>% glimpse()
culture %>% colnames()

culture <- culture %>% 
  rename(id = "研究ID") %>% 
  filter(str_detect(id, screen))

culture <- culture %>% 
  filter(材料名称 == "胸水") %>% 
  select(-(1:4), -(6:8), -(12:13))

culture %>% write.csv("data/yokohama/cleaned/culture.csv")

# DPC ---------------------------------------------------------------------

dpc <- key %>% 
  mutate(adm_date = ymd(adm_date))

yoshiki1_select <- function(name, new_name, value){
  data <- yoshiki1 %>% 
    filter(コード == name) %>%
    rename(id = "研究ID",
           adm_date = "入院年月日") %>% 
    mutate(adm_date = ymd(adm_date)) %>% 
    select(id, adm_date, !!value) %>% 
    rename(!!new_name := !!value) %>% 
    distinct(id, adm_date, .keep_all = TRUE)
  dpc <<- left_join(dpc, data, key = c("id", "adm_date"))
}

yoshiki1_select2 <- function(name, new_name, value){
  data <- yoshiki1 %>% 
    filter(コード == name) %>%
    rename(id = "研究ID",
           adm_date = "入院年月日") %>% 
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

yoshiki1_select("A000010", "birth_date", "ペイロード1(日付等)")
yoshiki1_select("A000010", "sex", "ペイロード2(コード等)")
yoshiki1_select("A000020", "adm_style", "ペイロード5")
yoshiki1_select("A000020", "adm_ambul", "ペイロード6")
yoshiki1_select("A000030", "disc_date", "ペイロード1(日付等)")
yoshiki1_select("A000030", "disc_to", "ペイロード2(コード等)")
yoshiki1_select("A000030", "disc_outcome", "ペイロード3")
yoshiki1_select("A000030", "death_24h", "ペイロード4")
yoshiki1_select("A000070", "adm_before", "ペイロード2(コード等)")
yoshiki1_select("A001010", "height", "ペイロード2(コード等)")
yoshiki1_select("A001010", "weight", "ペイロード3")
yoshiki1_select("A001020", "smokingidx", "ペイロード2(コード等)")
yoshiki1_select("A006010", "dmainnm", "ペイロード4")
yoshiki1_select("A006010", "dmain", "ペイロード2(コード等)")
yoshiki1_select("A006020", "dadmnm", "ペイロード4")
yoshiki1_select("A006020", "dadm", "ペイロード2(コード等)")
yoshiki1_select("A006030", "dresnm", "ペイロード4")
yoshiki1_select("A006030", "dres", "ペイロード2(コード等)")

yoshiki1_select2("A006040", "dcomnm", "ペイロード9")
yoshiki1_select2("A006040", "dcom", "ペイロード2(コード等)")
yoshiki1_select2("A006050", "ddevnm", "ペイロード9")
yoshiki1_select2("A006050", "ddev", "ペイロード2(コード等)")

yoshiki1_select("ADL0010", "adm_adl", "ペイロード2(コード等)")
yoshiki1_select("ADL0020", "disc_adl", "ペイロード2(コード等)")
yoshiki1_select("M040010", "hughjones", "ペイロード2(コード等)")
yoshiki1_select("JCS0010", "adm_jcs", "ペイロード2(コード等)")
yoshiki1_select("JCS0020", "disc_jcs", "ペイロード2(コード等)")

yoshiki1_select2("A007010", "opek", "ペイロード2(コード等)")
yoshiki1_select2("A007010", "openm", "ペイロード9")
yoshiki1_select2("A007010", "opedt", "ペイロード1(日付等)")

dpc <- dpc %>% 
  select(-diag_date)

dpc$adm_adl <- sapply(strsplit(dpc$adm_adl,""), function(x) sum(as.numeric(x))) 
dpc$disc_adl <- sapply(strsplit(dpc$disc_adl,""), function(x) sum(as.numeric(x))) 


dpc %>% write.csv("data/yokohama/cleaned/dpc.csv")
