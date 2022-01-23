# Data import -------------------------------------------------------------

dpc <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                  sheet = "dpc")
oral <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                   sheet = "oral")
iv <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                 sheet = "iv")
gram <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                   sheet = "gram")
culture <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                      sheet = "culture")
lab <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                  sheet = "lab")
chart <- read_csv("data/kumamoto/uncleaned/kumamoto_chart.csv", 
                  locale = locale(encoding = "SHIFT-JIS"), 
                  na = "empty")

# Chart ---------------------------------------------------------------------

chart %>% glimpse()
chart %>% colnames()
chart <- chart %>% 
  filter(電子カルテでの膿胸の診断 == "膿胸") %>% 
  mutate(hospid = 5) %>% 
  rename(id = "匿名化ID",
         adm_date = "入院年月日",
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
         o2 = "診断日酸素投与量"
         ) %>% 
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
         pleural_look = case_when(dev_place == "膿性でない" ~ 0,
                                  dev_place == "膿性" ~ 1,
                                  dev_place == "不明" ~ 2)) %>% 
  mutate_all(.funs = ~ as.character(.)) 
key <- chart %>% 
  select(id, adm_date, diag_date)

# DPC ---------------------------------------------------------------------

dpc <- dpc %>% 
  mutate_all(.funs = ~ as.character(.)) 
dpc %>% glimpse()
dpc %>% colnames()

dpc <- dpc %>% 
  rename(sex = "027 性別",
         birth_date = "024 生年月日",
         adm_date = "006 入院年月日2",
         disc_date = "007 退院年月日2",
         adm_style = ""
         
  )

# Oral --------------------------------------------------------------------

oral <- oral %>% 
  mutate_all(.funs = ~ as.character(.)) 
oral %>% glimpse()
oral %>% colnames()

oral <- oral %>% 
  rename(id = "匿名化ID",
         adm_date = "入院年月日",
         name = "薬剤名称",
         day = "実施日") %>% 
  select(id, adm_date, name, day)

oral <- inner_join(key, oral, by = c("id", "adm_date"))

unique(oral$name)

oral_list <- read_excel("memo/oral_list.xlsx")

# IV ----------------------------------------------------------------------


# Lab ---------------------------------------------------------------------

lab <- lab %>% 
  mutate_all(.funs = ~ as.character(.)) 
lab %>% glimpse()
lab %>% colnames()

lab <- lab %>% 
  rename(id = "匿名化ID",
         adm_date = "006 入院年月日2",
         label = "検体名称",
         name = "検査項目名称",
         date = "検体受付日",
         value = "数値結果値") %>% 
  select(id, adm_date, label, name, date, value)

lab <- inner_join(key, lab, by = c("id", "adm_date"))
lab_key <- lab %>% 
  select(id, adm_date, diag_date)
  
unique(lab$name)
lab_toal <- lab %>% 
  filter(name == "総蛋白 (TP)" | 
           name == "アルブミン (ALB)"|
           name == "血中尿素窒素 (BUN)"|
           name == "クレアチニン (CRE)"|
           name == "LD (LDH)"|
           name == "ｱﾙｶﾘﾌｫｽﾌｧﾀｰｾﾞ（ALP）"|
           name == "CRP"|
           name == "白血球数"|
           name == "好中球数"|
           name == "ヘモグロビン"|
           name == "血糖 （グルコース）"|
           name == "胸-LD（LDH）"|
           name == "胸-蛋白定量 (TP)"|
           name == "胸-糖定量 (Glu)"|
           name == "胸-細胞数"|
           name == "胸-リンパ球"|
           name == "胸-好中球"|
           name == "胸-好酸球"|
           name == "胸-組織球"|
           name == "胸-pH"|
           name == "胸-アルブミン (ALB)") 

# closest date before or after, but no more than 2 days prior to index

select_lab <- function(lab_name){
  lab <- lab_toal %>% 
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
  return(lab)
}

lab <- lab %>% 
  filter(!is.na(value)) %>% 
  select(id, adm_date, diag_date, name, value) %>% 
  pivot_wider(names_from = name,
              values_from = value) %>% 
  rename(blood_wbc = "白血球数",
         blood_neutro = "胸-好中球",
         blood_hb = "ヘモグロビン",
         blood_tp = "総蛋白 (TP)", 
         blood_alb = "アルブミン (ALB)",
         blood_ldh = "胸-LD（LDH）",
         blood_bun = "血中尿素窒素 (BUN)",
         blood_cre = "クレアチニン (CRE)",
         blood_crp = "CRP",
         blood_alp = "ｱﾙｶﾘﾌｫｽﾌｧﾀｰｾﾞ（ALP）",
         pleural_pH = "胸-pH",
         pleural_ldh = "胸-LD（LDH）",
         pleural_tp = "胸-蛋白定量 (TP)",
         pleural_alb = "胸-アルブミン (ALB)",
         pleural_glucose = "血糖 （グルコース）",
         pleural_neutro = "胸-好中球",
         pleural_cell = "胸-細胞数",
         pleural_lymph = "胸-リンパ球",
         pleural_eosino = "胸-好酸球",
         pleural_macro = "胸-組織球") %>% 
  select(start_with("blood","pleural"))