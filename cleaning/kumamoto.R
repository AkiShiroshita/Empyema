
# Data import -------------------------------------------------------------

dpcdwh <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                  sheet = "dpc")
dpc <- read_excel("data/kumamoto/uncleaned/kumamoto_dpc.xlsx")
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
         aspiration = "胸腔穿刺",
         drainage = "持続的胸腔ドレナージ",
         mv = "人工呼吸治療の有無") 

chart_clean(chart)

chart <- chart %>% 
  replace_na(replace = list(pleural_look = 2))

key <- chart %>% 
  select(id, adm_date, diag_date)

# DPC ---------------------------------------------------------------------

dpc <- dpc %>% 
  mutate_all(.funs = ~ as.character(.)) 
dpc %>% glimpse()
dpc %>% colnames()

dpc_renanme(dpc)
dpc_select(dpc)

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
oral_abx <- oral %>% 
  filter(name == "オーグメンチン配合錠２５０ＲＳ" |
           name == "アモキシシリンｶﾌﾟｾﾙ250mg(抗菌薬)(ｻﾜｼﾘﾝ)"|
           name == "エリスロシン錠１００ｍｇ") %>% 
  distinct(id, adm_date, name, .keep_all=TRUE)

# IV ----------------------------------------------------------------------

iv <- iv %>% 
  mutate_all(.funs = ~ as.character(.)) 
iv %>% glimpse()
iv %>% colnames()

iv <- iv %>% 
  rename(id = "匿名化ID",
         adm_date = "入院年月日",
         name = "薬剤名称",
         day = "開始日") %>% 
  select(id, adm_date, name, day)

iv <- inner_join(key, iv, by = c("id", "adm_date"))

unique(iv$name)
iv_abx <- iv %>% 
  filter(name == "★スルバシリン静注用 1.5g/瓶" |
           name == "セフメタゾールﾅﾄﾘｳﾑ静注用1g/瓶(ｾﾌﾒﾀｿﾞﾝ)"|
           name == "セフトリアキソンﾅﾄﾘｳﾑ静注用 1g/瓶"|
           name == "セファゾリンﾅﾄﾘｳﾑ注射用 1g/瓶(ｾﾌｧﾒｼﾞﾝ)"|
           name == "ダラシンＳ注射液 600mg/4mL/管"|
           name == "注射用ビクシリン 1g/瓶"|
           name == "ｼﾌﾟﾛﾌﾛｷｻｼﾝ点滴静注 300mg/150mL/袋"|
           name == "メロペネム点滴静注用｢明治｣ 0.5g/瓶"|
           name == "セフトリアキソンﾅﾄﾘｳﾑ静注用　1g/瓶"|
           name == "タゾピペ配合静注用【2.25g/瓶】(ｿﾞｼﾝ)"|
           name == "ｸﾘﾝﾀﾞﾏｲｼﾝﾘﾝ酸ｴｽﾃﾙ注射液600mg/4mL(ﾀﾞﾗｼﾝ)"|
           name == "注射用ビクシリン　1g/瓶"|
           name == "メロペネム点滴静注用｢明治｣　0.5g/瓶"|
           name == "バクトラミン注　5mL/管"|
           name == "ダラシンＳ注射液　600mg/4mL/管"|
           name == "ジスロマック点滴静注用500mg"|
           name == "タゾピペ配合静注用【4.5g/瓶】(ｿﾞｼﾝ)"|
           name == "セフォタックス注射用　1g/瓶"|
           name == "バンコマイシン注「MEEK」　500mg/瓶"|
           name == "《胸腔内》ﾐﾉｻｲｸﾘﾝ塩酸塩注100mg/瓶"|
           name == "ゾシン静注用【4.5g/瓶】"|
           name == "セフメタゾン静注用　1g/瓶"|
           name == "セフォセフ静注用　1g/瓶（SBT/CPZ）"|
           name == "ペントシリン注射用　2g/瓶"|
           name == "セフトリアキソンﾅﾄﾘｳﾑ静注用1g"|
           name == "ゾシン静注用【2.25g/瓶】"|
           name == "ｼﾌﾟﾛﾌﾛｷｻｼﾝ点滴静注　300mg/150mL/袋"|
           name == "セフォセフ静注用　1g/瓶"|
           name == "注射用ペニシリンＧｶﾘｳﾑ　100万単位/瓶"|
           name == "注射用マキシピーム　1g/瓶") %>% 
  distinct(id, adm_date, name, .keep_all=TRUE)

uk <- iv %>% 
  filter(name == "《胸腔内》★★ｳﾛﾅｰｾﾞ静注用 6万単位/瓶"|
           name == "《胸腔内》★★ｳﾛﾅｰｾﾞ静注用 6万単位"|
           name == "《胸腔内》★★★ｳﾛﾅｰｾﾞ静注用 6万単位"|
           name == "★★★ｳﾛﾅｰｾﾞ静注用 6万単位/瓶"|
           name == "《胸腔内》★★★ｳﾛｷﾅｰｾﾞ６万単位\r\n"|
           name == "★★★ｳﾛｷﾅｰｾﾞ6万単位") %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) 

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

lab_combine <- key

select_lab <- function(lab_name, new_name){
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
  lab <- lab %>% filter(!is.na(value)) %>% 
    distinct(id, adm_date, .keep_all=TRUE) %>%  
    select(-label, -name, -date)
  names(lab)[which(names(lab)=="value" ) ] <- new_name
  lab_combine <<- left_join(lab_combine, lab, key = c("id", "adm"))
}

select_lab("白血球数", "blood_wbc")
select_lab("好中球数", "blood_neutro")
select_lab("ヘモグロビン", "blood_hb")
select_lab("総蛋白 (TP)", "blood_tp")
select_lab("アルブミン (ALB)", "blood_alb")
select_lab("LD (LDH)", "blood_ldh")
select_lab("血中尿素窒素 (BUN)", "blood_bun")
select_lab("クレアチニン (CRE)", "blood_cre")
select_lab("CRP", "blood_crp")
select_lab("ｱﾙｶﾘﾌｫｽﾌｧﾀｰｾﾞ（ALP）" ,"blood_alp")
select_lab("血糖 （グルコース）", "pleural_glucose")
select_lab("胸-pH", "pleural_pH")
select_lab("胸-LD（LDH）", "pleural_ldh")
select_lab("胸-蛋白定量 (TP)", "pleural_tp")
select_lab("胸-アルブミン (ALB)", "pleural_alb")
select_lab("胸-糖定量 (Glu)", "pleural_glucose")
select_lab("胸-好中球", "pleural_neutro")
select_lab("胸-細胞数", "pleural_cell")
select_lab("胸-リンパ球", "pleural_lymph")
select_lab("胸-好酸球", "pleural_eosino")
select_lab("胸-組織球", "pleural_macro")

lab_combine %>% glimpse()

lab_combine <- lab_combine %>% 
  mutate(blood_wbc = as.character(as.numeric(blood_wbc)*1000)) 

# Culture -----------------------------------------------------------------

culture <- culture %>% 
  mutate_all(.funs = ~ as.character(.)) 
culture %>% glimpse()
culture %>% colnames()

unique(culture$検体)

pleural_culture <- culture %>% 
  filter(検体 == "胸水") %>% 
  drop_na(菌名) %>% 
  filter(菌名 != "No Growth") %>% 
  rename(id = "匿名化ID",
         adm_date = "入院年月日") 

culture_combine <- left_join(key, pleural_culture, key = c("id", "adm_date")) %>% 
  select(id, adm_date, diag_date, 採取日時, 菌名) %>% 
  drop_na() %>% 
  group_by(id, adm_date) %>% 
  slice(1) %>% 
  ungroup()

culture_combine
