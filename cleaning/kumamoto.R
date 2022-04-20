
# Data import -------------------------------------------------------------

dpcdwh <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                  sheet = "dpc")
dpc <- read_excel("data/kumamoto/uncleaned/kumamoto_dpc.xlsx")
oral <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                   sheet = "oral")
iv <- read_excel("data/kumamoto/uncleaned/kumamoto_dpcdwh.xlsx",
                 sheet = "iv")
ef <- read_excel("data/kumamoto/uncleaned/ef.xlsx")
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
  rename(id = "匿名化ID") 

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
  replace_na(replace = list(pleural_look = 2))

key <- chart %>% 
  select(id, adm_date, diag_date)

# DPC ---------------------------------------------------------------------

dpc <- dpc %>% 
  mutate_all(.funs = ~ as.character(.)) 
dpc %>% glimpse()
dpc %>% colnames()

dpc_renanme <- function(dpc){
  dpc <<- dpc %>% 
    rename(sex = "性別",
           birth_date = "生年月日",
           adm_date = "入院年月日",
           disc_date = "退院年月日",
           adm_style = "予定入院緊急入院区分",
           adm_ambul = "救急車による搬送の有無",
           disc_to = "退院先",
           disc_outcome = "退院時転帰",
           death_24h = "24時間以内の死亡の有無",
           adm_before = "前回同一疾病で自院入院の有無",
           dmainnm = "主傷病名",
           dmain = "主傷病コード",
           dadmnm = "入院の契機となった傷病名",
           dadm = "入院契機傷病コード",
           dres1nm = "医療資源を最も投入した傷病名",
           dres1 = "医療資源最投入傷病名１コード",
           dres2nm = "医療資源を２番目に投入した傷病名",
           dres2 = "医療資源最投入傷病名２コード",
           dcom1nm = "入院時併存症名１",
           dcom1 = "入院時併存症１コード",
           dcom2nm = "入院時併存症名２",
           dcom2 = "入院時併存症２コード",
           dcom3nm = "入院時併存症名３",
           dcom3 = "入院時併存症３コード",
           dcom4nm = "入院時併存症名４",
           dcom4 = "入院時併存症４コード",
           ddev1nm = "入院後発症疾患名１",
           ddev1 = "入院後発症疾患１コード",
           ddev2nm = "入院後発症疾患名２",
           ddev2 = "入院後発症疾患２コード",
           ddev3nm = "入院後発症疾患名３",
           ddev3 = "入院後発症疾患３コード",
           ddev4nm = "入院後発症疾患名４",
           ddev4 = "入院後発症疾患４コード",
           height = "身長",
           weight = "体重",
           smokingidx = "喫煙指数",
           adm_jcs = "意識障害",
           disc_jcs = "意識障害退院時",
           adm_adl = "入院時ADLスコア",
           disc_adl = "退院時ADLスコア",
           hughjones = "呼吸器HughJonesの呼吸困難分類",
           openm1 = "手術名１",
           opek1 = "手術１点数表コード",
           opedt1 = "手術１手術日",
           openm2 = "手術名２",
           opek2 = "手術２点数表コード",
           opedt2 = "手術２手術日",
           openm3 = "手術名３",
           opek3 = "手術３点数表コード",
           opedt3 = "手術３手術日",
           openm4 = "手術名４",
           opek4 = "手術４点数表コード",
           opedt4 = "手術４手術日",
           openm5 = "手術名５",
           opek5 = "手術５点数表コード",
           opedt5 = "手術５手術日")
}

dpc_renanme(dpc)
dpc <- dpc %>% 
  rename(id = "匿名化ID",
         aspiration = "胸腔穿刺",
         drainage = "持続的胸腔ドレナージ") 

dpc_select <- function(dpc){
  dpc_selected <<- dpc %>% 
    select(id,
           sex,
           birth_date,
           adm_date,
           disc_date,
           adm_style,
           adm_ambul,
           disc_to,
           disc_outcome,
           death_24h,
           adm_before,
           dmainnm,
           dmain,
           dadmnm,
           dadm,
           dres1nm,
           dres1,
           dres2nm,
           dres2,
           dcom1nm,
           dcom1,
           dcom2nm,
           dcom2,
           dcom3nm,
           dcom3,
           dcom4nm,
           dcom4,
           ddev1nm,
           ddev1,
           ddev2nm,
           ddev2,
           ddev3nm,
           ddev3,
           ddev4nm,
           ddev4,
           height,
           weight,
           smokingidx,
           adm_jcs,
           disc_jcs,
           adm_adl,
           disc_adl,
           hughjones,
           openm1,
           opek1,
           opedt1,
           openm2,
           opek2,
           opedt2,
           openm3,
           opek3,
           opedt3,
           openm4,
           opek4,
           opedt4,
           openm5,
           opek5,
           opedt5,
           aspiration,
           drainage)
}

dpc_select(dpc)

dpc_selected <- dpc_selected %>% 
  replace_na(replace = list(aspiration = 0, drainage = 0))
             
dpc_selected %>% glimpse()

dpc_selected$adm_adl <- sapply(strsplit(dpc_selected$adm_adl,""), function(x) sum(as.numeric(x))) 
dpc_selected$disc_adl <- sapply(strsplit(dpc_selected$disc_adl,""), function(x) sum(as.numeric(x))) 

complete <- left_join(chart, dpc_selected, by=c("id", "adm_date"))
complete %>% glimpse()
complete %>% write.csv("data/kumamoto/cleaned/dpc.csv")

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

oral_abx_start <- oral %>% 
  filter(name == "オーグメンチン配合錠２５０ＲＳ" |
           name == "アモキシシリンｶﾌﾟｾﾙ250mg(抗菌薬)(ｻﾜｼﾘﾝ)"|
           name == "エリスロシン錠１００ｍｇ") %>% 
  arrange(id, adm_date, day) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(oral_abx = name,
         oral_abx_start = day)

oral_abx_end <- oral %>% 
  filter(name == "オーグメンチン配合錠２５０ＲＳ" |
           name == "アモキシシリンｶﾌﾟｾﾙ250mg(抗菌薬)(ｻﾜｼﾘﾝ)"|
           name == "エリスロシン錠１００ｍｇ") %>% 
  arrange(id, adm_date, desc(day)) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(oral_abx = name,
         oral_abx_end = day)

oral_abx <- left_join(oral_abx_start, oral_abx_end, by = c("id", "adm_date", "oral_abx")) %>% 
  select(id, adm_date, oral_abx, oral_abx_start, oral_abx_end) %>% 
  mutate(oral_abx_start = as.Date(oral_abx_start),
         oral_abx_end = as.Date(oral_abx_end))

oral_abx <- oral_abx %>% 
  group_by(id, adm_date) %>% 
  summarise(oral_abx_all_names = paste0(oral_abx,
                               collapse = "_"),
            oral_abx_all_day = paste0(oral_abx_start,
                             oral_abx_end,
                             collapse = "_")) %>% 
  ungroup(id, adm_date) 

complete <- left_join(complete, oral_abx, by=c("id", "adm_date"))

oral_abx %>% write.csv("data/kumamoto/cleaned/oral_abx.csv")

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

iv_abx_start <- iv %>% 
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
  arrange(id, adm_date, day) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(iv_abx = name,
         iv_abx_start = day)

iv_abx_end <- iv %>% 
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
  arrange(id, adm_date, desc(day)) %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) %>% 
  rename(iv_abx = name,
         iv_abx_end = day)

iv_abx <- left_join(iv_abx_start, iv_abx_end, by = c("id", "adm_date", "iv_abx")) %>% 
  select(id, adm_date, iv_abx, iv_abx_start, iv_abx_end) %>% 
  mutate(iv_abx_start = as.Date(iv_abx_start),
         iv_abx_end = as.Date(iv_abx_end))

iv_abx <- iv_abx %>% 
  group_by(id, adm_date) %>% 
  summarise(iv_abx_all_names = paste0(iv_abx,
                               collapse = "_"),
            iv_abx_all_day = paste0(iv_abx_start,
                             iv_abx_end,
                             collapse = "_")) %>% 
  ungroup(id, adm_date) 

complete <- left_join(complete, iv_abx, by=c("id", "adm_date"))

uk <- iv %>% 
  filter(name == "《胸腔内》★★ｳﾛﾅｰｾﾞ静注用 6万単位/瓶"|
           name == "《胸腔内》★★ｳﾛﾅｰｾﾞ静注用 6万単位"|
           name == "《胸腔内》★★★ｳﾛﾅｰｾﾞ静注用 6万単位"|
           name == "★★★ｳﾛﾅｰｾﾞ静注用 6万単位/瓶"|
           name == "《胸腔内》★★★ｳﾛｷﾅｰｾﾞ６万単位\r\n"|
           name == "★★★ｳﾛｷﾅｰｾﾞ6万単位") %>% 
  distinct(id, adm_date, name, .keep_all=TRUE) 

iv_abx %>% write.csv("data/kumamoto/cleaned/iv_abx.csv")
uk %>% write.csv("data/kumamoto/cleaned/uk.csv")

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

lab <- inner_join(key, lab, by = c("id", "adm_date")) %>% 
  mutate(adm_date = ymd(adm_date),
         diag_date = ymd(diag_date),
         date = ymd(date)) %>% 
  arrange(id, date) %>% 
  drop_na()
lab_key <- key %>% 
  select(id, adm_date, diag_date) %>% 
  mutate(adm_date = ymd(adm_date),
         diag_date = ymd(diag_date)) %>% 
  arrange(id, diag_date)
  
unique(lab$name)

lab_total <- lab %>% 
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
           name == "胸-アルブミン (ALB)") %>% 
  drop_na()

lab_total %>% write.csv("data/kumamoto/uncleaned/lab_total.csv")

# closest date before or after, but no more than 2 days prior to index

lab_combine <- key %>% 
  mutate(adm_date = ymd(adm_date))

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
    select(-label, -name, -date)
  names(lab)[which(names(lab)=="value" ) ] <- new_name
  lab <- lab %>% 
    mutate(diag_date = as.character(diag_date))
  lab_combine <<- left_join(lab_combine, lab, key = c("id", "adm_date"))
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
select_lab("血糖 （グルコース）", "blood_glucose")
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
lab_combine <- lab_combine %>% 
  mutate(adm_date = as.character(adm_date))

complete <- left_join(complete, lab_combine, by=c("id", "adm_date"))

lab_combine %>% write.csv("data/kumamoto/cleaned/lab.csv")

complete %>% write.csv("data/kumamoto/cleaned/kumamoto.csv")

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

culture_combine %>% write.csv("data/kumamoto/cleaned/culture.csv")

# Procedure ----------------------------------------------------------------------

ef %>% glimpse()
ef %>% colnames()

ef <- ef %>% 
  rename(id = "匿名化ID",
         adm_date = "入院年月日",
         date = "実施年月日",
         ef = "レセプト電算処理システム用コード",
         name = "診療行為名称") %>% 
  mutate(id = as.character(id),
         adm_date = as.character(ymd(adm_date))) %>% 
  select(id, adm_date, date, ef, name)

ef_total <- inner_join(key, ef, by = c("id", "adm_date"))

ef_total %>% write.csv("data/kumamoto/uncleaned/ef_total.csv")

ef_combine <- key %>% 
  mutate(adm_date = ymd(adm_date))

ef_key <- chart %>% 
  select(id, adm_date, diag_date)

select_ef <- function(ef_name, new_name){
  ef_demo <- ef_total %>% 
    filter(ef == ef_name)
  start <- ef_demo %>% 
    arrange(adm_date, date) %>% 
    distinct(id, adm_date, .keep_all=TRUE) %>% 
    rename(start = "date")
  end <- ef_demo %>% 
    arrange(adm_date, desc(date)) %>% 
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

select_ef("140000610", "drainage")
select_ef("140004110", "drainage2")

ef_combine %>% glimpse()
ef_combine <- ef_combine %>% 
  mutate(adm_date = as.character(adm_date))

complete <- left_join(complete, ef_combine, by = c("id", "adm_date")) 
complete %>% write.csv("data/kumamoto/cleaned/kumamoto.csv")
