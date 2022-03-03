
# Import data -------------------------------------------------------------

dpcdwh <- read_csv("data/kobe/uncleaned/kobe.csv",
                   locale = locale(encoding = "SHIFT-JIS"))
chart <- read_excel("data/kobe/uncleaned/chart.xlsx",
                    sheet = "Sheet1")
lab <- read_excel("data/kobe/uncleaned/lab.xlsx")
culture <- read_excel("data/kobe/uncleaned/culture.xlsx",
                   locale = locale(encoding = "cp932"))
ef <- read_excel("data/kobe/uncleaned/ef.xlsx")
lab <- read_excel("data/kobe/uncleaned/lab.xlsx")
yoshiki1 <- read_excel("data/kobe/uncleaned/yoshiki1.xlsx")
culture <- read_excel("data/kobe/uncleaned/culture.xlsx")

# Chart -------------------------------------------------------------------

chart %>% glimpse()
chart %>% colnames()
chart <- chart %>% 
  filter(電子カルテでの膿胸の診断 == "膿胸") %>% 
  mutate(hospid = 4) %>% 
  rename(id = "研究用ID")

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
    mutate_all(.funs = ~ as.character(.)) 

chart %>% colnames()
chart <- chart %>% 
  select(-4,-5)

key <- chart %>% 
  select(id, adm_date, diag_date)

# Lab ---------------------------------------------------------------------

lab %>% glimpse()
lab %>% colnames()

lab <- lab %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "検査日",
         name = "検査項目名称",
         value = "結果値") %>% 
  select(id, adm_date, date, name, value)
culture %>% write.csv("data/kobe/cleaned/culture.csv")

# Culture -----------------------------------------------------------------

culture %>% glimpse()
culture %>% colnames()

culture <- culture %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "検体採取日時",
         name = "検体名",
         value = "菌名称") %>% 
  select(id, adm_date, date, name, value) %>% 
  mutate(adm_date = as.character(adm_date))

culture <- inner_join(key, culture, by = c("id", "adm_date"))
culture %>% write.csv("data/kobe/cleaned/culture.csv")
