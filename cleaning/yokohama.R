
# Data import -------------------------------------------------------------

dpc <- read_excel("data/yokohama/uncleaned/kumamoto_dpcdwh.xlsx")
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

chart_clean(chart)
chart <- chart %>% 
  replace_na(replace = list(pleural_look = 2))

key <- chart %>% 
  select(id, adm_date, diag_date)

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
  select(id, adm_date, name, date, value)

lab1 <- inner_join(key, lab1, by = c("id", "adm_date"))
lab_key <- lab1 %>% 
  select(id, adm_date, diag_date)

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
  lab_combine <<- left_join(lab_combine, lab, key = c("id", "adm"))
}

select_lab("ＷＢＣ", blood_wbc)
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
  mutate_all(.funs = ~ as.character(.)) 
lab2 %>% glimpse()
lab2 %>% colnames()

lab2 <- lab2 %>% 
  rename(id = "研究ID",
         adm_date = "入院日",
         name = "検査名称",
         label = "標準材料名称",
         date = "採取日時",
         value = "結果") %>% 
  select(id, adm_date, label, name, date, value)

lab2 <- inner_join(key, lab2, by = c("id", "adm_date"))
lab_key <- lab2 %>% 
  select(id, adm_date, diag_date)

unique(lab2$label)
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
  index1 <- ifelse((ymd(lab_key$diag_date) - date(lab$date[index1])) > 2, NA, index1)
  index2 <- neardate(lab_key$id,
                     lab$id,
                     lab_key$diag_date,
                     date(lab$date))
  index2 <- ifelse((ymd(lab_key$diag_date) - date(lab$date[index2])) > 2, NA, index2)
  index3 <- ifelse(is.na(index1), index2, # none before, take after
                   ifelse(is.na(index2), index1, #none after
                          ifelse(abs(date(lab$date[index2]) - ymd(lab_key$diag_date)) <
                                   abs(date(lab$date[index1])- ymd(lab_key$diag_date)), index2, index1)))
  lab <- lab[index3, ]
  lab <- lab %>% filter(!is.na(value)) %>% 
    distinct(id, adm_date, .keep_all=TRUE) %>%  
    select(-label, -name, -date)
  names(lab)[which(names(lab)=="value")] <- new_name
  lab_combine <<- left_join(lab_combine, lab, key = c("id", "adm"))
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
combine_func("blood_alp")
combine_func("pleural_pH")
combine_func("pleural_ldh")
combine_func("pleural_tp")
combine_func("pleural_alb")
combine_func("pleural_glucose")

