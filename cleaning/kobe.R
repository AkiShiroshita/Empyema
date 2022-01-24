
# Import data -------------------------------------------------------------

dpcdwh <- read_csv("data/kobe/uncleaned/kobe.csv",
                   locale = locale(encoding = "SHIFT-JIS"))

# DPC/DWH -----------------------------------------------------------------

dpcdwh %>% glimpse()
dpcdwh %>% colnames()

dpc <- dpcdwh %>% 
  select()


# Lab ---------------------------------------------------------------------

lab <- read_excel("data/kobe/uncleaned/lab.xlsx")
lab %>% glimpse()
lab %>% colnames()

lab <- lab %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "検査日",
         name = "検査項目名称",
         value = "結果値") %>% 
  select(id, adm_date, date, name, value)

# Culture -----------------------------------------------------------------

culture <- read_excel("data/kobe/uncleaned/culture.xlsx")
culture %>% glimpse()
culture %>% colnames()

culture <- culture %>% 
  rename(id = "研究用ID",
         adm_date = "入院日",
         date = "検体採取日時",
         name = "検体名",
         value = "菌名称") %>% 
  select(id, adm_date, date, name, value)