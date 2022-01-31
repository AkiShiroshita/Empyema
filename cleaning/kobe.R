
# Import data -------------------------------------------------------------

dpcdwh <- read_csv("data/kobe/uncleaned/kobe.csv",
                   locale = locale(encoding = "cp932"))
ef <- read_excel("data/kobe/uncleaned/ef.xlsx")
lab <- read_excel("data/kobe/uncleaned/lab.xlsx")
yoshiki1 <- read_excel("data/kobe/uncleaned/yoshiki1.xlsx")
culture <- read_excel("data/kobe/uncleaned/culture.xlsx")

source(here("cleaning","chart_clean.R"),
       encoding="UTF-8")
source(here("cleaning","dpc_rename.R"),
       encoding="UTF-8")
source(here("cleaning","dpc_select.R"),
       encoding="UTF-8")

# DPC/DWH -----------------------------------------------------------------

dpcdwh %>% glimpse()
dpcdwh %>% colnames()

dpc <- dpcdwh %>% 
  select()


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