
# Import data -------------------------------------------------------------

dpcdwh <- read_csv("data/kobe/uncleaned/kobe.csv",
                   locale = locale(encoding = "SHIFT-JIS"))

# DPC/DWH -----------------------------------------------------------------

dpcdwh %>% glimpse()
dpcdwh %>% colnames()

dpc <- dpcdwh %>% 
  select()