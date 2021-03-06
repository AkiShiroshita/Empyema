
# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "data.table",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "comorbidity",
             "psych",
             "ggplot2",
             "ggplotgui",
             "ggthemes",
             "arsenal",
             "survival",
             "rlang")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

getwd()
rm(list=ls())

usethis::use_git_config(user.name = "AkiShiroshita", user.email = "akihirokun8@gmail.com")
usethis::git_sitrep()
usethis::create_github_token()
gitcreds::gitcreds_set()

source(here("cleaning","chart_clean.R"),
       encoding="UTF-8")
source(here("cleaning","dpc_rename.R"),
       encoding="UTF-8")
source(here("cleaning","dpc_select.R"),
       encoding="UTF-8")
source(here("cleaning","uniter.R"),
       encoding="UTF-8")

# Ichinomiya --------------------------------------------------------------

file.edit("cleaning/ichinomiya.R")
rm(list=ls())

# Yokohama ----------------------------------------------------------------

file.edit("cleaning/yokohama.R")
rm(list=ls())

# Kobe --------------------------------------------------------------------

file.edit("cleaning/kobe.R")
rm(list=ls())

# Kumamoto ----------------------------------------------------------------

file.edit("cleaning/kumamoto.R")
rm(list=ls())

#file.edit("xxx")

#load(here("statistical_analysis","xxx.R"))
