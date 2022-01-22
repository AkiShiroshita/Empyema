
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
             "survival")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

getwd()
rm(list=ls())


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
#source(here("cleaning","xxx.R"))
#load(here("statistical_analysis","xxx.R"))
