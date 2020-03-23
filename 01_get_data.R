library(RSocrata)
library(readxl)
library(httr)

old_demographics <- read.socrata("https://data.cityofnewyork.us/resource/8mzw-jfss.csv")
demo <- read_xlsx("data/demographic-snapshot-2014-15-to-2018-19-(public).xlsx", sheet=5)
#ela_xl <- GET("https://infohub.nyced.org/docs/default-source/default-document-library/school-ela-results-2013-2019-(public).xlsx",
#                    write_disk)

# up-to-date score data downloaded from DOE website
ela_scores <-  read_xlsx("data/school-ela-results-2013-2019-(public).xlsx", sheet=2, na="s")
math_scores <- read_xlsx("data/school-math-results-2013-2019-(public).xlsx", sheet=2, na="s")
charter_ela_scores <- read_xlsx("data/charter-school-results-2013-2019-(public).xlsx", sheet=1, na="s")
charter_math_scores <- read_xlsx("data/charter-school-results-2013-2019-(public).xlsx", sheet=3, na="s")
