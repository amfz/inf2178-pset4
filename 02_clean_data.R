library(tidyverse)
library(janitor)
library(skimr)

### UTILITY FUNcTIONS
# remove percent signs
rm_percent <- function(vec) {
  str_replace(vec, "%", "")
}

# check if it's a numeric vector
numericcharacters <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(x)))) & is.character(x)
}


### CLEAN SCHOOL DEMOG DATA + ID OLD USM SCHOOLS ###
# fix the percentage columns
old_demographics <- old_demographics %>% 
  mutate_if(is.character, rm_percent) %>% 
  mutate_if(numericcharacters, as.numeric)


# filter, clean, ID USM -- USM schools were listed with 100% poverty 
all_elem <- old_demographics %>% 
  filter(grade_3 > 0 & grade_4 > 0 & grade_5 > 0) %>%
  mutate(usm = case_when(
    poverty_2 == 100 ~ TRUE,
    TRUE ~ FALSE)) %>% 
  mutate(sy_end = as.numeric(paste0("20", str_sub(year, -2))),
         dist = str_sub(dbn, 1, 2),
         boro = str_sub(dbn, 3, 3))  


#### CLEAN TEST SCORES ###
# combine charter and regular public school records
all_ela <- ela_scores %>% 
  rbind(charter_ela_scores)
all_math <- math_scores %>% 
  rbind(charter_math_scores) 

all_ela <- all_ela %>% 
  clean_names() %>%
  filter(grade %in% c("3", "4", "5")) %>% 
  filter(!is.na(mean_scale_score))

all_math <- all_math %>% 
  clean_names() %>%
  filter(grade %in% c("3", "4", "5")) %>% 
  filter(!is.na(mean_scale_score))


# create join keys
all_ela <- all_ela %>% 
  mutate(dbn_yr = paste0(dbn, year)) %>% 
  mutate(dbn_yr_gr =paste0(dbn_yr, "_", grade))

all_math <- all_math %>% 
  mutate(dbn_yr = paste0(dbn, year)) %>% 
  mutate(dbn_yr_gr =paste0(dbn_yr, "_", grade))

all_elem <- all_elem %>% 
  mutate(dbn_yr = paste0(dbn, sy_end))

# put all the test scores together
all_tests <- full_join(all_math, all_ela, by="dbn_yr_gr", suffix=c("_m", "_e"))

# drop some extraneous columns
cols_to_drop <- c("x1_m", "x1_e", "dbn_e", "school_name_e", "grade_e", "year_e", "category_e", "dbn_yr_e")
all_tests <- all_tests %>% 
  select(-cols_to_drop)

# fix names and drop 2013 and 2014 data
all_tests <- all_tests %>% 
  rename(dbn = dbn_m,
         school = school_name_m,
         grade = grade_m,
         category = category_m,
         dbn_yr = dbn_yr_m,
         year = year_m) 

# check on whether USM status held for all years
#table(all_elem$dbn, all_elem$usm) #spoiler, it doesn't

# create a simpler USM status table
usm_status <- all_elem %>% 
  filter(sy_end >= 2015) %>%
  select(dbn_yr, dbn, usm)

# checking that USM status is consistent over years
length(unique(usm_status$dbn)) ==
  nrow(unique(data.frame(usm_status$dbn, usm_status$usm)))

usm_tbl <- usm_status %>% 
  distinct(dbn, usm)


# finally, put it all together
all_tests <- all_tests %>% 
  mutate(post_usm = case_when(
    year >= 2018 ~ TRUE,
    TRUE ~ FALSE
  ),
  rel_time = year - 2017) %>%
  left_join(usm_tbl, by="dbn") %>% 
  mutate(grp = factor(case_when(
    usm==TRUE ~ "control",
    usm==FALSE ~ "treat"
  ), labels=c("USM", "nonUSM")))

# drop the test score records for very new schools
full_data <- all_tests %>% 
  filter(!is.na(grp))


## ADD IN CURRENT DEMOGRAPHIC DATA
demo <- demo %>% clean_names()

demo <- demo %>% 
  mutate(sy_end = as.numeric(paste0("20", 
                                    str_sub(year, -2))),
         dist = str_sub(demo$dbn, 1, 2),
         boro = str_sub(demo$dbn, 3, 3),
         dbn_yr = paste0(dbn, sy_end),
         percent_poverty = percent_poverty * 100,
         economic_need_index = economic_need_index * 100)
  

full_data <- full_data %>% 
  left_join(demo %>% 
              select(boro, 
                     dist, 
                     dbn_yr, 
                     percent_white,
                     percent_black,
                     percent_asian,
                     percent_hispanic,
                     percent_students_with_disabilities,
                     percent_english_language_learners,
                     percent_poverty, 
                     economic_need_index), 
            by="dbn_yr")

# get year-over-year change by school and grade
full_data <- full_data %>% 
  group_by(dbn, grade) %>% 
  arrange(year) %>% 
  mutate(prev_e=lag(percent_level_3_4_e),
         prev_m=lag(percent_level_3_4_m),
         delta_e=percent_level_3_4_e - prev_e,
         delta_m=percent_level_3_4_m - prev_m) %>% 
  filter(year>=2015)


# do some df cleanup
rm(ela_scores)
rm(math_scores)
rm(charter_ela_scores)
rm(charter_math_scores)
rm(all_ela)
rm(all_math)
rm(old_demographics)
rm(usm_status)


