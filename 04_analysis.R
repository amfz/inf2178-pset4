library(broom)
library(performance)
library(huxtable)

## basic regression models
ela <- lm(percent_level_3_4_e ~ 
            post_usm*grp, 
          data=full_data)
math <- lm(percent_level_3_4_m ~ 
             post_usm*grp, 
           data=full_data)

summary(ela)
summary(math)


## incorporate ENI
ela_eni <- lm(percent_level_3_4_e ~ 
                post_usm*grp + 
                economic_need_index,
              data=full_data)
math_eni <- lm(percent_level_3_4_m ~ 
                 post_usm*grp + economic_need_index,
               data=full_data)

summary(ela_eni)
summary(math_eni)

## try poverty rate
ela_pov <- lm(percent_level_3_4_e ~ 
                post_usm*grp + 
                percent_poverty,
              data=full_data)
math_pov <- lm(percent_level_3_4_m ~ 
                 post_usm*grp + 
                 percent_poverty,
               data=full_data)

summary(ela_pov)
summary(math_pov)

## try controlling for grade
ela_gr <- lm(percent_level_3_4_e ~ 
               post_usm*grp + 
               grade,
             data=full_data)
math_gr <- lm(percent_level_3_4_m ~ 
                post_usm*grp + 
                grade,
              data=full_data)

summary(ela_gr)
summary(math_gr)



### LIMIT YEARS TO BEtTER MEET PARALLEL TRENDS ASSUMPTION
data_subset <- full_data %>% 
  filter(year >= 2016)

ela2 <- lm(percent_level_3_4_e ~ 
             post_usm*grp, 
           data=data_subset)
math2 <- lm(percent_level_3_4_m ~ 
              post_usm*grp, 
            data=data_subset)

summary(ela2)
summary(math2)


ela_eni2 <- lm(percent_level_3_4_e ~ 
                 post_usm*grp + economic_need_index,
               data=data_subset)
math_eni2 <- lm(percent_level_3_4_m ~ 
                  post_usm*grp + economic_need_index,
                data=data_subset)

summary(ela_eni2)
summary(math_eni2)


ela_pov2 <- lm(percent_level_3_4_e ~ 
                 post_usm*grp + 
                 percent_poverty,
               data=data_subset)
math_pov2 <- lm(percent_level_3_4_m ~ 
                  post_usm*grp + 
                  percent_poverty,
                data=data_subset)

summary(ela_pov2)
summary(math_pov2)

# add a lag var
lag_ela2 <- lm(percent_level_3_4_e ~ 
                 post_usm*grp + 
                 prev_e,
               data=data_subset)
lag_math2 <- lm(percent_level_3_4_m ~ 
                  post_usm*grp + 
                  prev_m,
                data=data_subset)



lag_ela_eni2 <- lm(percent_level_3_4_e ~ 
                     post_usm*grp + 
                     economic_need_index +
                     prev_e,
                   data=data_subset)
lag_math_eni2 <- lm(percent_level_3_4_m ~ 
                      post_usm*grp + 
                      economic_need_index + 
                      prev_m,
                    data=data_subset)

lag_ela_pov2 <- lm(percent_level_3_4_e ~ 
                     post_usm*grp + 
                     percent_poverty + 
                     prev_e,
                   data=data_subset)
lag_math_pov2 <- lm(percent_level_3_4_m ~
                      post_usm*grp + 
                      percent_poverty + 
                      prev_m,
                    data=data_subset)



lag_ela_ell <- lm(percent_level_3_4_e ~
                  post_usm*grp + 
                  percent_english_language_learners + 
                  prev_e,
                data=data_subset)

lag_m_ell <- lm(percent_level_3_4_m ~
                      post_usm*grp + 
                      percent_english_language_learners + 
                      prev_m,
                    data=data_subset)


lag_ela_eni_ell <- lm(percent_level_3_4_e ~
                        post_usm*grp + 
                        economic_need_index + 
                        percent_english_language_learners + 
                        prev_e,
                      data=data_subset)

lag_m_eni_ell <- lm(percent_level_3_4_m ~
                        post_usm*grp + 
                        economic_need_index +
                        percent_english_language_learners + 
                        prev_m,
                      data=data_subset)


lag_ela_pov_ell <- lm(percent_level_3_4_e ~
                        post_usm*grp + 
                        percent_poverty + 
                        percent_english_language_learners + 
                        prev_e,
                      data=data_subset)

lag_m_pov_ell <- lm(percent_level_3_4_m ~
                      post_usm*grp + 
                      percent_poverty +
                      percent_english_language_learners + 
                      prev_m,
                    data=data_subset)


# check the effects of single vars
lag_alone_e <- lm(percent_level_3_4_e ~ prev_e, data=data_subset)
lag_alone_m <- lm(percent_level_3_4_m ~ prev_m, data=data_subset)

eni_alone_e <- lm(percent_level_3_4_e ~ economic_need_index, 
                  data=data_subset)
eni_alone_m <- lm(percent_level_3_4_m ~ economic_need_index, 
                  data=data_subset)


# example model checks -- these were largely done in-console
check_model(lag_ela2)
check_model(lag_math2)



# comparing models forever
huxreg(lag_ela_eni2, lag_ela_pov2, lag_ela_eni_ell, lag_ela_pov_ell, lag_ela_ell, lag_ela2)
compare_performance(lag_ela_eni2, lag_ela_pov2, lag_ela_eni_ell, lag_ela_pov_ell, lag_ela2)


huxreg(lag_math_eni2, lag_math_pov2, lag_m_eni_ell, lag_m_pov_ell, lag_m_ell, lag_math2)
compare_performance(lag_math_eni2, lag_math_pov2, lag_m_eni_ell, lag_m_pov_ell, lag_math2)

