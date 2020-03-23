library(tidyverse)
library(skimr)

# explore the main datasets
skim(all_elem)
skim(demo)
skim(full_data)

# make sure enrollment and test-taker counts don't vary widely
demo %>% group_by(sy_end) %>% summarize(three=sum(grade_3), four=sum(grade_4), five=sum(grade_5))
full_data %>% group_by(grade, year) %>% summarize(ela_takers=sum(number_tested_e), math_takers=sum(number_tested_m))

# check how different the two groups were demographically
all_elem %>% 
  group_by(sy_end, usm) %>%
  summarize(per_black=median(black_2), 
            per_white=median(white_2), 
            per_latinx=median(hispanic_2),
            per_asian=median(asian_2),
            per_m=median(male_2),
            per_f=median(female_2),
            per_ell=median(english_language_learners_2),
            per_dis=median(students_with_disabilities_2))



full_data %>% 
  distinct(dbn, year, .keep_all = TRUE) %>% 
  group_by(year, usm) %>% 
  summarize(per_black=median(percent_black), 
            per_white=median(percent_white), 
            per_latinx=median(percent_hispanic),
            per_asian=median(percent_asian),
            per_ell=median(percent_english_language_learners),
            per_dis=median(percent_students_with_disabilities))


usm_tbl %>% 
  tabyl(usm) %>% 
  knitr::kable(col.names=c("USM Status", "# of Schools", "%"),
               digits=3,
               align="ccc",
               padding=2,
               caption="Elementary Schools by Universal School Meals Participation, pre-2017")


# check comparative poverty and economic need rates
full_data %>% 
  filter(year>=2015) %>% 
  distinct(dbn, year, usm, .keep_all=TRUE) %>% 
  group_by(year, usm) %>% 
  summarize(mean_poverty_rate=mean(percent_poverty),
            med_poverty_rate=median(percent_poverty),
            mean_economic_need_index=mean(economic_need_index),
            med_eni=median(economic_need_index)) %>% 
  knitr::kable(col.naemes=c("Year",
                            "USM School Pre-2017?",
                            "Mean Poverty Rate",
                            "Median Poverty Rate",
                            "Mean ENI",
                            "Median ENI"),
               digits=3)


eni_by_usm <- full_data %>% 
  filter(year==2016) %>% 
  distinct(dbn, year, usm, .keep_all=TRUE) %>% 
  ggplot(aes(x=usm, y=economic_need_index)) +
  geom_boxplot() +
  labs(x="USM School Pre-2017",
       y="Economic Need Index",
       title="Figure: ENI by USM Status, 2015-16")
eni_by_usm

pov_by_usm <- full_data %>% 
  filter(year==2016) %>%
  distinct(dbn, year, usm, .keep_all=TRUE) %>% 
  ggplot(aes(x=usm, y=percent_poverty)) +
  geom_boxplot(color="purple") +
  labs(x="USM School Pre-2017",
       y="% Poverty",
       title="Figure: % Poverty by USM Status, 2015-16")
pov_by_usm


poverty_rate_hist <- all_elem %>%
  filter(sy_end==2016) %>% 
  ggplot(aes(x=poverty_2)) +
  geom_histogram(center=0, 
                 breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110),
                 closed="left",
                 color="black") +
  scale_x_continuous(breaks=c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
                     labels=c("0-9", 
                              "10-19", 
                              "20-29", 
                              "30-39", 
                              "40-49", 
                              "50-59", 
                              "60-69", 
                              "70-79", 
                              "80-89", 
                              "90-99", 
                              "100\n(USM)")) +
  labs(title="Figure 1: Elementary School Student Poverty Rates, 2015-16",
       x = "% of students in poverty", 
       y="Schools") 

poverty_rate_hist



new_pov_rate_hist <- demo %>% 
  filter(sy_end==2016) %>% 
  ggplot(aes(x=percent_poverty)) +
  geom_histogram(center=0, 
                 breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110),
                 closed="left",
                 color="black") +
  scale_x_continuous(breaks=c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
                     labels=c("0-9", 
                              "10-19", 
                              "20-29", 
                              "30-39", 
                              "40-49", 
                              "50-59", 
                              "60-69", 
                              "70-79", 
                              "80-89", 
                              "90-99", 
                              "100")) +
  labs(title="Figure 2: Adjusted Elementary School Student Poverty Rates, 2015-16",
       x = "% of students in poverty", 
       y="Schools") 

new_pov_rate_hist

## explore test scores
skim(full_data)

## distributions by grade and year
test_hist <- full_data %>% 
  ggplot() +
  geom_freqpoly(aes(x=percent_level_3_4_e, color="ELA"),
                bins= 10, 
                size=1, 
                alpha=.6,
                show.legend=TRUE) +
  geom_freqpoly(aes(x=percent_level_3_4_m, color="Math"), 
                bins=10, 
                size=1, 
                alpha=.6,
                show.legend=TRUE) +
  facet_grid(grade~year) +
  scale_x_continuous(breaks=c(0, 25, 50, 75, 100),
                     labels=c("0", 
                              "25",
                              "50",
                              "75",
                              "100")) +
  labs(x="% of Students at Level 3 or 4",
       y="Schools",
       color="Test",
       title="Figure: School-Level Test Performance by Grade and Test Year") +
  scale_color_manual(name="Subject",
                      values=c(ELA="red", Math="darkblue")) 

test_hist


## lvl 3+ distributions by group and year
grp_score_hist <- full_data %>% 
  ggplot() +
  geom_freqpoly(aes(x=percent_level_3_4_e, color="ELA"),
                bins= 10, 
                size=1, 
                alpha=.6,
                show.legend=TRUE) +
  geom_freqpoly(aes(x=percent_level_3_4_m, color="Math"), 
                bins=10, 
                size=1, 
                alpha=.6,
                show.legend=TRUE) +
  facet_grid(grp~year, 
             space="fixed", 
             scales="free_y",
             labeller=label_parsed) +
  scale_x_continuous(breaks=c(0, 25, 50, 75, 100),
                     labels=c("0", 
                              "25",
                              "50",
                              "75",
                              "100")) +
  labs(x="% of Students at Level 3 or 4",
       y="Schools",
       color="Test",
       title="Figure: School-Level Test Performance by Grade and Test Year") +
  scale_color_manual(name="Subject",
                     values=c(ELA="red", Math="darkblue")) 

grp_score_hist


score_trends_e <- full_data %>% 
  filter(!is.na(percent_level_3_4_e)) %>% 
  group_by(year, grp) %>% 
  summarize(mean_e=mean(percent_level_3_4_e)) %>% 
  ggplot(aes(x=year,
             y=mean_e,
             color=grp)) +
  geom_line(size=1) +
  labs(x="Year",
       y="Median % Level 3 or 4",
       title="Fig. ELA Test Performance Trends") +
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(20, 53))

score_trends_m <- full_data %>% 
  group_by(year, grp) %>% 
  summarize(mean_m=median(percent_level_3_4_m)) %>% 
  ggplot(aes(x=year,
             y=mean_m,
             color=grp)) +
  geom_line(size=1) +
  labs(x="Year",
       y="Median % Level 3 or 4",
       title="Fig. Math Test Performance Trends") +
  scale_y_continuous(limits=c(20, 53))

ggarrange(score_trends_e, score_trends_m, nrow=1,  ncol=2)

# just goofing off at this point
score_plt <- full_data %>% 
  ggplot(aes(x=percent_level_3_4_e,
             y=percent_level_3_4_m,
             color=grp)) +
  geom_point(alpha=0.2) +
  facet_wrap(~post_usm)
score_plt
