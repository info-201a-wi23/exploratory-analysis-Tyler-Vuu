library(dplyr)
library(ggplot2)
library("scales")

# data
Participant.df <- read.csv("USA_NHANES_2013-2014_ParticipantData.csv", stringsAsFactors = F)
Diet.df <- read.csv("USA_NHANES_2013-2014_DietData_part2.csv", stringsAsFactors = F)

# group by fam_size, calculate for average income, average bmi, average bmi_cat (classification of under/overweight)
fam_df <- Participant.df %>% 
              select(fam_size, fam_income, bmi_adults, bmi_cat) %>% 
              group_by(fam_size) %>% 
              summarize(across(everything(), list(mean = mean), na.rm = TRUE))
