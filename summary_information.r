library(dplyr)

# data
Participant.df <- read.csv("USA_NHANES_2013-2014_ParticipantData.csv", stringsAsFactors = F)
Diet.df <- read.csv("USA_NHANES_2013-2014_DietData_part2.csv", stringsAsFactors = F)

# how many participants are in the study (unique IDs)
num_participants <- length(unique(Participant.df$id))

# average, min, max bmi
avg_bmi <- mean(Participant.df$bmi_adults, na.rm = TRUE)
min_bmi <- min(Participant.df$bmi_adults, na.rm = TRUE)
max_bmi <- max(Participant.df$bmi_adults, na.rm = TRUE)

# average fam income
avg_fam_income <- mean(Participant.df$fam_income, na.rm = TRUE)

# how many have some other special diet
special_diet <- Participant.df %>% 
                filter(special_diet != '0')
num_spec_diet <- nrow(special_diet)

# female(2) vs male(1) ratio
num_female <- nrow(Participant.df %>% 
                     filter(sex == '2'))
num_male <- nrow(Participant.df %>% 
                   filter(sex == '1'))

# create a list
summary_info <- list()
summary_info$num_female <- round(num_female, digits = 2)
summary_info$num_male <- round(num_male, digits = 2)
summary_info$avg_bmi <- round(avg_bmi, digits = 2)
summary_info$min_bmi <- round(min_bmi, digits = 2)
summary_info$max_bmi <- round(max_bmi, digits = 2)
summary_info$num_spec_diet <- num_spec_diet
summary_info$avg_fam_income <- round(avg_fam_income, digits = 2)
summary_info$num_participants <- num_participants
