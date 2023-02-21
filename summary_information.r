#chart 2 summary
# graph summary
# I want to explore the relationship between family income and nutritions intake

library(dplyr)

# data
Participant.df <- read.csv("USA_NHANES_2013-2014_ParticipantData.csv", stringsAsFactors = F)
Diet.df <- read.csv("USA_NHANES_2013-2014_DietData_part2.csv", stringsAsFactors = F)

# 1. Select the columns that includes important nutritions
Nutrition.df <- Diet.df %>%
  select(id,totalpro, carb, totalfat)

income.df <- Participant.df %>%
  select(id, fam_income)

# 2. Calculate the average nutrition intake according to each id
Nutrition.avg.df <- Nutrition.df %>%
  group_by(id) %>%
  summarize(avg_pro = mean(totalpro,na.rm = T),
            avg_carb = mean(carb,na.rm = T),
            avg_fat = mean(totalfat, na.rm = T))


# join the data set 
income.nutrition <- left_join(Nutrition.avg.df, income.df, by = "id") 

# coefficients
ols.summary <- summary(lm(income.nutrition$avg_carb ~ income.nutrition$fam_income))
income.carb.relation <- ols.summary$coefficients[2]


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
