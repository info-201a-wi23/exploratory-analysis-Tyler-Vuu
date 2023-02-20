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




