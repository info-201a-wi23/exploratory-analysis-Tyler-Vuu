
library(dplyr)
library(ggplot2)
library(reshape2)

# data
Participant.df <- read.csv("USA_NHANES_2013-2014_ParticipantData.csv", stringsAsFactors = F)
Diet.df <-read.csv("USA_NHANES_2013-2014_DietData_part2.csv", stringsAsFactors = F)

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

# For lower income
lower.income <- income.nutrition %>%
  filter(fam_income < 5)
lower.income <- lower.income %>%
  summarize(total.carb = sum(avg_carb, na.rm = T),
             total.fat = sum(avg_fat, na.rm = T),
             total.pro = sum(avg_pro, na.rm = T)) 
total <- sum(lower.income[1,])
lower.income <- melt(lower.income)
lower.income$prop <- lower.income$value/total

mycols <- c( "#EFC000FF", "#868686FF", "#CD534CFF")

plot1 <- ggplot(lower.income, aes(x = "", y = prop, fill = variable)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = ""), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void() +
  labs(title = "lower income group",
       fill = "nutrition type")

# For higher income 
higher.income <- income.nutrition %>%
 filter(fam_income >= 5)

higher.income <- higher.income %>%
  summarize(total.carb = sum(avg_carb, na.rm = T),
         total.fat = sum(avg_fat, na.rm = T),
         total.pro = sum(avg_pro, na.rm = T)) 
total <- sum(higher.income[1,])
higher.income <- melt(higher.income)
higher.income$prop <- higher.income$value/total

plot2 <- ggplot(higher.income, aes(x = "", y = prop, fill = variable)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = ""), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void() +
  labs(title = "higher income group",
       fill = "nutrition type")

