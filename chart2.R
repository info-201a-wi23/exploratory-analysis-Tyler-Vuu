
library(dplyr)
library(ggplot2)
library(reshape2)

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

#melt data frame to better visulization
melt.df <- melt(income.nutrition, id = c("id", "fam_income"))


# plot

nutrition.plot <- ggplot(melt.df,mapping = aes(x = fam_income,
                                               y = value,
                                               color = variable)) +
  
  scale_color_brewer(palette = "Set2") +
               geom_smooth(method = "lm", se = FALSE) +
               labs(x = "Family Income Level",
                    y = "Average Nutrition Intake (g)",
                    color = "Three Main Nutritions")
    

         
