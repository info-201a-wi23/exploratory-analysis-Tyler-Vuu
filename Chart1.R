# This is graph 1 comparing family income levels to BMI
#Loading necessary libraries and datasets
library(dplyr)
library(ggplot2)

participant_data = read.csv("https://www.globaldietarydatabase.org/system/files/microdata/data-upload/2020-12/USA_NHANES_2013-2014_ParticipantData.csv", stringsAsFactors = FALSE)

#Loading datasets and corresponding ID's
BMI_data <- participant_data %>%
  select(id, bmi_adults, bmi_chld)

fam_income <- participant_data %>%
  select(id, fam_income)

#Averaging the BMI's for each income level
BMI_average_df <- participant_data %>%
  group_by(id) %>%
  summarize(avg_adult = mean(bmi_adults, na.rm = TRUE),
            avg_child = mean(bmi_chld, na.rm = TRUE))

#Join Averages into a single datasets
income.BMI <- left_join(BMI_average_df, fam_income, by = "id") 
melted.df <- melt(income.BMI, id = c("id", "fam_income"))

#Dot plot of unaveraged BMI's and income levels
ggplot(participant_data) +
  geom_point(aes(x=fam_income, y=bmi_adults)) +
  labs(title = "BMI to Income Level", x = "Income Level", y = "BMI")

#Line plot of averaged BMI's and income levels
BMI.plot <- ggplot(melted.df,mapping = aes(x = fam_income, y = value, color = variable)) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Family Income Level",
       y = "Average BMIs",
       color = "BMI type")

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}









