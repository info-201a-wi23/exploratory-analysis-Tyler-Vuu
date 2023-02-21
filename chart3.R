library("dplyr")
library("ggplot2")


#data
income_df <- read.csv("https://www.globaldietarydatabase.org/system/files/microdata/data-upload/2020-12/USA_NHANES_2013-2014_ParticipantData.csv")

meal_type_df <- read.csv("https://www.globaldietarydatabase.org/system/files/microdata/data-upload/2020-12/USA_NHANES_2013-2014_DietData_part2.csv")

# 1. Select the columns that includes household income from the first dataset and type of meal from the second.

income <- income_df %>% select(hh_income, na.rm = TRUE)
meals <- meal_type_df %>% select(meal_type, na.rm = TRUE)

# 2. join the data set 

income_meals_df <- income_df %>% left_join(meal_type_df, by = "id")
income_meals_df <- income_meals_df %>% select(id, hh_income, meal_type)

# 3. remove blank variables
income_meals_df <- income_meals_df %>% na.omit()

# 4. Group the data and calculate the count

income_meals_count <- income_meals_df %>% 
  group_by(hh_income, meal_type) %>% 
  summarize(count = n())

# 5. plot as a bar graph

bar_plot <- ggplot(income_meals_count, aes(x = hh_income, y = count, fill = meal_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Meal Type vs. Household Income", x = "Household Income", y = "Count", fill = "Meal Type")
