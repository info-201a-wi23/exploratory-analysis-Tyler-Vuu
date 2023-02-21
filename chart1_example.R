library(ggplot2)
nutrition_data = read.csv("")

BMI_data <- nutrition_data %>%
  filter(ParkName %in% c("BMI", "Income Level")) 

ggplot(BMI_data) +
  geom_point(aes(x=x_values, y = y_values)) +
  labs(title = "Arches Overtakes the Badlands in the 21st Century", x = "Income Level", y = "BMI")

