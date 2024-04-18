# Aidan Kiser
# 18 April 2024
# STAT 3010
# Xia
# Exam 3 Question 2

# load the necessary libraries
library(readr)
library(ggplot2)
# read data
mort_data <- read_csv("mort.csv")

# a.

# create scatter plots
# plot for HC
p1 <- ggplot(mort_data, aes(x = HC, y = Mortality)) +
  geom_point() +
  labs(title = "Scatter plot of Mortality vs. HC", x = "Hydrocarbons (HC)", y = "Mortality")

# plot for NOX
p2 <- ggplot(mort_data, aes(x = NOX, y = Mortality)) +
  geom_point() +
  labs(title = "Scatter plot of Mortality vs. NOX", x = "Nitrogen Oxides (NOX)", y = "Mortality")

# plot for SO2
p3 <- ggplot(mort_data, aes(x = SO2, y = Mortality)) +
  geom_point() +
  labs(title = "Scatter plot of Mortality vs. SO2", x = "Sulfur Dioxide (SO2)", y = "Mortality")

# print the plots
print(p1)
print(p2)
print(p3)

# b.

# run the linear regression with Mortality as the response and HC, NOX, SO2 as predictors
model <- lm(Mortality ~ HC + NOX + SO2, data = mort_data)

# summarize the model to get the regression equation and other statistics
summary(model)

# c.

# retrieve the adjusted R-squared value from the model summary
adjusted_r_squared <- summary(model)$adj.r.squared

# print
adjusted_r_squared
