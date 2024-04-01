# Aidan Kiser
# 31 March 2024
# STAT 3010
# Xia
# Assignment 5

# 1.

# load necessary library
library(stats)

# load the dataset from a CSV file
energy <- read.csv("hw5q1.csv", header = TRUE)

# check the first few rows to ensure it's loaded correctly
head(energy)

# 1. (a).

# fit the linear regression model with Energy as the dependent variable
# and Plastics, Paper, Garbage, and Water as independent variables
model <- lm(Energy ~ Plastics + Paper + Garbage + Water, data=energy)

# display a summary of the model to see the coefficients, R-squared, and other statistics
summary(model)

# 1. (b).

# given values for prediction
plastics <- 17.03
paper <- 23.46
garbage <- 32.45
water <- 53.23

# predict the energy content using the specified values
predicted_energy <- predict(model, newdata=data.frame(Plastics=plastics,
                                                      Paper=paper,
                                                      Garbage=garbage, Water=water))

# retrieve the actual energy content for observation #11 from the dataset
actual_energy <- energy$Energy[11]

# calculate the residual for observation #11
residual <- actual_energy - predicted_energy

# print
predicted_energy
residual

# 1. (c).

# to directly access the R-squared value, I use the summary object
r_squared <- summary(model)$r.squared

# print
print(paste("R-squared value:", r_squared))

# 1. (d).

# fit the full model with all predictors
full_model <- lm(Energy ~ Plastics + Paper + Garbage + Water, data=energy)

# perform stepwise regression using AIC as the criterion
stepwise_model <- step(full_model, direction="both")

# summary
summary(stepwise_model)

# 2. 

# load ggplot2 for advanced plotting
library(ggplot2)

# load the dataset
conc <- read.csv("hw5q2.csv", header = TRUE)

# 2. (a).

# create a scatterplot
ggplot(conc, aes(x = Meas, y = Calc)) +
  geom_point() +  # Add points
  theme_minimal() +  # Use a minimal theme
  labs(x = "Measured Load (kN)", y = "Calculated Load (kN)",
       title = "Scatterplot of Measured vs. Calculated Load") +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

# 2. (b).

# calculate the sample correlation coefficient between Meas and Calc
correlation_coefficient <- cor(conc$Meas, conc$Calc)

# print
print(correlation_coefficient)

# 3.

# load the dataset
shear <- read.csv("hw5q3.csv", header = TRUE)

# 3. (a).

# fit the linear model
model3a <- lm(y ~ x1 + x2, data = shear)

# summarize the model
summary(model3a)

# 3. (b).

# fit the linear regression model with an interaction term between x1 and x2
model3b <- lm(y ~ x1 + x2 + x1:x2, data = shear)

# summarize the model
summary(model3b)

# 3. (c).

# fit the linear regression model including the interaction term and the quadratic term for x2
model3c <- lm(y ~ x1 + x2 + I(x1*x2) + I(x2^2), data = shear)

# Summarize the model
summary(model3c)
