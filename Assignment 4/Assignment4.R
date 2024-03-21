# Aidan Kiser
# 20 March 2024
# STAT 3010
# Xia
# Assignment 4

# 1.In 1609 Galileo proved mathematically that the trajectory of a body falling with a horizontal velocity component is a parabola. 
# His search for an experimental setting in which horizontal motion was not affected appreciably (to study inertia) let him to 
# construct a certain apparatus. The data come from one of his experiments.
Location <- c("A", "A", "A", "B", "B", "B", "C")
Height <- c(100,200,300,450,600,800,1000)
Distance <- c(253,337,395,451,495,534,573)

# (a). Create a data frame called Galileo with the three variables and display the contents of the dataframe.
Galileo <- data.frame(Location, Height, Distance)
print(Galileo)

# (b). Compute the mean, median, variance, and IQR of the variable Distance in the dataframe Galileo.
# Mean
mean_distance <- mean(Galileo$Distance)
print(paste("Mean:", mean_distance))

# Median
median_distance <- median(Galileo$Distance)
print(paste("Median:", median_distance))

# Variance
variance_distance <- var(Galileo$Distance)
print(paste("Variance:", variance_distance))

# IQR
iqr_distance <- IQR(Galileo$Distance)
print(paste("IQR:", iqr_distance))

# (c). Create a variable for estimated distance D.Hat = 200 + .708 Height − .000344 Height² and add it to the data frame Galileo. 
# Create a new variable LO that takes a value of TRUE when the estimated distance is lower than the measured distance 
# (D.Hat < Distance) and a value of FALSE otherwise and add it to the data frame Galileo. Use the variable LO to extract a 
# subset of the Galileo dataframe removing the observations for which the estimated distance is lower than the measured distance. 
# Show the contents of this dataframe.
# Calculate D.Hat
Galileo$D.Hat <- 200 + 0.708 * Galileo$Height - 0.000344 * Galileo$Height^2

# Create LO variable
Galileo$LO <- Galileo$D.Hat < Galileo$Distance

# Extract a subset removing observations where estimated distance is lower than the measured distance
Galileo_subset <- Galileo[!Galileo$LO, ]

# Optionally, remove the D.Hat and LO columns for display purposes, if you wish
Galileo_subset <- Galileo_subset[, !(names(Galileo_subset) %in% c("D.Hat", "LO"))]

# Show the contents of this filtered dataframe
print(Galileo_subset)

# (d). (d) Plot Distance (y-axis) versus Height (x-axis) and overlay this plot with the curve of 
# D.Hat = 200 + .708 Height − .000344 Height².
# Plotting Distance versus Height
plot(Galileo$Height, Galileo$Distance, 
     xlab = "Height", ylab = "Distance", 
     main = "Distance vs. Height and Estimated Distance (D.Hat) Curve",
     col = "blue", pch = 16)

# Generating points for the D.Hat curve
height_range <- seq(min(Galileo$Height), max(Galileo$Height), length.out = 500)
d_hat_curve <- 200 + 0.708 * height_range - 0.000344 * height_range^2

# Overlaying the curve of D.Hat
lines(height_range, d_hat_curve, col = "red", lwd = 2)

# Adding a legend
legend("topright", legend = c("Measured Distance", "Estimated Distance (D.Hat)"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

# 2. The indoor thermal climate is an important characteristic affecting the health and productivity of workers in buildings. 
# The paper “Adaptive Comfort Temperature Model of Air-Conditioned Buildings in Hong Kong” 
# (Building and Environment, 2003: 837–852) reported data on a number of building characteristics measured during the summer 
# and also during the winter. The data which contain two columns on relative humidity and season are given in the file hw4q2.csv.
# (a). Read in the data and give a comparison boxplot of summer and winter relative humidity. Comment on your observations.
data <- read.csv("/Users/aidankiser/Documents/Spring 2024 Classes/Statistics for Engineers & Scientists/Assignments/Assignment 4/hw4q2.csv")

# Generating the comparison boxplot for Summer and Winter humidity
boxplot(Humidity ~ Season, data = data,
        xlab = "Season", ylab = "Humidity (%)",
        main = "Comparison of Humidity in Summer and Winter",
        col = c("lightblue", "lightgreen"))

# (b). Draw two normal quantile plots - one for summer humidity and another for winter humidity. Comment on your observations.
# Load necessary library
library(ggplot2)

# Data filtering for each season
summer_humidity <- data[data$Season == 'Summer', 'Humidity']
winter_humidity <- data[data$Season == 'Winter', 'Humidity']

# Plot for Summer Humidity
qqnorm(summer_humidity, main = "Normal Q-Q Plot for Summer Humidity")
qqline(summer_humidity, col = "red")

# Plot for Winter Humidity
qqnorm(winter_humidity, main = "Normal Q-Q Plot for Winter Humidity")
qqline(winter_humidity, col = "red")

# (c). Calculate the variances and IQRs for summer humidity and for winter humidity. Comment on your observations.
# Calculating variance for each season
variance_summer <- var(summer_humidity)
variance_winter <- var(winter_humidity)

# Calculating IQR for each season
iqr_summer <- IQR(summer_humidity)
iqr_winter <- IQR(winter_humidity)

# Printing the results
cat("Variance - Summer Humidity:", variance_summer, "\n")
cat("Variance - Winter Humidity:", variance_winter, "\n")
cat("IQR - Summer Humidity:", iqr_summer, "\n")
cat("IQR - Winter Humidity:", iqr_winter, "\n")
