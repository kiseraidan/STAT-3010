# Aidan Kiser
# 11 April 2024
# STAT 3010
# Xia
# Assignment 6

# 1. 

# load necessary library
library(readr)

# read the data1
data11 <- read_csv("hw6q1.csv")

# 1. (a).

# calc the proportion of failures for each day
data1$p <- data1$Failed / data1$Tested

# calc the average proportion of failures (p-bar)
p_bar <- mean(data1$p)

# calc the standard deviation of the proportion of failures
p_sd <- sqrt(p_bar * (1 - p_bar) / mean(data1$Tested))

# calc control limits
LCL <- max(0, p_bar - 3 * p_sd)  # lower control limit, cannot be less than 0
UCL <- p_bar + 3 * p_sd  # upper control limit

# print
cat("Centerline (p-bar):", p_bar, "\n")
cat("Lower Control Limit (LCL):", LCL, "\n")
cat("Upper Control Limit (UCL):", UCL, "\n")

# 2.

# load necessary library
library(qcc)

# load the data1
data2 <- read_csv("hw6q2.csv")

# 2. (a). & (b).

# prepare the matrix of observations for qcc
observations <- as.matrix(data[,c("x1", "x2", "x3", "x4")])

# construct the X-bar chart
xbar_chart <- qcc(observations, type="xbar", plot=TRUE)
title("X-bar Chart")

# construct the R chart
r_chart <- qcc(observations, type="R", plot=TRUE)
title("R Chart")

# 3. (a).

# given values
sigma <- 5  # Population standard deviation
k <- 1  # Units from the mean

# sample sizes
n_values <- c(2, 5, 10, 30)

# function to calculate probability
calculate_probability <- function(n) {
  sigma_x_bar <- sigma / sqrt(n)  # standard error
  z1 <- (k) / sigma_x_bar  # z-score for mu + k
  z2 <- (-k) / sigma_x_bar  # z-score for mu - k
  # area under the curve between z1 and z2
  probability <- pnorm(z1) - pnorm(z2)
  return(probability)
}

# calc and print probabilities for each sample size
probabilities <- sapply(n_values, calculate_probability)
names(probabilities) <- n_values
probabilities

# 3. (b).

# updated sample sizes
n_values_updated <- c(50, 100, 1000)

# use the same function as before for calculating probability
# calculate_probability is already defined from the previous part

# calc and print probabilities for the updated sample sizes
probabilities_updated <- sapply(n_values_updated, calculate_probability)
names(probabilities_updated) <- n_values_updated
probabilities_updated

# 3. (c).

# combine all sample sizes and their corresponding probabilities
all_n_values <- c(2, 5, 10, 30, 50, 100, 1000)
all_probabilities <- c(probabilities, probabilities_updated)

# plotting
plot(all_n_values, all_probabilities, type="o", col="blue", 
     xlab="Sample Size (n)", ylab="Probability", 
     main="Probability vs. Sample Size", log="x")
grid()
