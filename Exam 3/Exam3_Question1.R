# Aidan Kiser
# 18 April 2024
# STAT 3010
# Xia
# Exam 3 Question 1

# load the necessary library
library(readr)
# read data
rubythroats <- read_csv("rubythroats.csv")

# a.

# calc the mean, median, quartiles, and min/max for wing.length
mean_wl <- mean(rubythroats$wing.length, na.rm = TRUE)
median_wl <- median(rubythroats$wing.length, na.rm = TRUE)
quantiles_wl <- quantile(rubythroats$wing.length, probs = c(0.25, 0.75), 
                         na.rm = TRUE)
min_wl <- min(rubythroats$wing.length, na.rm = TRUE)
max_wl <- max(rubythroats$wing.length, na.rm = TRUE)

# print
print(paste("Mean:", mean_wl))
print(paste("Median:", median_wl))
print(paste("First Quartile:", quantiles_wl[1]))
print(paste("Third Quartile:", quantiles_wl[2]))
print(paste("Min:", min_wl))
print(paste("Max:", max_wl))

# b.

# calc the standard deviation for wing.length
sd_wl <- sd(rubythroats$wing.length, na.rm = TRUE)

# calc the correlation between wing.length and tarsus.length
correlation <- cor(rubythroats$wing.length, rubythroats$tarsus.length, 
                   use = "complete.obs")

# print
print(paste("Standard Deviation of Wing Length:", sd_wl))
print(paste("Correlation between Wing Length and Tarsus Length:", correlation))

# c. 

# calc the 99% confidence interval for the average wing.length
ci <- t.test(rubythroats$wing.length, conf.level = 0.99)

# print
print(ci)
