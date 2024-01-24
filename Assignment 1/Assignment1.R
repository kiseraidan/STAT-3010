# Aidan Kiser
# 18 January 2024
# STAT 3010
# Xia

# 1. Calculating the 5-number summary:

question1_data <- c(0.31, 0.356678, 0.4000112222234, 0.45667888, 0.5144, 0.558, 0.62, 0.6678, 0.7, 0.7)

# print the result
print(summary(question1_data))

# 3. (a). Calculating the Proportion of batches that have ≤ 5 defective transducers:

# making a copy of the csv data
transducers_data <- Transducers[, 1]

# calculating the counters of each unique pieces of data
defective_counts <- table(transducers_data)

# calculating the cumulative sum of frequencies ≤ 5 defective transducers
cumulative_freq_up_to_5 <- cumsum(defective_counts[1:6])

# calculating the total number of batches
total_batches <- sum(defective_counts)

# calculating the proportion of batches with at most 5 defective transducers
proportion_up_to_5 <- cumulative_freq_up_to_5 / total_batches

# print the result
print(proportion_up_to_5)

# 3. (b). Draw a histogram of the data using density on the vertical scale.

hist(transducers_data, freq = FALSE, main = "Histogram of Defective Transducers",
     xlab = "Number of Defective Transducers", ylab = "Density",
     col = "lightblue", border = "black", xlim = c(0, 8), breaks = seq(0, 8, by = 1))

# add a density curve to the histogram
lines(density(transducers_data), col = "red", lwd = 2)
