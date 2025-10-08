getwd()
library(readr)
read.csv("AgincourtDataCSV.csv")
the.fulldata <- as.matrix(read.csv("AgincourtDataCSV.csv", header = TRUE, sep = ","))

# Sample 10,000 rows
set.seed(123)
my.data <- the.fulldata[sample(1:24999, 10000), 1:5]
write.table(my.data, "Beerappa-223724697-AgincourtMyData.txt")

# Q1.1 - Analyzing the Water Temperature variable

# First, I'm plotting a histogram to see the frequency distribution of water temperatures.
# This helps me understand how the values are spread — whether it's normal, skewed, etc.
hist(my.data[, 5],
     main = "Histogram of Water Temperature",
     xlab = "Water Temperature (°C)",
     col = "skyblue",
     border = "black")

# Now I’m plotting a boxplot to visualize the spread and identify any outliers in the data.
# The boxplot shows the minimum, Q1, median, Q3, and maximum visually.
boxplot(my.data[, 5],
        main = "Boxplot of Water Temperature",
        ylab = "Water Temperature (°C)",
        col = "lightgreen")

# This command gives me the five-number summary of the Water Temperature variable.
# It includes: Min, Q1, Median, Q3, and Max — useful for summarizing distribution.
summary(my.data[, 5])

# This is another way to extract the exact five-number summary using base R.
fivenum(my.data[, 5])

#Q1.2 Answer – Explanation
#To summarize the center of the Water temperature variable, I would choose the median
#instead of the mean. This is because the median is less affected by outliers or any 
#skewed values in the data. Based on the boxplot, the distribution appears slightly 
#skewed (or possibly has mild outliers), so the median gives a better sense of the “typical”
#temperature value.

#To summarize the spread, I would choose the interquartile range (IQR).

#The IQR measures the spread of the middle 50% of the data (between Q1 and Q3)
#and is also resistant to outliers, unlike the standard deviation. Since the boxplot
#shows how the water temperature is concentrated within that middle range, the IQR gives
#a more reliable idea of how much variability there is among the typical values.

# Q1.3 - Scatterplot and Linear Regression

# Creating a scatterplot of Air Temperature (x) vs Water Temperature (y)
# This helps me see if there's a linear relationship between the two variables
plot(my.data[, 4], my.data[, 5],
     main = "Air Temperature vs Water Temperature",
     xlab = "Air Temperature (°C)",
     ylab = "Water Temperature (°C)",
     pch = 19, col = "blue")

# Fitting a simple linear regression model
# This line will fit a model: Water Temp = a + b * Air Temp
model <- lm(my.data[, 5] ~ my.data[, 4])

# Adding the regression line to the scatterplot
abline(model, col = "red", lwd = 2)

# Displaying the model coefficients
summary(model)

# Calculating correlation coefficient
correlation <- cor(my.data[, 4], my.data[, 5])
correlation  # Just to see the value

# Calculating coefficient of determination (R-squared)
r_squared <- correlation^2
r_squared
summary(model)$coefficients


# Q1.4(a) - Creating AT, AP, WS variables based on conditions

# Creating the 'AT' variable for Air Temperature
# High if > 28, Moderate if between 26 and 28, Low if < 26
AT <- ifelse(my.data[, 4] > 28, "High",
             ifelse(my.data[, 4] >= 26 & my.data[, 4] <= 28, "Moderate", "Low"))

# Create the 'AP' variable for Air Pressure
# High if > 1009, else Low
AP <- ifelse(my.data[, 3] > 1009, "High", "Low")

# Create the 'WS' variable for Wind Speed
# High if > 25, else Low
WS <- ifelse(my.data[, 2] > 25, "High", "Low")

# Combine them into a data frame
cross_data <- data.frame(AT, AP, WS)

# Generate the cross table
table_AT_AP_WS <- table(cross_data$AT, cross_data$AP, cross_data$WS)
table_AT_AP_WS

#Q1.4(b) – Answering the Probability Questions
#i) P(WS = High)
# Count of High WS
high_ws_count <- sum(WS == "High")
total <- length(WS)
prob_ws_high <- high_ws_count / total
prob_ws_high

#ii) P(AP = Low | AT = Moderate)
# Total rows where AT is Moderate
moderate_at <- cross_data$AT == "Moderate"
# Out of those, count where AP is Low
moderate_at_ap_low <- sum(cross_data$AP[moderate_at] == "Low")
# Total Moderate AT
moderate_total <- sum(moderate_at)
prob_ap_low_given_at_moderate <- moderate_at_ap_low / moderate_total
prob_ap_low_given_at_moderate

#iii) P(WS = High | AT = Low & AP = High)
# Rows where AT = Low and AP = High
at_low_ap_high <- cross_data$AT == "Low" & cross_data$AP == "High"
# Out of those, how many have WS = High?
ws_high_given_condition <- sum(cross_data$WS[at_low_ap_high] == "High")
# Total with AT = Low and AP = High
total_condition <- sum(at_low_ap_high)
prob_ws_high_given_conditions <- ws_high_given_condition / total_condition
prob_ws_high_given_conditions

# iv) Are Low AP and High AT mutually exclusive?
#Answer:No, Low AP and High AT are not mutually exclusive if there is at least
#one row where both conditions occur together.
sum(AP == "Low" & AT == "High")

#v) Are Low AT and Low AP independent?
#Two events A and B are independent if:
#P(A and B) = P(A) * P(B)
p_low_at <- sum(AT == "Low") / length(AT)
p_low_ap <- sum(AP == "Low") / length(AP)
p_both <- sum(AT == "Low" & AP == "Low") / length(AT)

# Compare
p_low_at * p_low_ap
p_both

#2) iv) Are Low AP and High AT mutually exclusive?
sum(AP == "Low" & AT == "High")


#3.2c
# Inter-arrival time data
data <- c(17, 5, 10, 20, 18, 6, 15, 8)

# Given prior hyperparameters
a <- 4
b <- 1

# Frequentist data stats
N <- length(data)
K <- mean(data)

# Posterior hyperparameters
a_post <- a + N
b_post <- b + N * K

# Lambda range for plotting
lambda <- seq(0.001, 0.5, length.out = 1000)

# Prior: Gamma(a, b)
prior <- dgamma(lambda, shape = a, rate = b)

# Likelihood (unnormalized, for visualization only)
likelihood <- lambda^N * exp(-N * lambda * K)
likelihood <- likelihood / max(likelihood)  # Scale for plotting

# Posterior: Gamma(a_post, b_post)
posterior <- dgamma(lambda, shape = a_post, rate = b_post)

# Plotting
plot(lambda, prior, type = "l", col = "blue", lwd = 2, ylim = c(0, max(c(prior, posterior))), 
     xlab = expression(lambda), ylab = "Density", main = "Prior, Likelihood & Posterior Distributions")
lines(lambda, likelihood, col = "green", lwd = 2)
lines(lambda, posterior, col = "red", lwd = 2)

legend("topright", legend = c("Prior", "Likelihood (scaled)", "Posterior"),
       col = c("blue", "green", "red"), lwd = 2)


#Q.4 C Define grid for theta values
theta <- seq(50, 400, length.out = 1000)

# Define the custom piecewise prior distribution
prior <- ifelse(theta >= 50 & theta <= 100, (1 / 10000) * theta - (1 / 200),
                ifelse(theta > 100 & theta <= 250, (-1 / 120000) * theta + (7 / 1200),
                       ifelse(theta > 250 & theta <= 300, (-1 / 24000) * theta + (17 / 1200),
                              ifelse(theta > 300 & theta <= 400, (-1 / 60000) * theta + (1 / 150), 0))))

# Likelihood (Normal distribution) for n = 1
# Observation: sample mean = 250, known standard deviation = 50
likelihood <- dnorm(theta, mean = 250, sd = 50)

# Posterior is proportional to prior × likelihood
posterior_unnormalized <- prior * likelihood

# Normalize posterior to get a probability distribution
posterior <- posterior_unnormalized / sum(posterior_unnormalized)

# Calculate posterior mean and standard deviation
posterior_mean <- sum(theta * posterior)
posterior_sd <- sqrt(sum((theta - posterior_mean)^2 * posterior))

# Print posterior mean and sd
cat("Posterior Mean:", posterior_mean, "\n")
cat("Posterior SD:", posterior_sd, "\n")

# Plotting
par(mfrow = c(1, 3))  # Layout for 3 plots side-by-side

# Plot prior
plot(theta, prior, type = "l", lwd = 2, col = "blue", 
     main = "Prior Distribution", xlab = expression(theta), ylab = "Density")

# Plot likelihood
plot(theta, likelihood, type = "l", lwd = 2, col = "green", 
     main = "Likelihood", xlab = expression(theta), ylab = "Density")

# Plot posterior
plot(theta, posterior, type = "l", lwd = 2, col = "red", 
     main = "Posterior Distribution", xlab = expression(theta), ylab = "Density")


#Q.5Load the dataset
zz <- read.table("lettersdata.txt")
zz <- as.matrix(zz)

# Part a: Scatterplot for visual inspection 
plot(zz, main = "Scatterplot of Letters Data", xlab = "X1", ylab = "X2", pch = 19)

# Based on the plot, visually we can estimate number of clusters (e.g., k = 3)

# Part b: K-Means Clustering with estimated k
set.seed(123)  # for reproducibility
k <- 3  # replace with your visually estimated value
kmeans_result <- kmeans(zz, centers = k)

# Plot K-means clusters
plot(zz, col = kmeans_result$cluster, pch = 19,
     main = paste("K-Means Clustering (k =", k, ")"),
     xlab = "X1", ylab = "X2")
points(kmeans_result$centers, col = 1:k, pch = 4, cex = 2, lwd = 3)

#  Part c: Elbow Method (TOTWSS vs. k from 2 to 20)
totwss <- numeric(19)
for (k in 2:20) {
  set.seed(123)
  km <- kmeans(zz, centers = k)
  totwss[k - 1] <- km$tot.withinss
}

# Plot TOTWSS vs. k
plot(2:20, totwss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within Sum of Squares (TOTWSS)",
     main = "Elbow Method to Determine Optimal k")


#Q5.2# Load required package
install.packages("kernlab")  # Run only once if not installed
library(kernlab)

# Load the data (if not already loaded)
zz <- read.table("lettersdata.txt")
zz <- as.matrix(zz)

#  Spectral Clustering with 4 clusters ----
sc <- specc(zz, centers = 4)  # Spectral clustering

# Plot spectral clustering results
plot(zz, col = sc, pch = 19,
     main = "Spectral Clustering (k = 4)",
     xlab = "X1", ylab = "X2")

#  K-Means Clustering for comparison (k = 4) ----
set.seed(123)
km4 <- kmeans(zz, centers = 4)

# Plot K-means clustering results for comparison
plot(zz, col = km4$cluster, pch = 19,
     main = "K-Means Clustering (k = 4)",
     xlab = "X1", ylab = "X2")
points(km4$centers, col = 1:4, pch = 4, cex = 2, lwd = 3)

