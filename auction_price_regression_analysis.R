# Read the data from the URL into a data frame
download.file("https://mgmt.iisc.ac.in/cm/MG229/Data_Files/clock_prices.data",
              "clock_prices.data")
data = read.table("clock_prices.data", header = T)

# Assign columns to respective variables
Price = data$Price
Age = data$Age
Bidders = data$Bidders

# Display the first few rows to verify the data
head(data)

#---------QUESTION 1---------

# Load library for 3D plotting
library(scatterplot3d)

# 3D Scatter Plot
scatterplot3d(Age, Bidders, Price,
              xlab = "Age (years)",
              ylab = "Number of Bidders",
              zlab = "Price (£ Stirling)",
              main = "Price vs. Age and Number of Bidders",
              pch = 16, color = "blue")

# Set plot margins to make space for y-axis labels
par(mar = c(5.1, 5.1, 4.1, 2.1))

# --- Plot 1: Price vs. Age with Regression Line ---
plot(Age, Price,
     xlab = "Age (years)",
     ylab = "Price (£ Stirling)",
     main = "Price vs. Age",
     pch = 16, col = "darkgreen")
# Add the line of best fit
abline(lm(Price ~ Age), col = "blue", lwd = 2)


# --- Plot 2: Price vs. Bidders with Regression Line ---
plot(Bidders, Price,
     xlab = "Number of Bidders",
     ylab = "Price (£ Stirling)",
     main = "Price vs. Bidders",
     pch = 16, col = "darkred")
# Add the line of best fit
abline(lm(Price ~ Bidders), col = "blue", lwd = 2)


# --- Plot 3: Age vs. Bidders (abline omitted due to weak linear trend) ---
plot(Age, Bidders,
     xlab = "Age (years)",
     ylab = "Number of Bidders",
     main = "Age vs. Bidders",
     pch = 16, col = "purple")
abline(lm(Age ~ Bidders), col = "blue", lwd = 2)


# --- Pairwise scatter plots with smoothed trend lines ---
pairs(data, main = "Pairwise Scatterplots of Clock Data",
      panel = panel.smooth, pch = 16)

#	Correlation Matrix 
cor(cbind(Price,Age,Bidders))

# Squared Correlation Matrix 
(cor(cbind(Price,Age,Bidders)))^2

#---------QUESTION 2(a)---------

# Fit the first-order multiple regression model
model1 <- lm(Price ~ Age + Bidders)
summary(model1)
anova(model1)
vcov(model1)

#---------QUESTION 2(b)---------

# 1. Direct Method: Use the built-in confint() function to get the CI
cat("The direct method using confint() gives:\n")
confint(model1)

# 2. Manual Method: Show the calculation steps to verify the result
cat("\nVerification using the manual formula:\n")

# Extract the coefficient (point estimate) for Bidders
beta2hat <- coefficients(model1)[3]

# Extract the standard error for the Bidders coefficient
se_Bidders <- summary(model1)$coefficients[3, 2]

# t-critical value for 95% CI with 29 degrees of freedom
t_crit <- qt(0.975, df = 29) # This is ~2.045

# Calculate the lower and upper bounds
lower_bound <- beta2hat - t_crit * se_Bidders
upper_bound <- beta2hat + t_crit * se_Bidders

cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

#---------QUESTION 2(c)---------

# 1. Create a data frame with the new clock information
new_clocks <- data.frame(Age = c(100, 100, 100), Bidders = c(10, 11, 12))

# 2. Predict the 95% confidence interval for the MEAN selling price
predicted_price_ci <- predict(model1, newdata = new_clocks, interval = "confidence", level = 0.95)

# 3. Calculate the 95% confidence interval for the PROFIT by subtracting the cost (£500)
cost <- 500
predicted_profit_ci <- predicted_price_ci - cost

# 4. Display the results
cat("Predicted 95% Confidence Intervals for Profit per Clock:", predicted_profit_ci, "\n")

#---------QUESTION 2(d)---------

# 1. Create a data frame for the specific clock being auctioned
specific_clock <- data.frame(Age = 150, Bidders = 15)

# Get the point prediction and its standard errors
prediction_info <- predict(model1, newdata = specific_clock, se.fit = T)
pred_se <- sqrt(prediction_info$se.fit^2 + prediction_info$residual.scale^2)

# Calculate the ONE-SIDED 99% upper bound using qt(0.99, df)
upper_bound_one_sided <- prediction_info$fit + pred_se * qt(0.99, df = prediction_info$df)

cat("The 99% one-sided upper bound for the bid is:", upper_bound_one_sided, "\n")

#---------QUESTION 2(e)---------

# Calculate the MARGINAL correlation coefficients
cor(data)

# --- Method 1: Calculation of Partial Correlations via residual method ---

# Partial Correlation of Price and Bidders, controlling for Age
p_on_a <- lm(Price ~ Age)
b_on_a <- lm(Bidders ~ Age)
p_corr_pb_a <- cor(residuals(p_on_a), residuals(b_on_a))
cat("Partial correlation Price-Bidders | Age:", p_corr_pb_a, "\n")

# Partial Correlation of Price and Age, controlling for Bidders
p_on_b <- lm(Price ~ Bidders)
a_on_b <- lm(Age ~ Bidders)
p_corr_pa_b <- cor(residuals(p_on_b), residuals(a_on_b))
cat("Partial correlation Price-Age | Bidders:", p_corr_pa_b, "\n")

# --- Method 2: Calculation of Partial Correlations via ANOVA Sum of Squares ---

# This measures the proportion of remaining variance a variable explains
# after another has already been included in the model.

# --- 1. Partial R-squared for Price ~ Bidders | Age ---
# This answers: "After accounting for Age, what percentage of the
# remaining variance in Price can be explained by Bidders?"

# Formula: SSR(Bidders|Age) / RSS(Age)

# Step 1: Get SSR(Bidders|Age) from the full model where Age is first.
# This is the "Sum Sq" for Bidders from anova(lm(Price ~ Age + Bidders)).
anova_model1 <- anova(lm(Price ~ Age + Bidders))
anova_model1
ssr_b_given_a <- anova_model1["Bidders", "Sum Sq"]
ssr_b_given_a

# Step 2: Get RSS(Age) from a model containing only Age.
# This is the "Sum Sq" for Residuals from anova(lm(Price ~ Age)).
anova_model2 <- anova(lm(Price ~ Age))
anova_model2
rss_age <- anova_model2["Residuals", "Sum Sq"]
rss_age

# Step 3: Calculate and display the results.
cat("----- Partial Correlation for Price ~ Bidders | Age -----\n")
parr2ba <- ssr_b_given_a / rss_age
cat("Partial R-squared:", parr2ba, "\n")
cat("Partial Correlation (sqrt):", sqrt(parr2ba), "\n\n")


# --- 2. Partial R-squared for Price ~ Age | Bidders ---
# This answers: "After accounting for Bidders, what percentage of the
# remaining variance in Price can be explained by Age?"

# Formula: SSR(Age|Bidders) / RSS(Bidders)

# Step 1: Get SSR(Age|Bidders) from the full model where Bidders is first.
# Note: The order matters in the model formula to get the correct sequential sum of squares.
anova_model3 <- anova(lm(Price ~ Bidders + Age))
anova_model3
ssr_a_given_b <- anova_model3["Age", "Sum Sq"]
ssr_a_given_b

# Step 2: Get RSS(Bidders) from a model containing only Bidders.
# This is the "Sum Sq" for Residuals from anova(lm(Price ~ Bidders)).
anova_model4 <- anova(lm(Price ~ Bidders))
anova_model4
rss_bidders <- anova_model4["Residuals", "Sum Sq"]
rss_bidders

# Step 3: Calculate and display the results.
cat("----- Partial Correlation for Price ~ Age | Bidders -----\n")
parr2ab <- ssr_a_given_b / rss_bidders
cat("Partial R-squared:", parr2ab, "\n")
cat("Partial Correlation (sqrt):", sqrt(parr2ab), "\n")


#---------QUESTION 2(f)---------

# 1. Compare Marginal Effects (Simple R-squared)
# This shows how much variance each predictor explains on its own.
r_sq_age <- summary(lm(Price ~ Age))$r.squared
r_sq_bidders <- summary(lm(Price ~ Bidders))$r.squared

cat("----- 1. Marginal Effects -----\n")
cat("R-squared for Price ~ Age model:", r_sq_age, "\n")
cat("R-squared for Price ~ Bidders model:", r_sq_bidders, "\n\n")


# 2. Compare Partial Effects (Squared Partial Correlations from 2e)
# This shows how much of the *remaining* variance each predictor explains
# after the other is already in the model.
# We use the squared values (r^2) as they represent the proportion of variance explained.
# Convert partial correlations into partial R-squared
r2_pa_b <- p_corr_pa_b^2
r2_pb_a <- p_corr_pb_a^2
cat("----- 2. Partial Effects (Proportion of Variance Explained) -----\n")
cat("Partial R-squared for Age (controlling for Bidders):", r2_pa_b, "\n")
cat("Partial R-squared for Bidders (controlling for Age):", r2_pb_a, "\n\n")


# 3. Compare Standardized Regression Coefficients
# This puts both predictors on the same scale (mean=0, sd=1) to directly
# compare the magnitude of their effects in the multiple regression model.
cat("----- 3. Standardized Regression Model -----\n")
S_Price <- (Price - mean(Price)) / sd(Price)
S_Age <- (Age - mean(Age)) / sd(Age)
S_Bidders <- (Bidders - mean(Bidders)) / sd(Bidders)

# We fit without an intercept (-1) as it's theoretically zero for standardized data
s_model1 <- lm(S_Price ~ -1 + S_Age + S_Bidders)
print(summary(s_model1))

# Formal test for difference between standardized coefficients
vcov_s_model <- vcov(s_model1)
s_coeffs <- coefficients(s_model1)

# t-statistic for the difference
t_stat <- (s_coeffs[1] - s_coeffs[2]) / sqrt(vcov_s_model[1,1] + vcov_s_model[2,2] - 2 * vcov_s_model[1,2])
# p-value for the test
p_value <- 2 * (1 - pt(abs(t_stat), df = df.residual(s_model1)))

cat("\n----- Test for Difference in Coefficients -----\n")
cat("p-value for the difference between standardized coefficients:", p_value, "\n")


#---------QUESTION 3---------

# --- Part 1: Is the first-order model acceptable? ---

# A. Visual Diagnostics: Generate the 4 standard residual plots
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# Studentized residuals
model_sigma <- summary(model1)$sigma
cat("Residual Standard Error (sigma):", model_sigma, "\n")
sres1 = residuals(model1)/(model_sigma*sqrt(1- influence(model1)$hat))

# B. Additional visual checks
par(mfrow = c(2, 2))
hist(sres1, main="Histogram of Residuals")
boxplot(sres1, main="Boxplot of Residuals")
plot(Age, sres1, main="Residuals vs. Age")
plot(Bidders, sres1, main="Residuals vs. Bidders")
par(mfrow = c(1, 1))

# C. Test for Normality of Residuals

# Load the library to make the test functions available
library(nortest)

# Create the normtest function
normtest <- function(x) {
  # Perform the 5 different normality tests
  shapiro_wilk <- shapiro.test(x)
  anderson_darling <- ad.test(x)
  cramer_von_mises <- cvm.test(x)
  lilliefors <- lillie.test(x)
  shapiro_francia <- sf.test(x)

  results <- data.frame(
    Method = c(
      shapiro_wilk$method,
      anderson_darling$method,
      cramer_von_mises$method,
      lilliefors$method,
      shapiro_francia$method
    ),
    P.Value = c(
      shapiro_wilk$p.value,
      anderson_darling$p.value,
      cramer_von_mises$p.value,
      lilliefors$p.value,
      shapiro_francia$p.value
    )
  )
  
  return(results)
}

normtest(sres1)


# D. Test for Constant Variance (Homoscedasticity)
library(lmtest)
bp_test <- bptest(model1)
print(bp_test)

# E. VIF to check for multi-collinearity
r_sq_preds <- summary(lm(Age ~ Bidders))$r.squared
print(r_sq_preds)
VIF <- 1 / (1 - r_sq_preds)
print(VIF)

# --- Part 2: Fit as appropriate a model as possible ---
# We check for an interaction effect to see if we can improve the model.
cat("\n----- Finding a More Appropriate Model -----\n")
model_interaction <- lm(Price~Age+Bidders+I(Age*Bidders))
summary(model_interaction)
anova(model_interaction)
vcov(model_interaction)

### The predictor Age in this interaction model is not significant, so it should be dropped

final_model = lm(Price~Bidders+I(Age*Bidders))
summary(final_model)
anova(final_model)
vcov(final_model)

# A. Visual Diagnostics: Generate the 4 standard residual plots
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

# Studentized residuals
final_model_sigma <- summary(final_model)$sigma
cat("Residual Standard Error (sigma):", final_model_sigma, "\n")
sres2 = residuals(final_model)/(final_model_sigma*sqrt(1- influence(final_model)$hat))

# B. Additional visual checks
par(mfrow = c(2, 2))
hist(sres2, main="Histogram of Residuals")
boxplot(sres2, main="Boxplot of Residuals")
plot(Age, sres2, main="Residuals vs. Age")
plot(Bidders, sres2, main="Residuals vs. Bidders")
par(mfrow = c(1, 1))

# C. Test for Normality of Residuals
normtest(sres2)

# D. Test for Constant Variance (Homoscedasticity)
library(lmtest)
bp_test <- bptest(final_model)
print(bp_test)

# E. VIF to check for multi-collinearity
library(car)
vif(final_model)

# --- Part 3: Re-answering questions based on the best model ---
# Based on the results, final_model is superior. We now use it.

#---------QUESTION 2(b) - Re-Answered with Final Model---------

beta1_hat = coefficients(final_model)[2]
beta1_hat
beta2_hat = coefficients(final_model)[3]
beta2_hat

#---------QUESTION 2(c) - Re-Answered with Final Model---------

# 1. Create a data frame with the new clock information
new_clocks <- data.frame(Age = c(100, 100, 100), Bidders = c(10, 11, 12))

# 2. Predict the 95% confidence interval using the final_model
predicted_price_ci <- predict(final_model, newdata = new_clocks, interval = "confidence", level = 0.95)

# 3. Calculate the 95% confidence interval for the PROFIT
cost <- 500
predicted_profit_ci <- predicted_price_ci - cost

# 4. Display the results
cat("Predicted 95% Confidence Intervals for Profit per Clock:\n")
print(predicted_profit_ci)

#---------QUESTION 2(d) - Re-Answered with Final Model---------

# 1. Create a data frame for the specific clock being auctioned
specific_clock <- data.frame(Age = 150, Bidders = 15)

# 2. Get the point prediction and standard errors from the final_model
prediction_info <- predict(final_model, newdata = specific_clock, se.fit = T)
pred_se <- sqrt(prediction_info$se.fit^2 + prediction_info$residual.scale^2)

# 3. Calculate the ONE-SIDED 99% upper bound
upper_bound_one_sided <- prediction_info$fit + pred_se * qt(0.99, df = prediction_info$df)

cat("The 99% one-sided upper bound for the bid is:", upper_bound_one_sided, "\n")
