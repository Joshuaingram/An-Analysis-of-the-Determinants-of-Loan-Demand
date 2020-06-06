# ========================================================== #
# LendingClub Loan Data Final Models, Output, and Graphs
#
# Joshua D. Ingram
#
# 05/26/2020
# ========================================================== #

# =============================================================================================================================== #
# Loading Data, Packages, and Re-factoring Data
# =============================================================================================================================== #

# loading packages and setting seed for reproducibility
library(tidyverse)
library(lmtest)
library(RColorBrewer)
library(vcd)
library(plyr)
library(VGAM)
library(car)
library(knitr)

set.seed(1)

# loading in and refactoring data

# 2008
LCD_2008 <- read_csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2008.csv", col_types = cols(
  loan_amnt = col_double(),
  int_rate = col_double(),
  grade = col_factor(),
  sub_grade = col_factor(),
  home_ownership = col_factor(),
  annual_inc = col_double(),
  verification_status = col_factor(),
  issue_d = col_date(format = "%b %Y"),
  addr_state = col_factor(),
  dti = col_double(),
  fico_range_low = col_double(),
  fico_range_high = col_double(),
  application_type = col_factor(),
  region = col_factor(),
  real_estate_col = col_factor(),
  emp_length_cat = col_factor()
))
LCD_2008 <- LCD_2008[,-1]
# Refactoring grade and subgrade
LCD_2008 <- mutate(LCD_2008, grade = factor(grade, levels=c("A", "B", "C", "D", "E", "F", "G")))
LCD_2008 <- mutate(LCD_2008, sub_grade = factor(sub_grade, levels=c("A1", "A2", "A3", "A4", "A5", 
                                                                    "B1", "B2", "B3", "B4", "B5", 
                                                                    "C1", "C2", "C3", "C4", "C5", 
                                                                    "D1", "D2", "D3", "D4", "D5",
                                                                    "E1", "E2", "E3", "E4", "E5",
                                                                    "F1", "F2", "F3", "F4", "F5",
                                                                    "G1", "G2", "G3", "G4", "G5")))

# 2019
LCD_2019 <- read_csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2019.csv", col_types = cols(
  loan_amnt = col_double(),
  int_rate = col_double(),
  grade = col_factor(),
  sub_grade = col_factor(),
  home_ownership = col_factor(),
  annual_inc = col_double(),
  verification_status = col_factor(),
  issue_d = col_date(format = "%b %Y"),
  addr_state = col_factor(),
  dti = col_double(),
  fico_range_low = col_double(),
  fico_range_high = col_double(),
  application_type = col_factor(),
  region = col_factor(),
  real_estate_col = col_factor(),
  emp_length_cat = col_factor()
))
LCD_2019 <- LCD_2019[,-1]
# Refactoring grade and subgrade
LCD_2019 <- mutate(LCD_2019, grade = factor(grade, levels=c("A", "B", "C", "D", "E", "F", "G")))
LCD_2019 <- mutate(LCD_2019, sub_grade = factor(sub_grade, levels=c("A1", "A2", "A3", "A4", "A5", 
                                                                    "B1", "B2", "B3", "B4", "B5", 
                                                                    "C1", "C2", "C3", "C4", "C5", 
                                                                    "D1", "D2", "D3", "D4", "D5",
                                                                    "E1", "E2", "E3", "E4", "E5",
                                                                    "F1", "F2", "F3", "F4", "F5",
                                                                    "G1", "G2", "G3", "G4", "G5")))

# Subsetting data to only include observations with reported incomes of under 200,000 dollars
LCD_2008 <- LCD_2008[which(LCD_2008$annual_inc <= 200000),]
LCD_2019 <- LCD_2019[which(LCD_2019$annual_inc <= 200000),]

# =============================================================================================================================== #
# Exploring data
# =============================================================================================================================== #

########
# 2008 #
########

# Number of Loans
nrow(LCD_2008)

# Average Loan Amount
mean(LCD_2008$loan_amnt)

# Distribution of Loan Amounts
ggplot(LCD_2008, aes(x=loan_amnt)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Loan Amounts in 2008")

# Distribution of interest rates
ggplot(LCD_2008, aes(x=int_rate)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Interest Rate", title = "Distribution of Interest Rates in 2008")

# Proportion and distribution of loan grades
grade.counts.table <- table(LCD_2008$grade)
grade.prop.long <- as.data.frame(as.table(prop.table(grade.counts.table)))
colnames(grade.prop.long) <- c("Grade", "Proportion")
grade.prop.table <- prop.table(grade.counts.table)

ggplot(data = grade.prop.long, aes(x = Grade, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Distribution of Loan Grades in 2008") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(7, "Reds")[1:7])

# relationship between interest rate and loan amount
ggplot(LCD_2008, aes(x = int_rate, y = loan_amnt)) +
  geom_point() +
  labs(x = "Interest Rate", y = "Loan Amount", title = "Relationship between Interest Rate and Loan Amount in 2008")

########
# 2019 #
########

# Number of Loans
nrow(LCD_2019)

# Average Loan Amount
mean(LCD_2019$loan_amnt)

# Distribution of Loan Amounts
ggplot(LCD_2019, aes(x=loan_amnt)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Loan Amounts in 2019")

# Distribution of interest rates
ggplot(LCD_2019, aes(x=int_rate)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Interest Rate", title = "Distribution of Interest Rates in 2019")

# Proportion and distribution of loan grades
grade.counts.table <- table(LCD_2019$grade)
grade.prop.long <- as.data.frame(as.table(prop.table(grade.counts.table)))
colnames(grade.prop.long) <- c("Grade", "Proportion")
grade.prop.table <- prop.table(grade.counts.table)

ggplot(data = grade.prop.long, aes(x = Grade, y = Proportion, fill = Grade)) + 
  geom_bar(stat = "identity", width = .8) + 
  geom_bar(stat = "identity", width = .8, color = "black", show.legend = F, alpha = 1) + 
  labs(y = "Proportion", title = "Distribution of Loan Grades in 2019") + 
  scale_fill_manual(name = "Grade", values = brewer.pal(7, "Reds")[1:7])

# relationship between interest rate and loan amount
ggplot(LCD_2019, aes(x = int_rate, y = loan_amnt)) +
  geom_point() +
  labs(x = "Interest Rate", y = "Loan Amount", title = "Relationship between Interest Rate and Loan Amount in 2019")

# ================================================================================================================================ #
# Model Fitting, Diagnostics, and Visualizations
# ================================================================================================================================ #

#######
# 2008

# First Stage model
int_hat_08 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

# collinearity
vif(int_hat_08)

# residual diagnostics
# normality
plot(int_hat_08, 2)
plot(density(rstandard(int_hat_08)))

# linearity
plot(int_hat_08, 1)

# homoscedasticity
bptest(int_hat_08)

# outliers
plot(int_hat_08, 4)
# cook's cutoff
4/(nrow(LCD_2008) - (6+1))

# Creating Column of Price Estimates
int_rate_hat <- predict(int_hat_08)
int_rate_hat <- exp(int_rate_hat)

LCD_2008$int_rate_hat <- int_rate_hat

# Second Stage Model
lm_q_08 <- lm(log(loan_amnt) ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008)

# collinearity
vif(lm_q_08)

# residual diagnostics
# normality
plot(lm_q_08, 2)
plot(density(rstandard(int_hat_08)))

# linearity
plot(lm_q_08, 1)

# homoscedasticity
bptest(lm_q_08)

# outliers
plot(lm_q_08, 4)
# cook's cutoff
4/(nrow(LCD_2008_fin) - (6+1))

# Final Output and Visualzations
# first stage output
summary(int_hat_08)
# second stage ouput
summary(lm_q_08)

# demand line visualization
int_rate_seq <- seq(0, 32, length.out = 600)
# mean income in the us in 2008
annual_inc_seq <- 52000
emp_length <- "6-9"
region <- "northeast"
home_ownership <- "MORTGAGE"

expand_grid_1 <- expand.grid(int_rate_seq, annual_inc_seq, emp_length, region, home_ownership)
colnames(expand_grid_1) <- c("int_rate_hat", "annual_inc", "emp_length_cat", "region", "home_ownership")
fit_demand <- exp(predict(lm_q_08, newdata = expand_grid_1))

predict_amnt <- cbind(expand_grid_1, fit_demand)
colnames(predict_amnt) <- c("int_rate_hat", "annual_inc", "emp_length_cat", "region", "home_ownership", "predicted_loan_amnt")

#####################################################################
# 2-D graph of loan amount holding all constant except interest rate
ggplot(predict_amnt, aes(x = int_rate_hat, y = predicted_loan_amnt)) + 
  geom_line() + 
  labs(x = "Interest Rate", y = "Predicted Loan Amount", title = "Demand for Loans in 2008")


######
# 2019

# First Stage model
int_hat_19 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2019)

# collinearity
vif(int_hat_19)

# residual diagnostics
# normality
plot(int_hat_19, 2)
plot(density(rstandard(int_hat_19)))

# linearity
plot(int_hat_19, 1)

# homoscedasticity
bptest(int_hat_19)

# outliers
plot(int_hat_19, 4)
# cook's cutoff
4/(nrow(LCD_2019) - (6+1))
# Creating Column of Price Estimates
int_rate_hat <- predict(int_hat_19)
int_rate_hat <- exp(int_rate_hat)

LCD_2019$int_rate_hat <- int_rate_hat

# Second Stage model
lm_q_19 <- lm(log(loan_amnt) ~ int_rate_hat + annual_inc + I(annual_inc^2)+ emp_length_cat + region + home_ownership, data = LCD_2019)

# collinearity
vif(lm_q_19)

# residual diagnostics
# normality
plot(lm_q_19, 2)
plot(density(rstandard(lm_q_19)))

# linearity
plot(lm_q_19, 1)

# homoscedasticity
bptest(lm_q_19)

# outliers
plot(lm_q_19, 4)
# cook's cutoff
4/(nrow(LCD_2019) - (6+1))

# Final Output and Visualzations
# first stage ouput
summary(int_hat_19)
# second stage ouput
summary(lm_q_19)

# demand line visualization
int_rate_seq <- seq(0, 32, length.out = 600)
# mean income in the us in 2008
annual_inc_seq <- 58000
emp_length <- "6-9"
region <- "northeast"
home_ownership <- "MORTGAGE"

expand_grid_1 <- expand.grid(int_rate_seq, annual_inc_seq, emp_length, region, home_ownership)
colnames(expand_grid_1) <- c("int_rate_hat", "annual_inc", "emp_length_cat", "region", "home_ownership")
fit_demand <- exp(predict(lm_q_19, newdata = expand_grid_1))

predict_amnt <- cbind(expand_grid_1, fit_demand)
colnames(predict_amnt) <- c("int_rate_hat", "annual_inc", "emp_length_cat", "region", "home_ownership", "predicted_loan_amnt")

#####################################################################
# 2-D graph of loan amount holding all constant except interest rate
ggplot(predict_amnt, aes(x = int_rate_hat, y = predicted_loan_amnt)) + 
  geom_line() + 
  labs(x = "Interest Rate", y = "Predicted Loan Amount", title = "Demand for Loans in 2019")