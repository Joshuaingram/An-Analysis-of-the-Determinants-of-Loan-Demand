# ========================================================== #
# LendingClub Loan Data Exploration
#
# Joshua D. Ingram
#
# 05/19/2020
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

# From data cleaning.r, already selected years: 2008, 2012, 2015, 2017, and 2019 for analysis
# loading in aggregated years dataset and individual years
# selected years
LCD_years <- read_csv("D:/main/Datasets/LendingClub Loans/Final/LCD_years.csv", col_types = cols(
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
LCD_years <- LCD_years[,-1]
# Refactoring grade and subgrade
LCD_years <- mutate(LCD_years, grade = factor(grade, levels=c("A", "B", "C", "D", "E", "F", "G")))
LCD_years <- mutate(LCD_years, sub_grade = factor(sub_grade, levels=c("A1", "A2", "A3", "A4", "A5", 
                                                                      "B1", "B2", "B3", "B4", "B5", 
                                                                      "C1", "C2", "C3", "C4", "C5", 
                                                                      "D1", "D2", "D3", "D4", "D5",
                                                                      "E1", "E2", "E3", "E4", "E5",
                                                                      "F1", "F2", "F3", "F4", "F5",
                                                                      "G1", "G2", "G3", "G4", "G5")))

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

# 2012
LCD_2012 <- read_csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2012.csv", col_types = cols(
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
LCD_2012 <- LCD_2012[,-1]
# Refactoring grade and subgrade
LCD_2012 <- mutate(LCD_2012, grade = factor(grade, levels=c("A", "B", "C", "D", "E", "F", "G")))
LCD_2012 <- mutate(LCD_2012, sub_grade = factor(sub_grade, levels=c("A1", "A2", "A3", "A4", "A5", 
                                                                    "B1", "B2", "B3", "B4", "B5", 
                                                                    "C1", "C2", "C3", "C4", "C5", 
                                                                    "D1", "D2", "D3", "D4", "D5",
                                                                    "E1", "E2", "E3", "E4", "E5",
                                                                    "F1", "F2", "F3", "F4", "F5",
                                                                    "G1", "G2", "G3", "G4", "G5")))

# 2015
LCD_2015 <- read_csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2015.csv", col_types = cols(
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
LCD_2015 <- LCD_2015[,-1]
# Refactoring grade and subgrade
LCD_2015 <- mutate(LCD_2015, grade = factor(grade, levels=c("A", "B", "C", "D", "E", "F", "G")))
LCD_2015 <- mutate(LCD_2015, sub_grade = factor(sub_grade, levels=c("A1", "A2", "A3", "A4", "A5", 
                                                                    "B1", "B2", "B3", "B4", "B5", 
                                                                    "C1", "C2", "C3", "C4", "C5", 
                                                                    "D1", "D2", "D3", "D4", "D5",
                                                                    "E1", "E2", "E3", "E4", "E5",
                                                                    "F1", "F2", "F3", "F4", "F5",
                                                                    "G1", "G2", "G3", "G4", "G5")))

# 2017
LCD_2017 <- read_csv("D:/main/Datasets/LendingClub Loans/Final/LCD_2017.csv", col_types = cols(
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
LCD_2017 <- LCD_2017[,-1]
# Refactoring grade and subgrade
LCD_2015 <- mutate(LCD_2015, grade = factor(grade, levels=c("A", "B", "C", "D", "E", "F", "G")))
LCD_2015 <- mutate(LCD_2015, sub_grade = factor(sub_grade, levels=c("A1", "A2", "A3", "A4", "A5", 
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
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Interest Rates in 2008")

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
  labs(y = "Density", x = "Loan Amount", title = "Distribution of Interest Rates in 2019")

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



########
# 2008 #
########

##################################
# First-stage LM to estimate price

# Model form 1 #
int_hat_08_1 <- lm(int_rate ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

# collinearity
vif(int_hat_08_1)

# residual diagnostics
# normality
plot(int_hat_08_1, 2)
plot(density(rstandard(int_hat_08_1)))

# linearity
plot(int_hat_08_1, 1)

# homoscedasticity
bptest(int_hat_08_1)

# outliers
plot(int_hat_08_1, 4)
# cook's cutoff
4/(nrow(LCD_2008) - (6+1))

# problem with normality, but sample size is large. linearity and homoscedasticity are broken and there are outliers... let's log the response

# Model form 2 #
int_hat_08_2 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

# collinearity
vif(int_hat_08_2)

# residual diagnostics
# normality
plot(int_hat_08_2, 2)
plot(density(rstandard(int_hat_08_2)))

# linearity
plot(int_hat_08_2, 1)

# homoscedasticity
bptest(int_hat_08_2)

# outliers
plot(int_hat_08_2, 4)
# cook's cutoff
4/(nrow(LCD_2008) - (6+1))

# linearity is better... let's try removing the observation 1157 because of it's effects on the model

# Observation 1157 removal #
LCD_2008_mut <- LCD_2008[-c(1157),]

# Model form 3 #
int_hat_08_3 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008_mut)

# collinerity
vif(int_hat_08_3)

# residual diagnostics
# normality
plot(int_hat_08_3, 2)
plot(density(rstandard(int_hat_08_3)))

# linearity
plot(int_hat_08_3, 1)

# homoscedasticity
bptest(int_hat_08_3)

# outliers
plot(int_hat_08_3, 4)
# cook's cutoff
4/(nrow(LCD_2008_mut) - (6+1))

# Things aren't great, but I still have more to learn with 2SLS and don't want to transform too much...

# Final stage-one model #
int_hat_08 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008_mut)

# Creating Column of Price Estimates
int_rate_hat <- predict(int_hat_08)
int_rate_hat <- exp(int_rate_hat)

LCD_2008_mut$int_rate_hat <- int_rate_hat


#########################################
# Second-Stage LM to estimate loan amount

# Model form 1 #
lm_q_08_1 <- lm(loan_amnt ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_mut)

# collinearity
vif(lm_q_08_1)

# residual diagnostics
# normality
plot(lm_q_08_1, 2)
plot(density(rstandard(int_hat_08_1)))

# linearity
plot(lm_q_08_1, 1)

# homoscedasticity
bptest(lm_q_08_1)

# outliers
plot(lm_q_08_1, 4)
# cook's cutoff
4/(nrow(LCD_2008_mut) - (6+1))

# terrible residual vs fitted graph... let's try to fix that

# Model form 2 #
lm_q_08_2 <- lm(log(loan_amnt) ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + I(annual_inc^2) + emp_length_cat + region + home_ownership, data = LCD_2008_mut)

# collinearity
vif(lm_q_08_2)

# residual diagnostics
# normality
plot(lm_q_08_2, 2)
plot(density(rstandard(int_hat_08_2)))

# linearity
plot(lm_q_08_2, 1)

# homoscedasticity
bptest(lm_q_08_2)

# outliers
plot(lm_q_08_2, 4)
# cook's cutoff
4/(nrow(LCD_2008_mut) - (6+1))

# squaring annual income and taking the log of loan amount helped a ton... let's remove our observation 2058
LCD_2008_mut_2 <- LCD_2008_mut[-c(769, 1165, 1504, 1541, 2058),]

# Model form 3 #
lm_q_08_3 <- lm(log(loan_amnt) ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + I(annual_inc^2) + emp_length_cat + region + home_ownership, data = LCD_2008_mut_2)

# collinearity
vif(lm_q_08_3)

# residual diagnostics
# normality
plot(lm_q_08_3, 2)
plot(density(rstandard(int_hat_08_3)))

# linearity
plot(lm_q_08_3, 1)

# homoscedasticity
bptest(lm_q_08_3)

# outliers
plot(lm_q_08_3, 4)
# cook's cutoff
4/(nrow(LCD_2008_mut_2) - (6+1))

# squaring annual income and taking the log of loan amount helped a ton... it seems that any observations with incomes over 200,000 are hurting our model due to overextrapolation
# I'll remove these and move forward... for future developments I'll need to use another method for estimating the demand function (clarify in paper)

# Final First and Second Stage Models #

# Removed observations with incomes over 200,000
LCD_2008_fin <- LCD_2008[which(LCD_2008$annual_inc <= 200000),]

# Final First-Stage Model #
int_hat_08 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008_fin)

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
4/(nrow(LCD_2008_fin) - (6+1))

# Creating Column of Price Estimates
int_rate_hat <- predict(int_hat_08)
int_rate_hat <- exp(int_rate_hat)

LCD_2008_fin$int_rate_hat <- int_rate_hat

# Final demand model #
lm_q_08 <- lm(log(loan_amnt) ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_fin)

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

################
summary(lm_q_08)

########
# 2019 #
########

# selecting rows with incomes under 200000 for consistency
LCD_2019_fin <- LCD_2019[which(LCD_2019$annual_inc <= 200000),]

##################################
# First-stage LM to estimate price

# Model form 1 #
int_hat_19_1 <- lm(int_rate ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2019_fin)

# collinearity
vif(int_hat_19_1)

# residual diagnostics
# normality
plot(int_hat_19_1, 2)
plot(density(rstandard(int_hat_19_1)))

# linearity
plot(int_hat_19_1, 1)

# homoscedasticity
bptest(int_hat_19_1)

# outliers
plot(int_hat_19_1, 4)
# cook's cutoff
4/(nrow(LCD_2019_fin) - (6+1))

# Model form 2 #
int_hat_19_2 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2019_fin)

# collinearity
vif(int_hat_19_2)

# residual diagnostics
# normality
plot(int_hat_19_2, 2)
plot(density(rstandard(int_hat_19_2)))

# linearity
plot(int_hat_19_2, 1)

# homoscedasticity
bptest(int_hat_19_1)

# outliers
plot(int_hat_19_2, 4)
# cook's cutoff
4/(nrow(LCD_2019_fin) - (6+1))

# Final First-Stage Model #
int_hat_19 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2019_fin)

# Creating Column of Price Estimates
int_rate_hat <- predict(int_hat_19)
int_rate_hat <- exp(int_rate_hat)

LCD_2019_fin$int_rate_hat <- int_rate_hat

#########################################
# Second-Stage LM to estimate loan amount

# Model form 1 #
lm_q_19_1 <- lm(loan_amnt ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2019_fin)

# collinearity
vif(lm_q_19_1)

# residual diagnostics
# normality
plot(lm_q_19_1, 2)
plot(density(rstandard(int_hat_19_1)))

# linearity
plot(lm_q_19_1, 1)

# homoscedasticity
bptest(lm_q_19_1)

# outliers
plot(lm_q_19_1, 4)
# cook's cutoff
4/(nrow(LCD_2019_fin) - (6+1))

# Model form 2 #
lm_q_19_2 <- lm(log(loan_amnt) ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2019_fin)

# collinearity
vif(lm_q_19_2)

# residual diagnostics
# normality
plot(lm_q_19_2, 2)
plot(density(rstandard(int_hat_19_2)))

# linearity
plot(lm_q_19_2, 1)

# homoscedasticity
bptest(lm_q_19_2)

# outliers
plot(lm_q_19_2, 4)
# cook's cutoff
4/(nrow(LCD_2019_fin) - (6+1))

# Model form 3 #
lm_q_19_3 <- lm(log(loan_amnt) ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + I(annual_inc^2) + emp_length_cat + region + home_ownership, data = LCD_2019_fin)

# collinearity
vif(lm_q_19_3)

# residual diagnostics
# normality
plot(lm_q_19_3, 2)
plot(density(rstandard(int_hat_19_3)))

# linearity
plot(lm_q_19_3, 1)

# homoscedasticity
bptest(lm_q_19_3)

# outliers
plot(lm_q_19_3, 4)
# cook's cutoff
4/(nrow(LCD_2019_fin) - (6+1))

# not great... but this shows there is a structural change in the demand function

# Final Demand Model #
lm_q_19 <- lm(log(loan_amnt) ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2019_fin)
summary(lm_q_19)

# ================================================================================================================================ #
# Visualizations of Models
# ================================================================================================================================ #

########
# 2008 #
########

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

########
# 2019 #
########

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

