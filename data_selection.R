########################################################################
# Data Cleaning for LendingClub Data for Ecnoometrics Research Project #
#                                                                      #
# Joshua D. Ingram                                                     #
#                                                                      #
# 5/18/2020                                                            #
########################################################################

# Packages to be used
library(tidyverse)
library(zoo)
library(RColorBrewer)
library(vcd)
library(plyr)
library(VGAM)
library(car)
library(knitr)

# To save time so I don't redo work, I'll be importing the clean dataset from my linear models class that I already worked on
# this removed .3% of the rows due to NAs, only contains our variables of interest, and contains some new variables I created from other pre-existing variables
LCD_clean <- read.csv("C:/main/datasets/LendingClub Loans/LCD_clean.csv")

# let's look at the data
head(LCD_clean)
summary(LCD_clean)

# looks like there are two observations with a dti of -1, so I'll remove those since that makes no sense
LCD <- LCD_clean[which(LCD_clean$dti >= 0),]

# Let's look at the distribution of annual income
ggplot(LCD[which(LCD$annual_inc >= 1000000),], aes(x=annual_inc)) + 
  geom_density(stat = "density", adjust = 3, alpha = .4, fill = "lightblue") + 
  labs(y = "Density", x = "Annual Income", title = "Distribution of Annual Income")
mean(LCD_clean$annual_inc)

# The disitribution of income is skewed to the right (lot's of middle-low income with a few thousand extremely high income individuals) 
# from previous analysis, these individuals making millions a year harmed our model. 
# I am going to create a neww varaible that indicates whether an individual makes more than $250,000 per year 
# If this doesn't help, I am going to outright remove these influential observations for the econometrics project but will work with them in my thesis
LCD <- mutate(LCD, high_inc = factor(case_when(annual_inc >= 200000 ~ 1,
                                               annual_inc < 200000 ~ 0)))

# changing issue_d to correct variable class (zoo)
LCD$issue_d <- as.yearmon(LCD$issue_d, "%b %Y")
summary(LCD)

# now that we have this new variable, let's subset our data into two years: 2008 and 2019
# 2008
LCD_2008 <- LCD[which(format(LCD$issue_d,"%Y") == "2008"),]

# 2019
LCD_2019 <- LCD[which(format(LCD$issue_d,"%y") =="2019"),]

# removing incomes of more than 250000
LCD_2008 <- LCD_2008[which(LCD_2008$annual_inc <= 250000),]
LCD_2019 <- LCD_2019[which(LCD_2019$annual_inc <= 250000),]

##########################################
# Model Fitting and Residual Diagnostics #
##########################################

########
# 2008 #
########

#########################################
# First Stage to estimate price

int_rate_hat_2008 <- lm(int_rate ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

############################
# Checking for Collinearity

cor(LCD_2008$dti, LCD_2008$fico_range_low)
cor(LCD_2008$dti, LCD_2008$annual_inc)
cor(LCD_2008$fico_range_low, LCD_2008$annual_inc)
vif(int_rate_hat_2008)

#############################
# Checking Model Assumptions

# non-linearity and non-constant variance... clearly have a non-linear relationship
plot(int_rate_hat_2008,1)

# normality assumption... not good
plot(int_rate_hat_2008,2)
plot(density(rstandard(int_rate_hat_2008)))

###############################
# Remedies to model assumptions

int_rate_hat_2008 <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)

# non-linearity and non-constant variance... clearly have a non-linear relationship
plot(int_rate_hat_2008,1)

# normality assumption... still not great but our sample is large
plot(int_rate_hat_2008,2)
plot(density(rstandard(int_rate_hat_2008)))

# perhaps some predictors have a non-linear relationship with price

plot(int_rate ~ dti, data = LCD_2008)
plot(int_rate ~ fico_range_low, data = LCD_2008)
plot(int_rate ~ annual_inc, data = LCD_2008[which(LCD_2008$annual_inc < 80000),])

######################
# Outliers

plot(int_rate_hat_2008,4)
cutoff <- 4/(nrow(LCD_2008) - (6+1))

#################################
# Model Accuracy and Significance

int_rate_hat_2008_final <- lm(log(int_rate) ~ dti + fico_range_low + real_estate_col + annual_inc + emp_length_cat + region, data = LCD_2008)
summary(int_rate_hat_2008_final)

####################################
# Creating Column of Price Estimates

int_rate_hat <- predict(int_rate_hat_2008_final)
int_rate_hat <- exp(int_rate_hat)

LCD_2008$int_rate_hat <- int_rate_hat

###################
# Demand Model

lm_08_q_1 <- lm(loan_amnt ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008)
summary(lm_08_q_1)

###########################
# Checking for Collinearity

# everything is fine
vif(lm_08_q_1)

##########################################
# Checking Model Assumptions and Outliers

# linearity and homoscedasticity... There are likely outliers
plot(lm_08_q_1,1)

# checking for outliers
plot(lm_08_q_1, 4)
cutoff <- cutoff <- 4/(nrow(LCD_2008_new) - (4+1))

# trying to fix this weird situation
lm_08_q_2 <- lm(loan_amnt ~ int_rate_hat + I(int_rate_hat^2) + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008)
summary(lm_08_q_2)

# linearity and homoscedasticity... There are likely outliers
plot(lm_08_q_1,1)

# We will move forward by removing these 4 outliers from our final model
lm_08_q_3 <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_new4)
summary(lm_08_q_3)

# model assumptions

# linearity and homoscedasticity... Variance is not good
plot(lm_08_q_3,1)

# normality... not great but the size is large so it's okay
plot(lm_08_q_3,2)
plot(density(rstandard(lm_08_q_3)))

################################
# Remedies to Model Assumptions

# ####### GO BACK AND REDO ALL OF THIS

##################################
# Model Accuracy and Significance

lm_08_q_final <- lm(loan_amnt ~ int_rate_hat + annual_inc + emp_length_cat + region + home_ownership, data = LCD_2008_new4)
summary(lm_08_q_final)
plot(lm_08_q_final,1)
plot(lm_08_q_final,2)
plot(density(rstandard(lm_08_q_final)))
conf <- confint(lm_08_q_final, level = 0.9)
coef2 <- summary(lm_08_q_final)$coef