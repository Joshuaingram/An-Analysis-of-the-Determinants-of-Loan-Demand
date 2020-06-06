# An Analysis of the Determinants of Loan Demand

**-- Project Status: Version 1 Completed**

## Project Introduction
This project and paper was done for the completion of the "Econometrics" class at New College of Florida in Spring 2020 (sophomore year). The motivation of this project is the find the determinants of the demand for loans, as well as to observe any changes in the demand for loans from the Great Recession in 2008 to  before the Great Lockdown in 2019. Understanding the determinants of loan demand and how they change in a period of economic growth are useful to understand the behavior of borrowers and can be useful in price setting and marketing for lenders.

### Methods Used

- Data Visualization
- Multiple Linear Regression
- Two-Stage Least Squares

### Language

- R

## Project Description

### LendingClub

LendingClub is the worlds largest peer-to-peer lending platform that launched as one of Facebook’s ﬁrst applications in 2007. LendingClub has proven itself to be rather successful, with over 2.8 million loans listed on the platform since its beginning and over $650 million in revenue in 2019. The platform acts as a market where lenders and loan-seekers come together to make transactions directly, entirely removing the need for banks to be the intermediaries. This generally gives borrowers the opportunity to ﬁnd lower interest rates on a loan than what they would pay elsewhere. Users on the platform clearly take advantage of this opportunity because according to LendingClub, 67% of borrowers on the platoform use these loans to reﬁnance existing loans or to pay oﬀ their credit cards as of March 2020.

Currently, borrowers are able to request a maximum loan of $40,000, but the limit was $20,000 up until 2016. When borrowers decide to request a loan, they supply several details about themselves and the loan. These details include their FICO credit score, reason for the loan, employment information, and many more details. Once these details are received, LendingClub provides a loan grade and subgrade that determines the interest rate to be paid and that acts as a quick way for lenders to evaluate the riskiness of the loan. Once things are processed, the loan is listed on the platform and lenders are able to look at the details provided by the borrower to determine if they would like to fund the loan.

### The Data

You can find the original dataset from LendingClub that was used here: [LendingClub Loan Data](https://www.lendingclub.com/info/statistics.action)

The data used for the analysis performed in this paper was obtained directly from the LendingClub website. It contains details provided by the borrower and by LendingClub on every single loan listed on the platform since 2007. The raw data is comprised of 21 diﬀerent datasets separated by year or quarter beginning in 2007 and ending in the ﬁrst quarter of 2020. Once combined, there were 2.88 million loans with 150 variables for each loan, as well as an accompanying data dictionary. Before any data cleaning, 23.74% of the data was missing. Once the data was subsetted to include only our variables of interest, 0.3% of the data was missing. Due to the size of the data and the small percentage of missing values, any rows containing an NA value were outright removed. Two subsets of the data were made to be used in our analysis, one being a subset of all loans in 2008 and another of all the loans in 2019.

Additionally, any observations with reported annual incomes over $200,000 were removed from the data due to these being a small number of cases that had incredible eﬀects on the assumptions for the models used. There were also three new categorical variables created from the existing data. One being “region”, which is the region of where the address given by the borrower is located, another being “emp_length_cat”, which indicates which of the several employment length categories the applicant falls into. Finally, “real_estate_col”was created and it is a binary variable that indicates whether the borrower has some form of real estate collateral or not. Below is a table of variable definitions: 

![Variable Definitions](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/variable_defs.PNG?raw=true)

### Data Summaries

![Loan Amount Density](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/dens_amount.PNG?raw=true)

![Loan Grade Distribution](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/dens_grades.PNG?raw=true)

## Results

The forms the models used were based off of theoretical models and other empirical studies of loan supply and demand. We used two-stage least squares to estimate the demand for loans. The variables used to estimate the interest rate in the first stage were both supply side and demand side variables. In the second stage model, we estimated the demand function for loans. This was done for both 2008 and 2019.

### 2008

**First-Stage Output**

![First-Stage Estimates 2008](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/first_stage_08.PNG?raw=true)

**Second-Stage Output**

![Second-Stage Estimates 2008](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/second_stage_08.PNG?raw=true)

![Income Effect 2008](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/inc_08.PNG?raw=true)

### 2019

**First-Stage Output**

![First-Stage Estimates 2019](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/first_stage_19.PNG?raw=true)

**Second-Stage Output**

![Second-Stage Estimates 2019](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/second_stage_19.PNG?raw=true)

![Income Effect 2019](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/inc_19.PNG?raw=true)

### Comparison

Both the 2008 and 2019 ﬁnal demand models had the same form and were both signiﬁcant, but the eﬀects of the individual explanatory variables were diﬀerent. Additionally and most importantly, the data seemed to have a structural change between 2008 and 2019. The 2019 models are not reliable due to the homoscedasticity assumption being broken, but we can still move forward with comparison if this is taken into consideration. 

For both years, we found that the explanatory variables had a non-linear eﬀect on the demand for loans. However, their magnitudes and signiﬁcance levels were diﬀerent. In 2008, we saw that for every one percentage point increase in the interest rate, the demanded loan amount decreased by a factor of e0.03. Whereas in 2019, the demanded loan amount decreased by a factor of e0.01. The form of our equations also allow us to easily observe the price elasticity of demand for loans. The price elasticity of demand in 2008 is 0.03 and in 2019 it is 0.01. This means the demand for loans is very price inelastic in both years, though it is even more inelastic in 2019. 

We also found that the relationship between loan demand and income had a quadratic relationship in both 2008 and 2019. Once we take into account the diﬀerence in the loan limits between the two years, the rate at which income eﬀects demand seems to be quite similar, though the threshold at which demand begins to decreases because of a high income is diﬀerent. This annual income threshold is greater in 2019 than in 2008. 

Finally, the signiﬁcance of the categorical predictors were diﬀernt in 2008 than in 2019. However, it cannot be said with conﬁdence due to the instability of the 2019 model. The 2008 model does a noticeably better job at predicting the loan demand, withan R^2 of 0.1954 as compared to an R^2 of 0.149 for the 2019 model. Due to the structural change in the data and the diﬀerence in the magnitudes of effects on loan demand, there is ano bvious change in the market for loans on the LendingClub platform from during the Great Recession in 2008 to the pre-Great Lockdown in 2019. Below is a visualization of the non-linear demand curves in 2008 and 2019 that shows the relationships between interest rate and the loan demand, ceteris paribus

![Demand Curves](https://github.com/Joshuaingram/An-Analysis-of-the-Determinants-of-Loan-Demand/blob/master/images/demand%20curves.PNG?raw=true)

## Conclusion

The results of the analysis are not concrete due to the need of better methods for the 2019 model and to take into account extremely high income individuals (greater than $200,000 per year), but we do observe a non-linear loan demand curve. Additionally, we ﬁnd the expected effect of an increase in interest rate to decrease loan demand for both time periods. We also found a quadratic relationship of income with loan demand. Above all else, we clearly observe a structural change in the market for loans on the LendingClub platform from 2008 to 2019. 

The ﬁndings from this project can be useful in understanding the relationship that interest rate and income have with demand in the market for loans. For this research to move forward, a new method to estimate the demand for loans needs to be used that will address the structural form of the data. This includes taking care of the issue of heteroscedasticity in the 2019 model, as well as including observations with very high annual incomes. Further research with this data could be conducted on the diﬀerences in the demand for loans for diﬀerent income brackets and the different motivations for taking out a loan, as well as studying the factors that aﬀect loan grades and subgrades. One could further investigate the default rates of LendingClub loans, as well as the change in demand for loans after the borrowers are less credit constrained after 2016 on the platform.