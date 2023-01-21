#Your data is a random sample of 1,000 observations drawn from the NYC population.  Limit your analysis to people under age 30.
#1. Test the null hypothesis that Queens and the Bronx have the same variance of income using a 2-sided test and report your p-value.
#2. If you estimated a regression in which Income is the dependent variable and Education is the explanatory variable.  What would be your slope estimate?
#3. What would be the estimated intercept?
#4. If a person's years of education is 17 what would their predicted income be based on the regression estimates from (2) and (3)?

library(readxl)
New_York_City_Census <- read_excel("census_file.xls")
View(New_York_City_Census)

#1, We need to solve the t-statistic formula
income = 0
income_thirty = 0
counter_bronx = 0
for (i in 1:1000) { #finding the average income in Bronx
  if(New_York_City_Census$Age[i] < 30 & New_York_City_Census$Borough[i] == "Bronx") { 
    income = New_York_City_Census$Income[i] 
    counter_bronx = counter_bronx + 1}
  else {income = 0}
  income_thirty = income_thirty + income
}
income_thirty
counter_bronx
bronx_thirty = income_thirty/counter_bronx
bronx_thirty

total_income = 0
for (i in 1:1000) { #finding the sigma squared
  if(New_York_City_Census$Age[i] < 30 & New_York_City_Census$Borough[i] == "Bronx") { 
    income = New_York_City_Census$Income[i] - bronx_thirty
    total_income = income^2 + total_income}
}
mean_sigma_bronx = total_income
mean_sigma_bronx


income = 0
income_thirty = 0
counter_queens = 0
for (i in 1:1000) { #finding the average income in Queens
  if(New_York_City_Census$Age[i] < 30 & New_York_City_Census$Borough[i] == "Queens") { 
    income = New_York_City_Census$Income[i] 
    counter_queens = counter_queens + 1}
  else {income = 0}
  income_thirty = income_thirty + income
}
income_thirty
counter_queens
queens_thirty = income_thirty/counter_queens
queens_thirty

total_income = 0
for (i in 1:1000) {#finding the sigma squared
  if(New_York_City_Census$Age[i] < 30 & New_York_City_Census$Borough[i] == "Queens") { 
    income = New_York_City_Census$Income[i] - queens_thirty
    total_income = income^2 + total_income}
}
mean_sigma_queens = total_income
mean_sigma_queens

#We use he above results to solve the t-statistic formula, 
#substracting the two average and dividing that by the sum of the sigma and sqrt'ing that.
t_distribut = (queens_thirty - bronx_thirty) / sqrt(mean_sigma_bronx/counter_bronx + mean_sigma_queens/counter_queens)
t_distribut

approx_df_values = function(s2_1, n_1, s2_2, n_2){ #formula using for testing the hypothesis about Variance, formula being the z-statistic
  approx = (s2_1 / n_1 + s2_2 / n_2)^2 /
    (s2_1^2 / (n_1^2 * (n_1 - 1)) + s2_2^2 / (n_2^2 * (n_2 - 1)))
  return(approx)
}
df_value = approx_df_values(s2_1 = mean_sigma_bronx, n_1 = counter_bronx, s2_2 = mean_sigma_queens, n_2 = counter_queens)
df_value

2*(1-pt(t_distribut,df=df_value)) #used to return the probability cumulative density of the t distribution to get the p-value
#not able to reject the null hypothesis

##################################################################################################
#2
counter = 0
income_total = 0
for (i in 1:1000) {
  if(New_York_City_Census$Age[i] < 30) { 
    income = New_York_City_Census$Income[i] 
    counter = counter + 1}
  else {income = 0}
  income_total = income_total + income
}
income_average= income_total / counter
income_average


counter = 0
education_total = 0
for (i in 1:1000) {
  if(New_York_City_Census$Age[i] < 30) { 
    education = New_York_City_Census$Education[i] 
    counter = counter + 1}
  else {education = 0}
  education_total = education_total + education
}
education_average= education_total / counter
education_average

total_edu = 0
counter = 0
for (i in 1:1000) {
  if(New_York_City_Census$Age[i] < 30) { 
    education = New_York_City_Census$Education[i] - education_average
    counter = counter + 1}
  else {education = 0}
  total_edu = education^2 + total_edu
}
mean_sigma_edu = total_edu / (counter-1)
mean_sigma_edu

total_edu_income = 0
counter = 0
for (i in 1:1000) {
  if(New_York_City_Census$Age[i] < 30) { 
    education_income = (New_York_City_Census$Education[i] - education_average)*(New_York_City_Census$Income[i] - income_average)
    counter = counter + 1}
  else {education_income = 0}
  total_edu_income = total_edu_income + education_income
}
s_education_income = total_edu_income / (counter-1)
s_education_income
#2 answer
b_hat_one = s_education_income / mean_sigma_edu
b_hat_one
#3 answer
b_hat_zero = income_average - b_hat_one*education_average
b_hat_zero

#4
seventeen_year_education = b_hat_zero + b_hat_one*17
seventeen_year_education
