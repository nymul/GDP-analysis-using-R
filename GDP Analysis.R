# Load necessary libraries
library(readr)
library(dplyr)
install.packages("readxl")
library(readxl)
library(ggplot2)
library(lubridate)
library(stats)
library(tidyverse)
library(stargazer)
library(ggplot2)



#CLEAR CONSOLE & ENVIROMENT
rm(list = ls()) # CLEAR ENVIROMENT

# Load the datasets
dataset1 <- read_excel("C:/Users/nymul/OneDrive/Desktop/gdp_analysis.xls")


view(dataset1)


# Apply log transformations
dataset1$log_gdp <- log(dataset1$GDP)
dataset1$log_Nonfarm_payroll <- log(dataset1$Nonfarm_payroll)
dataset1$log_unemployment_rate <- log(dataset1$Unemployment_Rate)
dataset1$log_Housing_data <- log(dataset1$Housing_data)
dataset1$log_Manufacturing <- log(dataset1$Manufacturing)
dataset1$log_Personal_Consumption_Expenditures <- log(dataset1$Personal_Consumption_Expenditures)




# Run linear regression


model1 <- lm(log_gdp ~ log_Nonfarm_payroll,
            data = dataset1)


model2 <- lm(log_gdp ~ log_Nonfarm_payroll + log_unemployment_rate,
             data = dataset1)


model3 <- lm(log_gdp ~ log_Nonfarm_payroll + log_unemployment_rate + 
            log_Housing_data,
             data = dataset1)


model4 <- lm(log_gdp ~ log_Nonfarm_payroll + log_unemployment_rate + 
            log_Housing_data + log_Manufacturing,
             data = dataset1)

model5 <- lm(log_gdp ~ log_Nonfarm_payroll + log_unemployment_rate + 
            log_Housing_data + log_Manufacturing + log_Personal_Consumption_Expenditures,
             data = dataset1)





# Display regression summary
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)


stargazer(model1, model2, model3, model4, model5,
          type = "text",
          covariate.labels = c("Nonfarm Payroll", "Unemployment Rate", "New Housing Units", "Manufacturing", "Personal Consumption Expenditures"),
          title = "Linear Regression Model with Log Transformations",
          out = "gdp_linear_regression_results.txt")







# Plot for GDP
ggplot(dataset1, aes(x = date)) +
    geom_line(aes(y = GDP, color = "GDP"), size = 1) +
#  geom_line(aes(y = Personal_Consumption_Expenditures, color = "Personal Consumption Expenditures"), size = 1) +
  #  geom_line(aes(y = log_college_graduate, color = "College Graduate"), size = 1) +
  #  geom_line(aes(y = log_employment_rate, color = "Employment Rate"), size = 1) +
  #  geom_line(aes(y = log_health_insurance_consumption, color = "health insurance consumption"), size = 1) +
  #  geom_line(aes(y = log_healthcare_expenditure, color = "Healthcare Expenditure"), size = 1) +
  labs(title = "GDP In The U.S",
       x = "Date",
       y = "Values in Billions",
       color = "Variables") +
  theme_minimal()







# Plot for Personal Consumption Expenditures
ggplot(dataset1, aes(x = date)) +
    geom_line(aes(y = GDP, color = "GDP"), size = 1) +
  geom_line(aes(y = Personal_Consumption_Expenditures, color = "Personal Consumption Expenditures"), size = 1) +
  #  geom_line(aes(y = log_college_graduate, color = "College Graduate"), size = 1) +
  #  geom_line(aes(y = log_employment_rate, color = "Employment Rate"), size = 1) +
  #  geom_line(aes(y = log_health_insurance_consumption, color = "health insurance consumption"), size = 1) +
  #  geom_line(aes(y = log_healthcare_expenditure, color = "Healthcare Expenditure"), size = 1) +
  labs(title = "Personal Consumption Expenditures In The U.S",
       x = "Date",
       y = "Values in Billions",
       color = "Variables") +
  theme_minimal()



# Plot for Unemployment rate
ggplot(dataset1, aes(x = date)) +
    geom_line(aes(y = log_gdp, color = "GDP"), size = 1) +
  geom_line(aes(y = Unemployment_Rate, color = "Unemployment Rate"), size = 1) +
  #  geom_line(aes(y = log_college_graduate, color = "College Graduate"), size = 1) +
  #  geom_line(aes(y = log_employment_rate, color = "Employment Rate"), size = 1) +
  #  geom_line(aes(y = log_health_insurance_consumption, color = "health insurance consumption"), size = 1) +
  #  geom_line(aes(y = log_healthcare_expenditure, color = "Healthcare Expenditure"), size = 1) +
  labs(title = "Unemployment Rate In The U.S",
       x = "Date",
       y = "% change in unemployment",
       color = "Variables") +
  theme_minimal()



# Plot for Total NON-FARM PAYROLL
ggplot(dataset1, aes(x = date)) +
    geom_line(aes(y = log_gdp, color = "GDP"), size = 1) +
  geom_line(aes(y = log_Nonfarm_payroll, color = "Payroll"), size = 1) +
  #  geom_line(aes(y = log_college_graduate, color = "College Graduate"), size = 1) +
  #  geom_line(aes(y = log_employment_rate, color = "Employment Rate"), size = 1) +
  #  geom_line(aes(y = log_health_insurance_consumption, color = "health insurance consumption"), size = 1) +
  #  geom_line(aes(y = log_healthcare_expenditure, color = "Healthcare Expenditure"), size = 1) +
  labs(title = "Logged NON-FARM PAYROLL-GDP IN THE U.S",
       x = "Date",
       y = "Values",
       color = "Variables") +
  theme_minimal()





# Plot for New Housing Units
ggplot(dataset1, aes(x = date)) +
    geom_line(aes(y = log_gdp, color = "GDP"), size = 1) +
  geom_line(aes(y = log_Housing_data, color = "Housing Data"), size = 1) +
  #  geom_line(aes(y = log_college_graduate, color = "College Graduate"), size = 1) +
  #  geom_line(aes(y = log_employment_rate, color = "Employment Rate"), size = 1) +
  #  geom_line(aes(y = log_health_insurance_consumption, color = "health insurance consumption"), size = 1) +
  #  geom_line(aes(y = log_healthcare_expenditure, color = "Healthcare Expenditure"), size = 1) +
  labs(title = "New Privately-Owned Housing Units and GDP (LOGGED)",
       x = "Date",
       y = "Logged Values",
       color = "Variables") +
  theme_minimal()



# Plot for Employees in Manufacturing
ggplot(dataset1, aes(x = date)) +
    geom_line(aes(y = log_gdp, color = "GDP"), size = 1) +
  geom_line(aes(y = log_Manufacturing, color = "Manufacturing"), size = 1) +
  #  geom_line(aes(y = log_college_graduate, color = "College Graduate"), size = 1) +
  #  geom_line(aes(y = log_employment_rate, color = "Employment Rate"), size = 1) +
  #  geom_line(aes(y = log_health_insurance_consumption, color = "health insurance consumption"), size = 1) +
  #  geom_line(aes(y = log_healthcare_expenditure, color = "Healthcare Expenditure"), size = 1) +
  labs(title = "Number of Employees in Manufacturing in the U.S and GDP (LOGGED)",
       x = "Date",
       y = "Logged Values of GDP AND Manufacturing",
       color = "Variables") +
  theme_minimal()