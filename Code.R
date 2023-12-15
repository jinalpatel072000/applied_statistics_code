
library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(generalhoslem)



setwd("C:/Users/jpate/Documents/Personal Documents/Applies Statistics")
getwd()


churn <- read.csv("customer_churn_data.csv")

glimpse(churn)
summary(churn)

sapply(churn, function(x) sum(is.na(x)))

churn[is.na(churn$TotalCharges),]

sum(is.na(churn$TotalCharges))/nrow(churn)

nrow(churn)

remove_clean <- na.omit(churn)
churn_clean <- na.omit(churn)
sapply(churn_clean, function(x) sum(is.na(x)))

# Summary for tenure
summary(churn_clean$tenure)

# Summary for monthly charges
summary(churn_clean$MonthlyCharges)

# Summary for total charges
summary(churn_clean$TotalCharges)

churn_clean$InternetService <- as.numeric(mapvalues(churn_clean$InternetService,from=c("No", "DSL","Fiber optic"),to=c("0", "1", "2")))
churn_clean$InternetService



glimpse(churn_clean)

describe(churn_clean$Churn)
describe(churn_clean$tenure)


glimpse(churn_clean)

str(churn_clean)
head(churn_clean)

Churn <- as.factor(churn_clean$Churn)

#----------------EDA--------------#

#1 contract type customer are less likely to churn

#Contract status plot
plotGraph  <- ggplot(churn_clean, aes(x = Contract)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot contract data within a grid
plot(plotGraph)


#2.	Is there a significant difference in monthly charges for customers with different tenure lengths? 


# Convert 'tenure' to a factor variable to represent different groups
churn_clean$tenure_group <- cut(churn_clean$tenure, breaks = c(0, 20, 40, 60, Inf), labels = c("0-20", "21-40", "41-60", "61+"))

# Perform ANOVA
anova_result <- aov(MonthlyCharges ~ tenure_group, data = churn_clean)

# Check the summary
summary(anova_result)

posthoc_result <- TukeyHSD(anova_result)

# Check the post-hoc test results
print(posthoc_result)

ggplot(churn_clean, aes(x = tenure_group, y = MonthlyCharges)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Monthly Charges Across Tenure Groups",
       x = "Tenure Group",
       y = "Monthly Charges") +
  theme_minimal()


#3.	Which type of internet service influence more on churn rate ? (EDA)

Graph <- ggplot(churn_clean, aes(x = InternetService)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

plot(Graph)


------------------------------------------------------------------------------------------------
  
  #----------------correlation analysis-------------#
  #Is there a correlation between the customer’s Contract type and churn rate?
churn_clean$Churn <- as.numeric(mapvalues(churn_clean$Churn,from=c("No", "Yes"),to=c("0", "1")))
churn_clean$Churn

churn_clean$Contract <- as.numeric(mapvalues(churn_clean$Contract,from=c("Month-to-month", "One year","Two year"),to=c("0", "1", "2")))
churn_clean$Contract

Correlation_matrix <- cor(churn_clean[,c("Contract","Churn")])
corrplot(Correlation_matrix,method = "color")
summary(Correlation_matrix)

churn_correlation_result <- cor.test(churn_clean$Contract,churn_clean$Churn)
print(churn_correlation_result)

churn_clean %>%
  dplyr::select (Contract, MonthlyCharges, Churn) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)


-------------------------------------------------------------------
  
  #-----------------Hypothesis  testing---------------------#
#6.	Customers who have been with the company for a longer time are less likely to leave,
  #Compared to clients who have been with the firm for a shorter time  
head(churn_clean)

hypertension_group <- churn_clean$tenure[churn_clean$Churn == "1"]
no_hypertension_group <- churn_clean$tenure[churn_clean$Churn == "0"]

# Perform t-test
t.testing <- t.test(hypertension_group, no_hypertension_group)
print(t.testing)


ggplot(churn_clean, aes(x = as.factor(Churn), y = tenure, fill = as.factor(Churn))) +
  geom_boxplot() +
  labs(title = "Tenure by Churn ",
       x = "Churn",
       y = "tenure") +
  theme_minimal()



----------------------------------------------------------------------------------------
  #-------------------Linear Regression-------------#
  #8.	Is there any linear relationship between Monthly service Charges 
  #and the total service charges that Customer are paying

  # Perform linear regression
  regression_model <- lm(TotalCharges ~ MonthlyCharges + tenure, data = churn_clean)

# Summary of the regression model
summary(regression_model)
#MonthlyCharges <- as.numeric(churn_clean$MonthlyCharges)
# Plotting the regression line

ggplot(churn_clean, aes(x = MonthlyCharges, y = TotalCharges, color = tenure)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: MonthlyCharges vs. TotalCharges",
       x = "MonthlyCharges",
       y = "TotalCharges") +
  theme_minimal()
  