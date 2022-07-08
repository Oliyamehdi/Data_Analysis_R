########################################################################################################

# Assignment activity 6: Making recommendations to the business using Multiple Linear Regression

########################################################################################################

## Load the tidyverse library
library(tidyverse)
library(ggplot2)

########################################################################################################
## Import the data set
games <- read.csv(file.choose (), header = T)

# Create a new dataframe with only relavent columns
games1 = subset(games, select = c("NA_Sales", "EU_Sales","Global_Sales")) 

# View the daataframe
head(games1)

# Establish a correlation between variables

cor(games1)
# As seen there is a very high correlation between the variables of over 90% between EU_Sales and Global Sales and 

# Visualise correlation with a line-of-best- fit.
ggplot(games1,aes(x = NA_Sales, y = Global_Sales)) + geom_point() +geom_smooth(method = "lm")

ggplot(games1,aes(x = EU_Sales, y = Global_Sales)) + geom_point() +geom_smooth(method = "lm")

#As seen above there is positive linear relationship between the variables as if one goes up so does the other. 


# Fit Multiple Linear Regression Model 
model3 <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = games1)
model3
summary(model3)
# As seen in the summary the adjusted R-Suare of 96% suggests a good fit of the model 

## Create a new data frame for the assumed values
Global_forecast <- data.frame(NA_Sales=5000 , EU_Sales = 3500)
Global_forecast

## Predict for the next financial year
predictTest = predict(model3, newdata = Global_forecast, interval = 'confidence')
predictTest

#############################################################################

