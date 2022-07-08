####################################################################################

# Assignment activity 5: Clean, manipulate, and visualise the data

####################################################################################

# Install the libraries
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(DataExplorer)
library (moments)  
library(ggplot2)

####################################################################################

# Import your csv 
game <- read.csv(file.choose(), header = TRUE)

####################################################################################

# Review/sense-check the data set
tibble (game)
str(game)

# These functions provide summary statistics of the data set
summary(game)

# This creates a downloadable HTML file containing summary stats of the data set
DataExplorer::create_report(game)

# To search for sum of missing values in a data set
sum(is.na(game))

####################################################################################

#Change case of strings
game$Genre <- tolower(game$Genre)

head(game)

####################################################################################

# Univariate Analysis

# Most Popular Genre
ggplot (game, aes (x = Genre)) +
  geom_histogram (fill = "blue",
                  color = "black",
                  stat = "count") +   
  labs (x = "Genre",
        y = "Frequency",
        title = "Genre Distribution")

## From the histogram it can be seen that the 'Action' genre is the most popular genre followed by the 'Sports' genre

# # Most Popular Platform
ggplot (game, aes (x = Platform)) +
  geom_histogram (fill = "green",
                  color = "black",
                  stat = "count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs (x = "Platform",
        y = "Frequency",
        title = "Distribution of Platform")

## As seen in the histogram DS is the mos popular platform followed by PS2 and Wii



####################################################################################
# Create new dataframe aggregated by year

# Dataframe with aggreate of Global_Sales by year
games_new<-aggregate( Global_Sales~Year, game, sum)

#Dataframe with aggreate of NA_Sales by year
games_new_NA<-aggregate( NA_Sales~Year, game, sum)

#Dataframe with aggreate of EU_Sales by year
games_new_EU<-aggregate( EU_Sales~Year, game, sum)

# Dataframe with aggregate of Global_Sales, NA_Sales, EU_SAles
games_final <- mutate(games_new,games_new_NA,games_new_EU)

# View new dataframe
head(games_final)

####################################################################################

# Keeping the business question in mind of predicting global sales we will focus on understanding the NA_Sales, EU_Sales and Global_Sales variable.

cor(game$NA_Sales,game$EU_Sales)
cor(game$EU_Sales,game$Global_Sales)
cor(game$NA_Sales,game$Global_Sales)

## Scatterplot with a line-of-best-fit to show correlation
ggplot(game, aes(x =NA_Sales , y =Global_Sales )) + geom_point() + geom_smooth(method = lm)+
       labs(title = "Relationship between NA_Sales and Global_Sales")


## Scatterplot with a line-of-best-fit to show correlation
ggplot(game, aes(x =EU_Sales , y =Global_Sales )) + geom_point() + geom_smooth(method = lm)+
  labs(title = "Relationship between EU_Sales and Global_Sales")

# As seen with both NA_Sales and EU_Sales both are highly corelated to Global Sales with  90% and 94% respectively. 

####################################################################################################

## Skewness of Data
skewness(games_final$NA_Sales)
 
# Histogram to show skewness of data
ggplot(game, aes(x =NA_Sales )) + geom_histogram(bins = 50)+
  labs(title = "Histogram for NA_Sales")
 #As you can observe in the output of 0.84, skewness is less than 1 but greater than 0. 
 #This is positive skewness, which suggests that the distribution is right-skewed and biased towards higher values


skewness(games_final$EU_Sales) 

# Histogram to show skewness of data
ggplot(game, aes(x =EU_Sales )) + geom_histogram(bins = 50)+
  labs(title = "Histogram for EU_Sales")
#As you can observe in the output of 0.64, skewness is less than 1 but greater than 0. 
#This is positive skewness, which suggests that the distribution is right-skewed and biased towards higher values


####################################################################################################

       