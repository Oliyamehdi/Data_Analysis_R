#######################################################################################

# Assignment activity 4: Visualise data to gather insights

# Objective:

   ##Which age group submits the most reviews?
   ##What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?
  

#######################################################################################

# Install Packages and Import Libraries

install.packages("ggplot2")
install.packages("tidyverse")

library (tidyverse)
#######################################################################################

## Import the data set
lego <- read.csv(file.choose(),header = TRUE)

## Explore the data set
head(lego)

#Simplify the view to see datatype 
tibble(lego)

# Glimpse (for a transposed version)
glimpse(lego)

# Summary of the variables
summary(lego)

#######################################################################################

# Convert 'age' to factor (categorical variable)

lego2 <- mutate(lego,
                 ages= as.factor(ages))

# Check the new data frame with ages as a character variable
tibble(lego2)

# Summary of the variables
summary(lego2)

#######################################################################################

# Which age group submits the most reviews?

# Return a frequency table for the 'ages' column
table(lego2$ages)

## The frequency table suggests that 19 year old's are the highest consumers of the product.


## Plot a Bar graph
qplot(ages, data=lego2, geom="bar") + labs(y = "Count", 
                                           x = "Age",
                                           title = "Age of Customers")

# As visually seen 19 year olds are the highest consumers of the product followed by 23 year olds and 26 year olds

## Return a frequency table for the 'num_reviews' column
table(lego2$num_reviews)
## A large number of consumers choose not to give reviews with "0" reviews being the highest.

# Plot histogram to understand num_reviews distribution
qplot(num_reviews, bins = 30, data= lego2)+labs(y = "Frequency", 
                                                x = "Number of Reveiws",
                                                title = "Reveiw Distribution")
## As seen in the histogram consumers are not very forth comming in giving reviews as 0( people who do not give reviews) are the highest

# Create a Scatterplot to determine any relationship between two variables 
ggplot(lego2,
       mapping = aes(x = ages, y = num_reviews)) +
  geom_point(color = "purple",
             alpha = 0.75,
             size = 2.5)  +
  labs(title = "Relationship between age and number of reveiws",
      x= "Age",
      y ="Number of Reviews")

# As visible in the scatter plot the age group of 8 year olds give the most reviews followed by 7 year olds. 

## Which age group submits the most reviews?
 # Age group of 7-8 year old submit the most reviews with 8 year olds providing the highest number of reviews.
 # While 19 year old are the highest purchasers of the product they do not submit a lot of reviews with only about 50 reviews from that age group.

##############################################################################################

## What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?

# Create a new dataframe of customers over 25 years of age
lego_age25 <- lego[lego$age>=25,]

# Explore data set
head (lego_age25)

#Simplify the view
tibble(lego_age25)

# Dimesions of the dataframe
dim(lego_age25)

## Return a frequency table for the 'list_price' column
table(lego_age25$list_price)
# As seen in the frequency table $ 259.87 is the highest price.

# Plot histogram to understand list_price distribution
ggplot (lego_age25, aes (x = list_price)) +
   geom_histogram (fill = "red",
                   color = "blue",
                   stat = "count") +    
   labs (x = "Price",
         y = "Frequency",
         title = "Lego Price Distribution")
# The histogram suggests that most peices sold are priced below $100 for this age group.  


# Create a Scatterplot to determine any relationship between two variables
qplot(piece_count, list_price, data=lego_age25)

ggplot(lego_age25,
       mapping = aes(x =piece_count , y = list_price)) +
  geom_point(color = "green",
             alpha = 0.50,
             size = 1.5)  +
  labs(title = "Relationship between list_price and piece_count",
       x= "peice_count",
       y ="list_price")

 # As seen in the scatterplot there is a linear relationship between price and peice count. 
 # There are lego peices with moe than 2000 peices however the peice with the highest price seems to be below 1500 peice count. 


# Order observation in descending order to view highest price.
lego_age25 %>% arrange(desc(list_price))

## What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?
## The most expensive Lego set purchased by customers (who are at least 25 years) 
##was bought by a customer aged 29 years old priced at 259.87 with a piece count of 1413.

##################################################################################################







