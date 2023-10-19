## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.

# Install the tidyverse library.
# install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse)

# Import the data set.
getwd()
setwd("C:/Users/basak/OneDrive/Desktop/LSE Content/Course 3/Course 3 Assignment")
ts <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
head(ts)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
ts2 <- select(ts, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(ts2)

# View the descriptive statistics.

# Convert data frame to a tibble.
as_tibble(ts2)

# Use the glimpse() function.
glimpse(ts2)

# Use the summary() function.
summary(ts2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Check global sales per product.
qplot(Product,
      Global_Sales,
      colour=Platform,
      data=ts2,
      main='Global Sales of Products')

# Check global sales per platform.
qplot(Global_Sales,
      Platform,
      data=ts2,
      geom=c('point', 'jitter'),
      main='Global Sales per Platform')

# Compare EU vs NA sales.
qplot(EU_Sales,
      NA_Sales,
      colour=Platform,
      data=ts2,
      xlim=c(0, 40),
      ylim=c(0, 40),
      main='EU vs NA Sales')

# Check platforms of products by adding jitter to the scatterplot.
qplot(Product,
      Platform,
      colour=Global_Sales,
      data=ts2,
      geom=c('point', 'jitter'),
      main='Product Platforms')

## 2b) Histograms
# Create histograms.

qplot(NA_Sales,
      data=ts2,
      geom='histogram',
      binwidth=10,
      main='NA Sales')

qplot(EU_Sales,
      data=ts2,
      geom='histogram',
      binwidth=5,
      main='EU Sales')

qplot(Global_Sales,
      data=ts2,
      geom='histogram',
      binwidth=10,
      main='Global Sales')

qplot(Platform,
      data=ts2,
      main='Product Counts per Platform')

## 2c) Boxplots
# Create boxplots.

qplot(NA_Sales,
      Platform,
      data=ts2,
      geom='boxplot',
      xlim=c(0,40),
      main='NA Sales per Platform')

qplot(EU_Sales,
      Platform,
      data=ts2,
      geom='boxplot',
      xlim=c(0,40),
      main='EU Sales per Platform')

qplot(Global_Sales,
      Platform,
      data=ts2,
      geom='boxplot',
      xlim=c(0,40),
      main='Global Sales per Platform')

###############################################################################

# 3. Observations and insights

# - Global sales for products with smaller product numbers are higher compared
#   to those with larger product numbers.
# - There is a single Wii platform product (product 107) that is clearly an
#   outlier with a very high global sale number (67.8).
# - PC, GC and XOne platforms have many products with low sales numbers.
# - NA sales make up most of the global sales numbers.
# - X360, PS3, PC, Wii and DS platforms have the most number of products.
# - 2600, GEN and PSV platforms have the least number of products.
# - SNES platform products have the highest mean sales globally followed by Wii,
#   NES and DS.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(ts)

# Check output: Determine the min, max, and mean values using the skim function.
library(skimr)
skim(ts)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

# Using aggregate()
aggregate(Global_Sales~Product, ts, sum)

# Using group_by()
library(dplyr)

# Group by Product and Platform and calculate sum of sales
ts_sales <- ts %>% 
  group_by(Product, Platform) %>%
  summarise(sum_global=sum(Global_Sales), 
            sum_eu=sum(EU_Sales), 
            sum_na=sum(NA_Sales),
            .groups='drop')

# Group by only Product and calculate sum of sales
ts_product_sales <- ts %>% 
  group_by(Product) %>%
  summarise(sum_global=sum(Global_Sales), 
            sum_eu=sum(EU_Sales), 
            sum_na=sum(NA_Sales),
            .groups='drop')

# Group by only Platform and calculate sum of sales and product counts
ts_platform_sales <- ts %>% 
  group_by(Platform) %>%
  summarise(sum_global=sum(Global_Sales), 
            sum_eu=sum(EU_Sales), 
            sum_na=sum(NA_Sales),
            count_product=n(),
            .groups='drop')

# View the data frame.
head(ts_product_sales)

# Explore the data frame.
skim(ts_product_sales)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

ggplot(data=ts_sales, 
       mapping=aes(x=Product, y=sum_global, color=Platform)) +
       geom_point() +
       labs(title='Total Global Sales of Products by Platform',
            y='Total Global Sales') +
       theme_bw()

ggplot(data=ts_product_sales, 
       mapping=aes(x=Product, y=sum_global)) +
  geom_point() +
  labs(title='Total Global Sales per Product',
       y='Total Global Sales') +
  theme_bw()

ggplot(data=ts_platform_sales, 
       mapping=aes(x=sum_global, y=Platform)) +
  geom_point() +
  labs(title='Total Global Sales per Platform',
       x='Total Global Sales') +
  theme_bw()

ggplot(data=ts_platform_sales, 
       mapping=aes(x=sum_global, y=count_product, color=Platform)) +
  geom_point() +
  labs(title='Global Sales vs Product Count',
       x='Total Global Sales',
       y='Product Count') +
  theme_bw()

# Create bar charts.

ggplot(data=ts_platform_sales, 
       mapping=aes(x=Platform, y=sum_global)) +
  geom_bar(stat='identity') +
  labs(title='Total Global Sales per Platform',
       x='Total Global Sales') +
  theme_bw()

# Create histograms.

ggplot(data=ts_product_sales, 
       mapping=aes(x=sum_global)) +
  geom_histogram(binwidth=5) +
  labs(title='Product Global Sales',
       x='Total Global Sales',
       y='Count of Product') +
  theme_bw()

ggplot(data=ts_product_sales, 
       mapping=aes(x=sum_eu)) +
  geom_histogram(binwidth=5) +
  labs(title='Product EU Sales',
       x='Total EU Sales',
       y='Count of Product') +
  theme_bw()

ggplot(data=ts_product_sales, 
       mapping=aes(x=sum_na)) +
  geom_histogram(binwidth=5) +
  labs(title='Product EU Sales',
       x='Total NA Sales',
       y='Count of Product') +
  theme_bw()

# Create boxplots.

ggplot(data=ts2, 
       mapping=aes(x=Global_Sales, y=Platform)) +
  geom_boxplot() +
  labs(title='Global Sales per Platform',
       x='Global Sales') +
  theme_bw()

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(ts2$Global_Sales)
qqline(ts2$Global_Sales) 

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
# install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(ts2$Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(ts2$Global_Sales) 
kurtosis(ts2$Global_Sales)

## 3d) Determine correlation
# Determine correlation.
ts_numeric = select(ts2, -Platform)
round(cor(ts_numeric), digits=2)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Check correlation of Product with other columns
ggplot(data=ts2, 
       mapping=aes(x=Product, y=EU_Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

ggplot(data=ts2, 
       mapping=aes(x=Product, y=NA_Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

ggplot(data=ts2, 
       mapping=aes(x=Product, y=Global_Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

# Check correlation of EU_Sales with other columns
ggplot(data=ts2, 
       mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

ggplot(data=ts2, 
       mapping=aes(x=EU_Sales, y=Global_Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

# Check correlation of NA_Sales with other columns
ggplot(data=ts2, 
       mapping=aes(x=NA_Sales, y=Global_Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

###############################################################################

# 5. Observations and insights

# Data that is +/- 1 standard deviation around the mean mostly follows normal
# distribution. Both ends of the tails deviate strongly from the normal
# distribution. The p-value of Shapiro-Wilk test is very low which also indicate
# that sales data is not following normal distribution but from the kurtosis
# test we can see that this is mostly due to the data having a very heavy-tail
# (32 compared to 3 of normal distribution).
# 
# EU_Sales and NA_Sales are heavily correlated with Global_Sales which makes
# sense because Global_Sales data contains them. Product column is unexpectedly
# correlated with the sales columns which doesn't make too much sense since it
# is an identifier. This suggest that the products might have been partially
# numbered based on their sales performance. EU_Sales and NA_Sales also have
# a significant correlation between them which suggests that sales performance
# of products across these continents are similar.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.
###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

model_gl_eu <- lm(sum_global ~ sum_eu, data=ts_product_sales)
summary(model_gl_eu)

model_gl_na <- lm(sum_global ~ sum_na, data=ts_product_sales)
summary(model_gl_na)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

plot(ts_product_sales$sum_eu, ts_product_sales$sum_global)
abline(coefficients(model_gl_eu))

qqnorm(residuals(model_gl_eu))
qqline(residuals(model_gl_eu), col='red')

plot(ts_product_sales$sum_na, ts_product_sales$sum_global)
abline(coefficients(model_gl_na))

qqnorm(residuals(model_gl_na))
qqline(residuals(model_gl_na), col='red')

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

model_gl_eu_na <- lm(sum_global ~ sum_eu + sum_na, data=ts_product_sales)

# Multiple linear regression model.
summary(model_gl_eu_na)

# Visualize residuals of the multiple linear regression model.
qqnorm(residuals(model_gl_eu_na))
qqline(residuals(model_gl_eu_na), col='blue')

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

sales_forecast <- data.frame(sum_eu = c(23.80, 1.56,  0.65, 0.97, 0.52),
                             sum_na = c(34.02, 3.93, 2.73, 2.26, 22.08))
predictions <- predict(model_gl_eu_na,
                       newdata=sales_forecast)
sales_forecast$sum_global <- predictions

# Show the predictions on a 3D scatterplot using plotly

# install.packages('plotly')
library(plotly)

fig <- plot_ly(type='scatter3d', mode='markers')
fig <- fig %>%
  add_trace(x = ts_product_sales$sum_na,
            y = ts_product_sales$sum_eu,
            z = ts_product_sales$sum_global,
            opacity = 0.3,
            marker = list(color = 'rgb(50, 100, 255)', size = 3),
            name = 'Actual'
  )
fig <- fig %>%
  add_trace(x = sales_forecast$sum_na,
            y = sales_forecast$sum_eu,
            z = sales_forecast$sum_global,
            opacity = 1.0,
            marker = list(color = 'rgb(255, 30, 30)', size = 5),
            name = 'Predicted'
  )
fig <- fig %>% 
  layout(scene = list(xaxis = list(title = 'NA Sales'),
                      yaxis = list(title = 'EU Sales'),
                      zaxis = list(title = 'Global Sales')))
fig

###############################################################################

# 5. Observations and insights
# 6.	EU and NA sales are heavily correlated with global sales, as expected. 
# A multiple linear regression model created using the global sales as the 
# dependent variable and North American and European sales as independent 
# variables can explain 96.64% of the variance in the dependent variable. 
# This is a very good fit and suggests that global sales can be predicted 
# reasonably well using only NA and EU sales data.



###############################################################################
###############################################################################




