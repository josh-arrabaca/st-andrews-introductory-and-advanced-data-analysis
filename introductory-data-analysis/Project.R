# MT5762 Project
# Joshua Arrabaca (ID: 220029955)

# Load libraries
library(readr)
library(tidyverse)
library(Hmisc)
library(epitools)
library(pwr)

# Load the dataset
sales_data <- read_csv("sales_data.csv")

# Change month column to a factor
sales_data <- sales_data %>%
  mutate(month_name = factor(month_name, 
                             levels = c("Jan", "Feb", "Mar",
                                        "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep")))

# Add a column for total sales (icecream_sales + hotdrink_sales)
sales_data <- sales_data %>%
  add_column(total_sales = sales_data$hotdrink_sales + sales_data$icecream_sales)


# Part 1 - Exploratory Data Analysis

# Pairs plots to examine possible correlations
pairs(sales_data)

# Scatterplot ice cream sales vs hot drink sales
plt.ice.hot <- ggplot(data=sales_data, 
                      aes(x=hotdrink_sales, y=icecream_sales)) +
  geom_point(colour="#636363", fill= "#d95f02", pch=21, size = 3) +
  xlab("Hot Drink Sales") + ylab("Ice Cream Sales") +
  ggtitle("Hot Drink Sales vs Ice Cream Sales")

plt.ice.hot

# Scatterplot ice cream sales vs temperature, with school holidays as the color fill
plt.ice.schholi <- ggplot(data=sales_data, 
                          aes(x=temperature, 
                              y=icecream_sales, 
                              fill = factor(school_holidays))) +
  geom_point(colour="#636363",pch=21, size=3) +
  scale_fill_manual(values = c("#7570b3", "#d95f02")) +
  xlab("temperature") + ylab("Ice Cream Sales") + 
  labs(fill = "Holiday (1) and \nNon-Holiday (0)") +
  ggtitle("Ice Cream Sales vs Temperature on Holidays and Non-Holidays")

plt.ice.schholi

# Find the means of sales on holidays vs non-holidays
holi.means <- sales_data %>% group_by(school_holidays) %>%
  summarise("Holiday Means" = round(mean(total_sales), 2))

holi.means

# Boxplot of total sales on holidays vs non-holidays  
plt.total.box.holi <- ggplot (sales_data) +
  geom_boxplot (aes(x = factor(school_holidays),
                    y = total_sales, 
                    fill = factor(school_holidays))) +
  scale_fill_manual(values = c("#7570b3", "#d95f02")) +
  xlab("Non-Holiday vs Holiday") + ylab("Total Sales") + 
  labs(fill = "Non-Holiday (0)\nHoliday (1)") +
  ggtitle("Average Total Sales on Holidays vs Non-Holidays")

plt.total.box.holi 

# Scatterplot of total sales vs temperature, with weekends/weekdays as the color fill
plt.total.wkend <- ggplot(data=sales_data, 
                          aes(x=temperature, 
                              y=total_sales, 
                              fill = factor(weekend))) +
  geom_point(colour="#636363", pch=21, size=3) +
  scale_fill_manual(values = c("#7570b3", "#d95f02")) +
  xlab("Temperature") + ylab("Total Sales") + 
  labs(fill = "Weekday (0)\nWeekend (1)") +
  ggtitle("Total Sales vs Temperature on Weekdays and Weekends")

plt.total.wkend

# Boxplot of total sales on weekends vs weekdays
plt.weekend <- ggplot (sales_data) +
  geom_boxplot (aes(x = factor(weekend),
                    y = total_sales, 
                    fill = factor(weekend))) +
  scale_fill_manual(values = c("#7570b3", "#d95f02")) +
  xlab("Weekday vs Weekend") + ylab("Total Sales") + 
  labs(fill = "Weekday (0)\nWeekend (1)") +
  ggtitle("Average Total Sales on Weekdays vs Weekends")

plt.weekend

# Compute for the mean total sales of weekdays vs weekends
weekend.means <- sales_data %>% group_by(weekend) %>%
  summarise("Mean Count" = round(mean(total_sales), 2))

weekend.means

# Boxplot of total sales per month
month.plot <- ggplot(sales_data) +
  geom_boxplot(aes (x=month_name, y=total_sales), 
               colour="#636363",
               fill = "#7570b3" ) +
  xlab ("Month") + ylab("Total Sales") +
  ggtitle("Monthly Total Sales")

month.plot

# Compute for the means per month
month.means <- sales_data %>% group_by(month_name) %>%
  summarise("Mean Count" = round(mean(total_sales), 2))

month.means

# Part 2 - Proportions and Odds Rations
# Q: the expected proportion of days with fewer than 200 ice cream sales and a 95% confidence interval

# Compute for the aggregate ice cream sales with < 200
icecream.sales.less <- sum (sales_data$icecream_sales < 200)
icecream.sales.greater <- sum (sales_data$icecream_sales >= 200)
icecream.sales.total <- length (sales_data$icecream_sales)

# Use binconf to compute for the odds ratio
binconf(x=icecream.sales.less, n=(icecream.sales.total), alpha=0.05, method="wilson") |>   round(3)


# Q:the expected proportion of days with fewer than 200 total sales (ice cream and hot drinks) and a 95% confidence interval

# Compute for the aggregate total sales with < 200
total.sales.less <- sum (sales_data$total_sales < 200)
total.sales.greater <- sum (sales_data$total_sales >= 200)
total.sales.total <- length (sales_data$total_sales)

# Use binconf to compute for the odds ratio
binconf(x=total.sales.less, n=(total.sales.total), alpha=0.05, method="wilson") |>   round(3)

# Q: the odds ratio for a purchase being an ice cream rather than a hot drink in January and in August and a 95% confidence interval for each.

# Find the sales per product type for January and August 
jan.icecream <- sum(filter (sales_data, month_name == "Jan")$icecream_sales)
jan.hotdrink <- sum(filter (sales_data, month_name == "Jan")$hotdrink_sales)
aug.icecream <- sum(filter (sales_data, month_name == "Aug")$icecream_sales)
aug.hotdrink <- sum(filter (sales_data, month_name == "Aug")$hotdrink_sales)

# Create object containing the number of the above
counts.jan <- c(jan.icecream, aug.icecream, jan.hotdrink, aug.hotdrink)
counts.aug <- c(aug.icecream, jan.icecream, aug.hotdrink, jan.hotdrink)

# Convert to a matrix
matcounts.jan <- matrix(counts.jan, nrow=2, byrow=TRUE)
matcounts.aug <- matrix(counts.aug, nrow=2, byrow=TRUE)

# Compute for the odds ratio for January
jan.OR <- oddsratio(matcounts.jan, method='wald')
jan.OR

# Compute for the odds ratio for August
aug.OR <- oddsratio(matcounts.aug, method='wald')
aug.OR

# Add row and column names when viewing
dimnames(matcounts.jan) <- list("Food"=c("IceCream","HotDrink"), "Month"=c("Jan","Aug"))
dimnames(matcounts.aug) <- list("Food"=c("IceCream","HotDrink"), "Month"=c("Aug","Jan"))

# Q whether there is a significant difference in odds ratios between January and August.
# Since the 95% CI do not overlap, we can deduce that the odds ratios are statistically different.


## Part 3 - Power

# Q: Test whether there is a difference between the expected number of sales on week days (Mon-Fri) and weekends. Interpret and explain your results.

# Find the means, lengths, and SD for weekend vs weekday, and compute for the mean sales on weekends vs weekdays
sales.weekday <- (filter (sales_data, weekend == 0)$icecream_sales)
sales.weekend <- (filter (sales_data, weekend == 1)$icecream_sales)

len1 = length(sales.weekday)
len2 = length(sales.weekend)

mean1 = mean(sales.weekday)
mean2 = mean(sales.weekend)

sd1 = sd(sales.weekday)
sd2 = sd(sales.weekend)

# Q: Test whether there is a difference between the expected number of sales on week days (Mon-Fri) and weekends. Interpret and explain your results.
# Do an F-test
t <- t.test(x = sales.weekday, y = sales.weekend)
t

# Compute for the effect size (Cohen's d)
cohens.d = (mean2 - mean1)/sqrt(((len1-1)*(sd1^2) + (len2-1)*(sd2^2))/
                                  (len1+len2-2))
cohens.d           

# Q: Compute the power of the above test, assuming that the true difference is the one observed.
pwr.t2n.test (n1 = len1, n2 = len2, d = cohens.d, sig.level = 0.05, alternative = "greater")

# Q: For the observed sample size, what effect size (i.e., difference between the expected values) would be required to obtain a power of 90%?
pwr.t2n.test (n1 = len1, n2 = len2, power =0.90, sig.level = 0.05, alternative = "greater")

# For the given effect size, what sample size would be required to obtain a power of 90%?
pwr.t.test (d = 0.5806339, sig.level = 0.05, power= 0.90, alternative = "greater")
 

# Part 4 - Linear Model

# Create a linear model with ice cream sales as the response variable
model <- lm(icecream_sales ~ 
              temperature + 
              humidity + 
              windspeed + 
              weekend + 
              bank_holiday + 
              school_holidays + 
              month_name, 
            data = sales_data)

# View the model
summary(model)

# View the coefficients only
model$coefficients |> round(3) |> data.frame()
#specify confidence interval

# Predict: a weekday in May with temperature 18°C, 6% humidity, and 10 km/h windspeed, 6% humidity, and 10 km/h windspeed
df.a <- data.frame(temperature = 18,
                   humidity = 6,
                   windspeed = 10,
                   weekend = 0,
                   bank_holiday = 0,
                   school_holidays = 0,
                   month_name = "May")

pred.a <- predict(model, newdata = df.a, interval = "confidence", level = 0.95)
pred.a |> round(2)

# Predict: For a school holiday on a weekend in April with temperature 28°C, 35% humidity, and 5 km/h windspeed, the predictions are:
df.b <- data.frame(temperature = 28,
                   humidity = 35,
                   windspeed = 5,
                   weekend = 1,
                   bank_holiday = 0,
                   school_holidays = 1,
                   month_name = "Apr")

pred.b <- predict(model, newdata = df.b, interval = "confidence", level = 0.95)
pred.b |> round(2)

# Predict: For a week day in September with temperature 12°C, 90% humidity, and 35 km/h windspeed: 
df.c <- data.frame(temperature = 12,
                   humidity = 90,
                   windspeed = 35,
                   weekend = 0,
                   bank_holiday = 0,
                   school_holidays = 0,
                   month_name = "Sep")

pred.c <- predict(model, newdata = df.c, interval = "confidence", level = 0.95)
pred.c |> round(2)

# Predict:  a day on a January weekend that is not a holiday with temperature -2°C, 75% humidity, and 15 km/h windspeed:
df.d <- data.frame(temperature = -2,
                   humidity = 75,
                   windspeed = 15,
                   weekend = 1,
                   bank_holiday = 0,
                   school_holidays = 0,
                   month_name = "Jan")

pred.d <- predict(object = model,
                  newdata = df.d,
                  interval = "confidence",
                  level = 0.95)
pred.d |> round(2)



# Discussion
# View the residuals plots
model
plot(model)

# Create a histogram of the residuals
residuals.plot <- ggplot() +
  geom_histogram(aes(x = model$residuals), 
                 fill = '#d95f02', 
                 color = "#636363") +
  xlab("Residuals") + ylab("Frequency") +
  ggtitle("Histogram of Residuals for the Ice Cream Linear Model") 

residuals.plot 
