#import the necessary libraries to clean the data2.

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

#Now we start by importing the data2set.

data<- read.csv("Amazon_Sale_Report.csv")
View(head(data))
data2<- clean_names(data)
View(head(data2, 10))
colnames(data2)

#checking the datatypes of every single columns
str(data2)

#Correcting the datatype of columns
class(data2$date)
data2$date<- mdy(data2$date)
data2$amount <- as.numeric(data2$amount)
str(data2)

#random checking
data2$ship_state <- tolower(data2$ship_state)
data2$ship_state <- str_to_title(data2$ship_state)

#checking which of the rows have NA values
sapply(data2, function(x) sum(is.na(x)))

#from this we can see that both the amount column and ship.postal.code column has Null values
#so we clear this by adding custom inputs as 0, and "missing"

data2$amount[is.na(data2$amount)] <- 0
data2$`ship_city`[is.na(data2$`ship_city`)] <- "Missing"
data2 <- data2[!is.na(data2$`ship_postal_code`),]

#checking for Null values
sapply(data2, function(x) sum(is.na(x)))

#No more null values now we answer the analysis questions.
#Some of the questions we are going to answer are 

#What is the average sales per month?
#Which market (state) generated the most sales on average?
#What were the profits by segment?
#Which products sell best? 
#When were the best- and worst-selling periods?

#What is the average sales per month?
data2 %>% 
  mutate(Month = format(as.Date(date, "%m-%d-%y"), "%m")) %>% 
  group_by(Month) %>%
  summarise(Avg_Sales = mean(amount))

data2$Month <- floor_date(as.Date(data2$date, "%m-%d-%y"), unit = "month")

data3 <- data2 %>% 
  group_by(Month) %>%
  summarize(AvgSales = mean(amount))

# Line chart 
ggplot(data3, aes(x = Month, y = AvgSales)) +
  geom_line() + 
  scale_y_continuous(labels = label_number()) + theme_minimal()

# Heatmap of sales by month and category
ggplot(data2) +
  geom_tile(aes(x = Month, y = category, fill = category)) + theme_minimal()

#----------------------------------------------------------------#

#Which market (state) generated the most sales on average?
data2 %>% 
  group_by(`ship_state`) %>%
  summarise(Avg_Sales = mean(amount)) %>%
  arrange(desc(Avg_Sales))
data2 %>%
  group_by(`ship_state`) %>%
  summarise(Avg_Sales = mean(amount)) %>%
  arrange(desc(Avg_Sales)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(ship_state, -Avg_Sales), y = Avg_Sales)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "State", y = "Average Sales") +
  theme_minimal() +
  coord_flip()

#---------------------------------------------------------------#
#Which category of products sell best? 
data2 %>%
  group_by(category) %>%
  summarise(Total_Sales = sum(amount)) %>%
  arrange(desc(Total_Sales))
#####
data2_summary <- data2 %>%
  group_by(category) %>%
  summarise(Total_Sales = sum(amount)) %>%
  arrange(desc(Total_Sales))

# Create a horizontal bar chart
bar_chart <- ggplot(data2_summary, aes(x = reorder(category, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Category", y = "Total Sales") +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

# Display the bar chart
print(bar_chart)

#---------------------------------------------------------------#
#When were the best- and worst-selling periods?
data2 %>%
  mutate(Month = format(as.Date(date, "%m-%d-%y"), "%m")) %>%
  group_by(Month) %>%
  summarise(Sales = sum(amount)) %>%
  ggplot(aes(Month, Sales, fill = Month)) + 
  geom_col()+
  labs(
    title = "Sales by Month",
    y = "Total Sales"  
  )+
  scale_y_continuous(labels = label_number()) + theme_minimal()
#--------------------------------------------------------------#
#What is the average order value (AOV) for each product category?
data2 %>%
  group_by(category) %>%
  summarise(AOV = mean(amount))

  #Visualize AOV
data2 %>% 
  group_by(category) %>%
  summarise(AOV = mean(amount)) %>%
  ggplot(aes(x=category, y=AOV, fill = category)) +
  geom_col() +
  labs(title="Average Order Value by Product Category")+theme_minimal()
