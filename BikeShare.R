install.packages("vroom")
install.packages("ggplot")
install.packages("skimr")
install.packages("DataExplorer")
install.packages("GGally")
install.packages("patchwork")
install.packages("lubridate")
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(vroom)
library(skimr)
library(DataExplorer)
library(GGally)
library(patchwork)
library(lubridate)
library(dplyr)


dataset<-vroom("Bike Data/train.csv")
dplyr::glimpse(dataset) 
skimr::skim(dataset)
DataExplorer::plot_intro(dataset)
DataExplorer::plot_correlation(dataset)
DataExplorer::plot_bar(dataset)
DataExplorer::plot_histrograms(dataset)
DataExplorer::plot_missing(dataset)
GGally::ggpairs(dataset) 

dataset <- dataset %>%
  mutate(date = as.Date(datetime))
#barplot
    p1 <- ggplot(dataset, aes(x = weather)) +
      geom_bar(fill = "gray") +
      labs(title = "Count of Weather Conditions", x = "Weather", y = "Count") +
      theme_bw()
    p1

# 2. Histogram of temperature
    p2 <- ggplot(dataset, aes(x = temp)) +
      geom_histogram(bins = 30, fill = "gray", color = "white") +
      labs(title = "Temp Distribution", x = "Temp (C)", y = "Count of bikes at temp") +
      theme_bw()
    p2
# 3. Boxplot of bike count by season
    p3 <- ggplot(dataset, aes(x = factor(season), y = count, fill = factor(season))) +
      geom_boxplot() +
      labs(title = "Bike Count by Season", x = "Season", y = "Count") +
      theme_bw() +
      theme(legend.position = "none")
    p3
#there are lots of potential outliers here

# 4. Dot plot (because line plots are confusing w/ the dropped dates) 
#of avg daily count over time
    daily_counts <- dataset %>%
      group_by(date) %>%
      summarise(avg_count = mean(count), .groups = "drop")
    
    p4 <- ggplot(daily_counts, aes(x = date, y = avg_count)) +
      geom_point() +
      labs(title = "Average Daily Bike Count Over Time",
           x = "Date", y = "Avg Count") +
      theme_bw()
    p4
    
#heres my grid
(p1 | p2) / (p3 | p4)
    
    
my_linear_model <- linear_reg() |> 
  set_engine('lm') |> 
  set_mode('regression') |> 
  fit(formula=Response)

trainData<-vroom("Bike Data/train.csv") 
trainData<- trainData |> 
  select(-casual, -registered)
testData<-vroom("Bike Data/test.csv")
library(tidymodels)
## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>%  #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula= count~.-datetime, data=trainData)## Generate Predictions Using Linear Model

  bike_predictions <- predict(my_linear_model, 
                              new_data=testData) # Use fit to predict
  bike_predictions 
  
## Format the Predictions for Submission to Kaggle
  kaggle_submission <- bike_predictions %>%
    bind_cols(., testData) %>% #Bind predictions with test data
    select(datetime, .pred) %>% #Just keep datetime and prediction variables
    rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
    mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
    mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
    
  ## Write out the file
    vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
    
##### I spent a while messing with the data here only to find something ######
#####       Thats already described in the data description             ###########

# DateTime <- ggplot(dataset, aes(x = datetime, y= count)) +
#   geom_line(color="#69b3a2") +
#   labs(title = "Dist of Weather Conditions", x = "Date Time", y = "Count") +
#   theme_minimal()
# 
# DateTime
# #something is weird here. it looks like theres missind data or something in a pattern
#
# 
# dataset_first_half_2011 <- dataset %>%
#   filter(year(datetime) == 2011, month(datetime) <= 6)
# 
# DateTime2 <- ggplot(dataset_first_half_2011, aes(x = datetime, y= count)) +
#   geom_line(color="#69b3a2") +
#   labs(title = "Dist of Weather Conditions", x = "Date Time", y = "Count") +
#   theme_minimal()
# 
# DateTime2
# #it looks like from this the end of the month dropps off, to ge the exact day I will fiter down further
# 
# dataset_Jan_Feb <- dataset %>%
#   filter(year(datetime) == 2011, month(datetime) <= 3)
# 
# DateTime3 <- ggplot(dataset_Jan_Feb, aes(x = datetime, y= count)) +
#   geom_line(color="#69b3a2") +
#   labs(title = "Dist of Weather Conditions", x = "Date Time", y = "Count") +
#   theme_minimal()
# 
# DateTime3
# 
# # Aggregate to daily total count
# daily_counts <- dataset_Jan_Feb %>%
#   group_by(date = as.Date(datetime)) %>%
#   summarise(total_count = sum(count), .groups = "drop")
# 
# # Plot with vertical x-axis labels
# DateTime4 <- ggplot(dataset_Jan_Feb, aes(x = datetime, y = count)) +
#   geom_point(color = "#69b3a2") +
#   labs(title = "Bike Counts (Jan–Feb 2011)",
#        x = "Date", y = "Count") +
#   theme_minimal() +
#   scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# DateTime4

# 
# Testing<-ggplot(dataset, aes(x=temp, y=count)) +
#   geom_point() +
#   geom_smooth(se=FALSE)
# Testing
