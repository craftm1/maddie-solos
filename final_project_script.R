## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")
mean(data$solo)
mean(data$backlash)
mean(data$style)
mean(data$placement)
max(data$solo)
max(data$backlash)
max(data$style)
min(data$placement)
##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(data$solo)
mean(data$backlash)
mean(data$style)
mean(data$placement)
max(data$solo)
max(data$backlash)
max(data$style)
min(data$placement)
##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
lm(backlash ~ solo, data = data)
aov(backlash ~ solo, data = data)
summary (backlash ~ solo, data = data)
boxplot(backlash ~ solo, data = data)

ggplot(raw_data, aes(x = as.factor(data$style), y = data$backlash)) +
  geom_boxplot() +
  labs(title = "Box Plot of solo by backlash",
      x = "solo",
       y = "backlash") +
  theme_minimal()

# Perform ANOVA
anova <- aov (backlash ~ style, data = raw_data)
# Summarize ANOVA results
summary (anova)
# get R2
# between/total
# OR between/(between+within)
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$placement, data$backlash)
print(linear_plot)
meany <- mean(data$placement)
meanx <- mean(data$backlash)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(backlash ~ placement, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
#plot the residuals
plot (data$placement, residuals (linear_relationship))
plot (data$backlash, residuals (linear_relationship))
##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$backlash, data$placement)

chisq.test(data$backlash, data$placement)