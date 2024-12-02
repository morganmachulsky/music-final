## Project:  STA 215, Fall 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(haven)

# Load data 
library(readr)
dataset <- read_csv("final.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(dataset$streams)
sd(dataset$streams)
table(dataset$rap)

mean(dataset$bpm)
sd(dataset$bpm)
mean(dataset$song_length)
sd(dataset$song_length)

table(dataset$pop)
table(dataset$hip_hop)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
boxplot(streams ~ rap, data = dataset)

anova <- aov(streams ~ rap, data = dataset)
summary(anova) 

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
# Create scatter plot of BPM vs Song_length
plot(dataset$bpm, dataset$song_length, 
     xlab = "BPM", ylab = "sonng_length (milliseconds)",
     main = "Scatter Plot: BPM vs song_length", pch = 19, col = "blue")

# Fit linear model (regression line)
model <- lm(song_length ~ bpm, data = dataset)

# Add regression line to the plot
abline(model, col = "red", lwd = 2)

# Show summary of the linear model
summary(model)

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$song_length, residuals(model))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$pop, dataset$hip_hop)

chisq.test(table(dataset$pop, dataset$hip_hop))