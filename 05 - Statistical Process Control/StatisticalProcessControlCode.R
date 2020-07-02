########################################
# Dallin Romney                        #
# Design of Experiments                #
########################################

rm(list = ls()) # Clear workspace
cat("\014")     # Clear console (control + L)
install.packages('qicharts2')
library(qicharts2)

############## PART II Project: Batteries ##############

# Read in and organize data
batteryData = read.csv("C:/Users/Dalli/Google Drive/School/Design of Experiments/Homework/HW5/hoverBatteriesDF.csv")
month          = batteryData$month
date           = batteryData$date
leadResistance = batteryData$leadResistance

dates = as.Date(paste0("2019-", month, "-", date)) # Change numbers to dates
batteryData$dates = dates

# Plot resistances over time and xbar control chart
plot(dates, leadResistance, xlab = 'Date', ylab = 'Lead Resistance', main = 'Battery Lead Resistance over Time')

qic(y = leadResistance, data = batteryData, chart = 'xbar', x = dates, xlab = 'Date', ylab = "Lead Resistance", title = 'Lead Resistance Xbar Chart')
sd(leadResistance)
mean(leadResistance)

