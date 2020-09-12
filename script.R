library(dplyr)
library(ggplot2)
library(tidyverse)


data1 <- read.csv('project.csv')

myData = select(data1, -1:-3, -5:-23, -25, -31, -33, -36, -38:-39, -41:-42, -45, -47:-49)

levels(myData$Severity) <- c(1:4)

myData <- myData %>% 
  rename(
    temperature = Temperature.F.,
    humidity = Humidity...,
    pressure = Pressure.in.,
    visibility = Visibility.mi.,
    wind_direct = Wind_Direction,
    wind_speed = Wind_Speed.mph.,
  )

hist(myData$temperature, main = "Count of accidents as function of Temperature", xlab = "Temperature [F]")
hist(myData$humidity, main = "Count of accidents as function of Humidity", xlab = "Humidity")
hist(myData$pressure, main = "Count of accidents as function of pressure", xlab = "pressure")
hist(myData$visibility[1:1000], main = "Count of accidents as function of visibility", xlab = "visibility", xlim = c(0,10))

severity_4 <- subset(myData, Severity == 4)
severity_3 <- subset(myData, Severity == 3)
severity_2 <- subset(myData, Severity == 2)
severity_1 <- subset(myData, Severity == 1)

#Histograms for numeric variables:
# Temperature:
hist(severity_1$temperature,  xlim = c(0,60))
hist(severity_2$temperature,  xlim = c(0,60))
hist(severity_3$temperature,  xlim = c(0,60))
hist(severity_4$temperature,  xlim = c(0,60))

# Humidity:
hist(severity_1$humidity)
hist(severity_2$humidity)
hist(severity_3$humidity)
hist(severity_4$humidity)

# Pressure:
hist(severity_1$pressure)
hist(severity_2$pressure)
hist(severity_3$pressure)
hist(severity_4$pressure)

# Visibility:
hist(severity_1$visibility, xlim = c(0,10))
hist(severity_2$visibility, xlim = c(0,10))
hist(severity_3$visibility, xlim = c(0,10))
hist(severity_4$visibility, xlim = c(0,10))

ggplot(myData, aes(wind_direct, pressure, group = Severity)) +
  geom_bar(aes(fill = Severity), position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(x = "Wind Direction",
       y = "Pressure",
       title = "Wind direction does not have a great impact on severity") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6))
