# Name:    Shah Wali Ullha Baig

# DS-321__Data Visualization
# Assignment 2


library(ggplot2)
#library(ggpairs)
#library(dplyr)
#library(tidyverse)
library(readr)
library(GGally)
library(plotly)
library(corrplot)

#<-----------------------------_(1)_---------------------------->

blue_jays <-  read.csv('blue_jays.csv')

# Body mass vs body length using scatter plot

ggplot(blue_jays, aes(x=body_mass_g, y=bill_length_mm)) + 
  geom_point(size=1.2, shape=21) + 
  labs(x = "Body Mass", y = "Body length")

# Scatter plot with Regression line

ggplot(blue_jays, aes(x=body_mass_g, y=bill_length_mm)) + 
  geom_point(size=1.2, shape=21) + geom_smooth()  + 
  labs(x = "Body Mass", y = "Body length")

# Scatter plot with Regression line withouot Shades 

ggplot(data = blue_jays, aes(x=body_mass_g, y=bill_length_mm)) + 
  geom_point(size=1.2, shape=21) + geom_smooth(se=FALSE) + 
  labs(x = "Body Mass", y = "Body length")

#<-----------------------------_(2)_---------------------------->

# Make a cluster of male and female birds based on head 
# length and skull size vs body mass using scatter plot

# (1): Body mass VS Head Length:

ggplot(data = blue_jays,
       aes(x = body_mass_g , y = head_length_mm, col = factor(sex))) +
  geom_point() + 
  labs(title = "Body mass VS Head Length" , x = "Body Mass", 
       y = "Head Length") + scale_colour_discrete(name="Gender")

# (2): Body mass VS Skull Size:

ggplot(data = blue_jays,
       aes(x = body_mass_g , y = skull_size_mm, col = factor(sex))) +
  geom_point() + 
  labs(title = "Body mass VS Skull Size" , x = "Body Mass", 
       y = "Skull Size") + scale_colour_discrete(name="Gender")

#<-----------------------------_(3)_---------------------------->

# Show all against all scatter plot of blue-jays bird and find 
# the correlation between them

# (1): Scatter Plots:
p <- pairs(blue_jays[,3:8], col = factor(blue_jays$sex))
ggplotly(p)

ggplotly(p)+geom_smooth(method="lm")

