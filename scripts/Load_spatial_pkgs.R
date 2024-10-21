library(maptools)
library(maps)
library(mapdata)
library(sf)
library(rgdal)
library(rgeos)
library(ggmap)
library(tidyverse)
library(ggplot2)





vignette(sf)


vignette("kmeans")

lmfit <- lm(mpg ~ wt, mtcars)
lmfit

summary(lmfit)
augment(lmfit)
tidy(lmfit)
glance(lmfit)

glmfit <- glm(am ~wt, mtcars, family = "binomial")
tidy(glmfit)

glance(glmfit)
augment(glmfit)




