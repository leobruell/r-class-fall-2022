# Mapping ####

newList <- list(c(1,2,3),1:10, 7)
library(purrr)
summedList <- newList |> map(sum)
class(summedList)
test <- newList |> map_dbl(sum)
class(test)

library(dplyr)
data(diamonds, package = 'ggplot2')
diamonds |> map_dbl(mean)

diamonds |> summarise(across(everything(), mean))

# Joins ####
library(readr)
colors_url <- 'http://www.jaredlander.com/data/DiamondColors.csv'
diamondColors <- read_csv(colors_url)
head(diamondColors)
class(diamondColors)
unique(diamonds$color)

diamonds |> left_join(diamondColors, by=c('color'='Color')) |> 
    select(carat, color, price, Description, Details)

library(tidyr)
emotion <- read_tsv('http://www.jaredlander.com/data/reaction.txt')
emotionLong <- emotion |> gather(key=Type, value=Measurement, Age,
                                 BMI, React, Regulate) |> 
    arrange(ID)

emotionLong

#This undoes arrange
emotionLong |> spread(key=Type, value=Measurement)
