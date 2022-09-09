#First Class Notes 
4/3 #This returns float because R is designed for stats.
4 * 2 + 3 #R follows PEMDAS

#Variable assignment ####
x <- 2 #main method
y = 3 #alternative assignment 
rm(y)
y
x + 1

2 > 3
2 >= 3
2 == 3
2 < 3
2 != 3

# Vectors ####
x <- c(1, 3, 8)
x + 1
x
z <- x+1
z
x <- 7
z
x <- c(4, 2, 10)
x*2
x^3
y <- c(1,2,3)
x + y
x < y
# If vectors are different lengths, R will start to loop through 
# the short one.

a_hundred = 1:100
a_hundred[4]
sum(a_hundred)
mean(a_hundred)

# Functions ####
times_2 <- function(x)
{
    return(x * 2)    
}

times_2(6)

timesThese <- function(x, y)
{
    return(x * y)
}

timesThese(8, 5)
timesThese <- function(x, y=2) {
    return(x * y)
}
timesThese(4)

# Read-In Data ####
library(readr)
tomato <- read_csv('class-datasets/data.2fTomatoFirst.csv')
tomato
class(tomato)
#R dataframe is basically pandas. Pandas copied R. 
nrow(tomato)
ncol(tomato)
head(tomato)
tail(tomato)
dim(tomato)
#All values in a column has to be the same datatype.
tomato$Sweet
mean(tomato$Sweet)
tomato$Price
mean(tomato$Price) #This is missing because there is a value with NA
mean(tomato$Price, na.rm = TRUE)

?mean # This pulls up help for function

