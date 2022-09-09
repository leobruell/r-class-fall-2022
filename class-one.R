#First Class Notes 
4/3 #This returns float because R is designed for stats.
4 * 2 + 3 #R follows PEMDAS

x <- 2 #Variable assignment ####
x
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
