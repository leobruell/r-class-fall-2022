library(ggplot2)
data("diamonds", package = 'ggplot2')
head(diamonds)
hist(diamonds$carat, main="Carat Hist", xlab='Carat')
nrow(diamonds)

# Histogram ####
ggplot(diamonds) + geom_histogram(aes(x=carat))


ggplot(data=diamonds) + geom_density(aes(x=carat), color='blue', fill="grey50")


# Scatter ####
plot(price ~ carat, data=diamonds)

ggplot(diamonds, aes(x=carat, y=price)) + geom_point()

g <- ggplot(diamonds, aes(x=carat, y=price))

g + geom_point(aes(color=cut), size=1, shape=1)

plot(0:25, pch=0:25)

g + geom_point() + geom_smooth() # not straight
g + geom_point() + geom_smooth(method = 'lm') # straight

#Boxplots ####
boxplot(diamonds$carat)
boxplot(carat ~ cut, data=diamonds)

ggplot(diamonds, aes(y=carat, x=1)) + geom_boxplot()

ggplot(diamonds, aes(y=carat, x=cut)) + geom_boxplot()

ggplot(diamonds, aes(y=carat, x=cut)) + geom_violin()

ggplot(diamonds, aes(x=cut, y=carat)) +
    geom_jitter(alpha=1/3, size=1, shape=1, width=.35, aes(color=price)) +
    geom_violin(alpha=1/2, fill='grey', draw_quantiles=c(.25, .5, .75)) +
    scale_color_viridis_c('Price', label=scales::dollar) +
    labs(
        x='Cut', y='Carat', 
        title='Distribution of diamond carats', 
        subtitle='Informed by cut and price'
    ) + 
    theme(
        plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust=0.5)
    )

g + geom_point(aes(color=color)) + facet_wrap(~color)

library(plotly)
p <- ggplot(diamonds, aes(x=carat, fill=cut)) + 
    geom_histogram() + 
    facet_wrap(~cut) + theme(legend.position='none')
p %>% ggplotly() #This makes stuff interactive very easily!
diamonds |> head() |> dim()

#Data Manipulation w/ dplyr ####

library(dplyr)
diamonds |> summarise(mean(price))

diamonds |> group_by(cut) |> summarise(avg_price=mean(price), sum_carat=sum(carat))

#Tibbles ####
library(tibble)
x <- 10:1
y <- -4:5
sports <- c("Hockey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")
my_tib  <- tibble(x, y, sports)

select(my_tib, x, y)

my_tib |> select(x, y)

my_tib |> arrange(x) #sorting on col, add - to change direction
my_tib |> arrange(-x)
