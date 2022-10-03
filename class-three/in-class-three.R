#Class Three In-Class Notes

#More on Reading in Data ####

library(data.table) #This is another option for reading in data.

library(readr)

tomatoDT <- fread('https://www.jaredlander.com/data/TomatoFirst.csv')
class(tomatoDT)
head(tomatoDT)

tomatoDT |> tibble::as_tibble()

#Refresh on how to read in data from csvs
test <-  read_csv('class-datasets/data.2fBronxCensus.csv')
head(test)

#Reading in data from excel
download.file('https://www.jaredlander.com/data/ExcelExample.xlsx',
              'class-datasets/ExcelExample.xlsx', mode = 'wb'
)
library(readxl)
excel_sheets('class-datasets/ExcelExample.xlsx')

tomatoXL <- read_excel('class-datasets/ExcelExample.xlsx', sheet='Tomato')
tomatoXL

wine <- read_excel('class-datasets/ExcelExample.xlsx', sheet = 'Wine')

getwd() #this shows our current working directory!

#Reading from databases
#Most databases do not live on your computer. SQLite is one that does.
#You can actually get your text messages off your iphone as an sqlite database

library(DBI)
con <- dbConnect(
    drv=RSQLite::SQLite(),
    'class-datasets/data.2fdiamonds.db'
)
con
class(con)
dbListTables(con)

dbListFields(con, name='diamonds')
dbListFields(con, name='DiamondColors')

diaTab <- dbReadTable(con, 'diamonds')
diaTab |> head()
bigDiamonds <-  dbGetQuery(
    con,
    statement="SELECT * FROM diamonds WHERE carat > 1"
)
bigDiamonds |> head()

library(dbplyr)
library(dplyr)
#This allows us to interact with sql through r commands
diaTbl <- tbl(con, 'diamonds')
class(diaTbl)
diaTbl |> filter(carat > 1)

#Reading in JSON
library(jsonlite)
pizza <- fromJSON('https://www.jaredlander.com/data/FavoriteSpots.json')
pizza
class(pizza)
dim(pizza)
pizza |> tibble::as.tibble() 
#We get list columns when we read in this data. 
pizza$Details[[1]]

#Below we can break up the list columns. 
pizza |> tidyr::unnest(cols=c(Details, Coordinates))

library(sf)
# https://packagemanager.rstudio.com/client/#/repos/1/packages/leaflet
library(leaflet)

#Reading tab seperated files
emotion <- readr::read_tsv('class-datasets/data.2freaction.txt')
emotion

#wide data
library(ggplot2)
ggplot(emotion, aes(x=Age, color=factor(Test))) +
    geom_point(aes(y=React), shape=16) +
    geom_point(aes(y=Regulate), shape=17)
       
library(tidyr)
library(dplyr)
(emotion['Regulate']


emotion['Regulate'] <- sapply(emotion['Regulate'], as.double)
emotion
emotion_long <- emotion |> 
    select(ID, Test, Age, React, Regulate) |> 
    pivot_longer(cols=c(React, Regulate),
                 values_to='Measurement',
                 names_to='EmotionType'
    )
emotion_long

emotion |> group_by(ID) |> 
    summarise(AvgReact=mean(React), AvgReg=mean(Regulate))

emotion_long |> 
    group_by(ID, EmotionType) |> 
    summarise(Avg=mean(Measurement)) |> 
    pivot_wider(
        id_cols = ID,
        names_from = EmotionType,
        values_from = Avg
    )

emotion |> 
    group_by(ID) |> 
    summarise(across(c(React, Regulate), mean))

# lists ####
#Lists are a little like dictionaries in Python
list(1,2,3)
list1 <- list(c(1,2,3), 3:7)

theDF <- data.frame(
    First=1:5,
    Second=5:1,
    Sport=c('Baseball','Basketball', 'Curling', 'Hockey',
            'Soccer')
)
theDF

list2 <- list(theDF, 1:10, list1)
list2

list2[[1]] |> #This is how we select elements in a list. 
    select(First, Sport)

length(list2)
list2[[1]] |> dim()
list2[[2]] |> NROW()
list2[[3]] |> length()

library(purrr)

list2 |> map(length)
list2 |> map(NROW)

list2 |> map_dbl(NROW)
list2 |> map_chr(class)

list2 |> map_chr(class)
list2 |> map_chr(NROW)

list4 <- list(A=1:100, B=17, C=c(3,1,8,12), D=c(33,27, 30))
list4
list4 |> map_dbl(sum)
list4 |> map_dbl(mean)

list2 |> map_if(is.numeric, sum)

#1
library(jsonlite)
pizza <- fromJSON('https://jaredlander.com/data/PizzaPlaces.json') 
pizza <- pizza %>% tibble::as.tibble() 
pizza  <- pizza %>% tidyr::unnest()  
pizza %>% head(10)
#2
diamonds_short %>% summarise(across(where(is.numeric), mean))
#3
diamonds_short %>% select(is.numeric) %>% map(mean)
#4
diamonds_short %>% select(is.numeric) %>% map_df(mean)
#5
diamonds_short %>% map_if(is.numeric, mean, .else = ~NULL)
#6
library(WDI)
countries <- WDI(country = c('BE', 'BY','BZ', 'BJ','BM'), indicator = 'NY.GDP.PCAP.KD',start = 2000, end=2016)
countries  %>% pivot_wider(id_cols = c('iso2c','country'), values_from = 'NY.GDP.PCAP.KD',names_from = 'year',names_prefix = 'gdp.')
#7
airlong <- airquality %>% pivot_longer(c('Ozone','Solar.R','Wind','Temp'), names_to = 'MeasurementType', values_to = 'Measurement')
airlong %>% head(10)
#8
gov_type  <- read_csv('https://jaredlander.com/data/GovType.csv')
gdp_data <- left_join(gdp_data, gov_type, by=c('country'='Country'))
gdp_data


