

library(xlsx)
library(tidyr)
setwd("~/Desktop/Future/Ultimate R/Data") #set working directory
getwd() #verify your working directory

#Load data
data<- read.csv("nameoffile.csv") #load file into R
write.csv(WHO_Europe,"WHO_Europe.csv") #save new data set as csv.file

cat("\014") #clear console - code & press ctl + L

#VARIABLES
#no spaces, separate with periods, dont start with number, case sensitive
#assignment of variable with = or <-
ls()  #see all the variables in current R session
rm(nameofvariable) #delete variable from R memory 

class(data) #class of dataset
dim(data) #dimensions of dataset
str(data) #structure of any data type
glimplse(data) #dplyr, longer view
summary(data) #main statistics depending on class, number of NAs
names(data) #prints column (variables) names

head(data,6) #prints first 6 rows of data
tail(data)
hist() #histogram data$variable
plot() #scatterplot (data$variablex, data$variabley)

#many histograms of my variables in one plot?

# TIDY DATA #########################################################################################
#observations in rows, variables in columns in a proper manner
gather() #
unite()
spread() 
separate() #split one column in 2 - M.34 into M and 34
#Newdata <- separate(data, col = nameofcolumn, into = c("newcol1", "newcol2"), sep = "separator e.g./ if any")

# TIDY DATA #########################################################################################
# DATA CLASSES
# character: "treatment" = name of variable
# numeric: 23.44, 120, NaN, Inf
# integer: 4L, 111L
# factor: factor("Hello"), "factor(8)" 
    # categorical variables = have small number of potential outcomes
    # number of levels = # of categories
    # ordered (grades 1-5) or unordered (continents)
# logical: TRUE, FALSE, NA
# CHANGE DATA CLASS
as.character(2016)
as.numeric(TRUE)
#Change type of data to Date
library(lubridate)
ymd("2020-04-04") #parse character class as date class
mdy("August 25, 2015")
hms("13:33:09")
# students2$dob <- ymd(students2$dob) changes the dob column to date format
