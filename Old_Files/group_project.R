print('Mohammad Hossein Movahedi')
print('The group project')

#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('readxl')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(readxl)

#Reading the data set as a dataframe

s15 <- read.csv()

# structure of the data
str(mushrooms)