setwd("C:/Users/Lincoln/OneDrive/Fall 2022/BTMA 431/Final Project")

library(ggplot2)
library(readr)


## Q3 data - imported CSV file from sullygnome and cleaned them
finalData.df <- read_csv("finalData.csv")

finalData.df <- finalData.df[-c(1, 3, 4, 5, 6, 7, 9, 10)]

finalData.df <- finalData.df[-c(26:50),]

write.csv(finalData.df,
          "C:/Users/Lincoln/OneDrive/Fall 2022/BTMA 431/Final Project/finalDataPart1.csv",
          row.names = FALSE)


## Q3a data - imported multiple CSV files from sullygnome, bound them into one, and cleaned them

files <- c('Most streamed games on Twitch - 2019.csv', 'Most streamed games on Twitch - 2020.csv',
           'Most streamed games on Twitch - 2021.csv', 'Most streamed games on Twitch - 2022.csv')
year <- 2019

for (i in files) {
  eval(parse(text = paste0('data_', year, ' <- read_csv("', i, '")')))
  
  eval(parse(text = paste0('data_', year, ' <- data_', year, '[-c(1,2,4,5,6,7,8,10,11,12)]')))
  
  eval(parse(text = paste0('data_', year, '[3] <- ', year)))
  
  eval(parse(text = paste0('colnames(data_', year, ')[3] <- "Year"')))
  
  eval(parse(text = paste0('data_', year, '<- data_', year, '[-c(11:50),]')))
  
  year = year + 1
}

totalData <- rbind(data_2019, data_2020)

totalData <- rbind(totalData, data_2021)

totalData <- rbind(totalData, data_2022)

write.csv(totalData, "C:/Users/Lincoln/OneDrive/Fall 2022/BTMA 431/Final Project/finalDataPart2.csv")
