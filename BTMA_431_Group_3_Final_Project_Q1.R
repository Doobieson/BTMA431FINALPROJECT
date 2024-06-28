library(httr)
library(jsonlite)
library(rvest)
library(XML)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

url <- "https://www.esportsearnings.com/games/most-prize-money-awarded"

mostPrize_page <- read_html(url)

# Need to handle the top 5 games in a special due to format
titles <- mostPrize_page %>% html_nodes(xpath="//div[@class='games_top_games_title']") %>% html_text() #get title of top 5 games
data <- mostPrize_page %>% html_nodes(xpath="//div[@class='games_top_games_info_box']") %>% html_children() #get data of top 5 games
prizes <- c() #store the total prize pools
players <- c() #store the player count
tournaments <- c() #store the number of tournaments
for(i in 1:length(data)){ #for each game (in the top 5)
  splitData <- str_split(toString(data[i]), ">")[[1]] #convert HTML to a string and split around closing tags
  prizes <- c(prizes, parse_number(splitData[2])) #pick out the total prize pool
  players <- c(players, parse_number(splitData[3])) #pick out the number of players
  tournaments <- c(tournaments, parse_number(splitData[4])) #pick out the number of tournaments
  
}

# create data frame based on data
top5 <- data.frame("Game"=titles, "Prize"=prizes, "Players"=players, "Tournaments"=tournaments)

mostPrize_html <- GET(url)
mostPrize_parsed <- htmlParse(mostPrize_html)
page.table <- readHTMLTable(mostPrize_parsed, stringsAsFactors = FALSE)[[1]] #get the table containing games 6 and beyond

# create data frame from table information, with named columns
table_info <- data.frame(
  Game=page.table$V2,
  Prize=parse_number(page.table$V3),
  Players=parse_number(page.table$V4),
  Tournaments=parse_number(page.table$V5)
)

# combine the two data frames
complete_info <- rbind(top5, table_info)

#export to csv 
write.csv(complete_info, "esports.csv", row.names=FALSE)


fit <- lm(complete_info$Prize ~ complete_info$Players, data=complete_info)
summary(fit)

plot <- ggplot(complete_info, aes(
    x = Players, y = Prize)) +
    geom_point(color = "blue") + geom_smooth(method = "lm",
    formula = y ~ x, color = "red") +
    ggtitle("Predicted Total Tournament Prize Pool Based on Number of Players") +
    xlab("Number of Players") +
    ylab("Total Tournament Prize Pool")
plot
