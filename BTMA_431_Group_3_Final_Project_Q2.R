library(httr)
library(jsonlite)
library(rvest)
library(XML)
library(ggplot2)
library(dplyr)
library(scales)

# Set SSL certificate to FALSE so we don't have to have a verified server on our end ...
# or on the websites end
httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

### Question 2: Which team/organization has the highest amount of prize money won?

# To get our raw data, we access the API from esportsearnings.com and utilize a ...
# function within their API titled "LookupHighestEarningTeams". The API key ...
# is specific to one of our group members (Arjan), so this key may be different for ...
# different users.
lookupHighestEarningTeamsRAW <- GET("http://api.esportsearnings.com/v0/LookupHighestEarningTeams?apikey=448cef49c9d93c4b183470583d95672f0af5b2486691b3a26c60ce521967ea76")

# Once we have used the GET() function to call our API function, we have raw JSON ...
# data. We must use the rawToChar() function on our JSON content to convert the ...
# raw JSON data into characters to be used in R.
lookupHighestEarningTeamsJSON <- rawToChar(lookupHighestEarningTeamsRAW$content)

# Lastly, we use the fromJSON() function to create a dataframe in R from our ...
# JSON file of character data. This dataframe is already sorted from highest ...
# earning to lowest earning team, for the top 100 teams globally.
lookupHighestEarningTeamsDF <- fromJSON(lookupHighestEarningTeamsJSON)

# We know that the first row contains data for the highest earning team, so ...
# we can assign the total earnings for the highest earning team to a new variable.
highestEarningTeamMoney <- lookupHighestEarningTeamsDF$TotalUSDPrize[1]

# Then we can paste our answer displaying the team name and total earnings. As of ...
# December 1, 2022, the highest earning team globally is Team Liquid.
paste("The highest earning team is", lookupHighestEarningTeamsDF$TeamName[1],
      "with total USD earnings of:", prettyNum(highestEarningTeamMoney, 
                                     big.mark = ",", scientific = FALSE))

### Question 2a: For the team with the highest amount of prize money won, what is ...
# the distribution of their total earnings from all games they participate in?

# Create a new column concatenating the team name and the teamID to use in ...
# our html link scraping process
lookupHighestEarningTeamsDF$IdName <- paste(lookupHighestEarningTeamsDF$TeamId, lookupHighestEarningTeamsDF$TeamName)  

# Replace all spaces in the IdName column with dashes. This format will work with ...
# the esportsearnings.com html link format
lookupHighestEarningTeamsDF$IdName <- gsub(" ", "-", lookupHighestEarningTeamsDF$IdName)

# Add a new column consisting of only forward slashes to use in our scraping
lookupHighestEarningTeamsDF$slash <- "/"

# The primary link we will be scraping from will be the teams link from esportsearnings
mainLink <- getHTMLLinks(htmlParse(GET("https://www.esportsearnings.com/teams/")))

# When compiling our list of all links for all teams, we can assemble the link ...
# as follows: https://www.esportsearnings.com/teams/teamID-teamName/top-games

# The below paste0() function will assemble the html link per the above formatting ...
# and store each of the 100 teams' links in a new variable, allTeamLinks
allTeamLinks <- paste0(mainLink[28], mainLink[6], lookupHighestEarningTeamsDF$slash,
                       lookupHighestEarningTeamsDF$IdName, "/top-games")

# We need to create an empty list to store all dataframes of team game data for ...
# all teams
listOfTeamGameData <- list()

# We create a function that takes a link for a team as an input, then appends ...
# the corresponding data frame of game data for that team to our tracker list ...
# created above.
teamGameFunction <- function(teamLinks){
  # First, use the read_html() function on the link for the team
  topEarningTeamHTML <- read_html(teamLinks)
  
  # Then we use the html_nodes() function to identify tables of data within the ...
  # link and assign the table to our new variable (topEarningTeamTABLE)
  topEarningTeamTABLE <- html_nodes(topEarningTeamHTML, css = "table")
  
  # Now we can convert the table of data into a dataframe using the html_table() ...
  # function and the as.data.frame() function while setting the first row to be ...
  # the headers for our data frame
  topEarningTeamDF <- as.data.frame(html_table(topEarningTeamTABLE, header = TRUE))
  
  # Lastly, we append the new data frame for the team game data to our tracker list
  listOfTeamGameData <- append(listOfTeamGameData, topEarningTeamDF)
}

# Using the lapply() function on our character of all team links will result in a ...
# list of 100 data frames. One data frame of game data for each of the 100 teams
teamGameData <- lapply(allTeamLinks, teamGameFunction)

# For all data frames of game data in our list
for(i in seq_along(teamGameData)){
  # Create a new column in each data frame and assign the corresponding teamID ...
  # and name concatenation to the entire column
  teamGameData[[i]]$Team <- lookupHighestEarningTeamsDF$IdName[i] 
}

# Once again, we understand that our data was sorted from highest earning to lowest ...
# earning, so we can check the first data frame in our list to double check if it ..
# is still Team Liquid. If this is correct, we know our process worked.
teamGameData[[1]]$Team

# We can assign the data frame for Team Liquid to a new variable so we have an ...
# isolated data frame that is easy to work with
teamLiquid <- as.data.frame(teamGameData[1], stringsAsFactors = FALSE)

# We need to run the following three transform functions on the Team Liquid data ...
# frame in order to clean the data of all extraneous characters ($ , %) and set the ...
# type of data to numeric in these columns
teamLiquid <- transform(teamLiquid, Total..Team. = as.numeric(gsub("[$,]", "", teamLiquid$Total..Team.)))
teamLiquid <- transform(teamLiquid, Total..Overall. = as.numeric(gsub("[$,]", "", teamLiquid$Total..Overall.)))
teamLiquid <- transform(teamLiquid, X..of.Total = as.numeric(gsub("[%]", "", teamLiquid$X..of.Total)))

# Once the columns have been assigned as numerics, we can quickly sum up the total ...
# earnings again and assign the value to a variable. Note that this variable ...
# will have a slight variance of ~200K from the value found in Question 2. This ...
# may be because of tournaments that have not yet been updated in the database ...
# but have been included in the final calculation. This could also be because ...
# of old data that is left out of the calculation on accident from the database owners
totalTeamLiquidEarnings <- sum(teamLiquid$Total..Team.)

# A new column will be created with a calculation of the percentage of the total ...
# amount Team Liquid has earned from each game individually
teamLiquid$percentPerGame <- teamLiquid$Total..Team. / totalTeamLiquidEarnings

# This percentage is initially calculated as a decimal value and store in ...
# scientific format. We will convert this into non-scientific format
teamLiquid$percentPerGame <- format(teamLiquid$percentPerGame, scientific = FALSE)

# Once the data is in decimal form in non-scientific format, we round off each ...
# weighting to 6 decimal places
teamLiquid$percentPerGame <- round(as.numeric(teamLiquid$percentPerGame), digits = 6)

# Then we can write this data frame into a csv file for use in a Power BI visualization
# (See additional submission files for Power BI visualization)
write.csv(teamLiquid, "C:/Users/asbir/OneDrive/Desktop/Fall 2022/BTMA 431/TeamLiquid.csv", row.names = FALSE)

### Question 2B: Can we predict the total earnings of ALL teams by using the total ...
# number of tournaments as a predictor?

lookupHighestEarningTeamsDF <- transform(lookupHighestEarningTeamsDF, TotalUSDPrize = 
                                         as.numeric(lookupHighestEarningTeamsDF$TotalUSDPrize))

# Using our data frame from Q2, we can create a logistic regression on total USD ...
# prize money of a team using the total number of tournaments the team participates ...
# in as a predictor 
prizeRegressionOnTournamentsPerTeam <- lm(log(lookupHighestEarningTeamsDF$TotalUSDPrize) ~ 
                                        log(lookupHighestEarningTeamsDF$TotalTournaments), 
                                        data = lookupHighestEarningTeamsDF)

# Viewing the summary of regression results
summary(prizeRegressionOnTournamentsPerTeam)

# We can conclude that the model creates a prediction according to the formula below:
# Total USD Prize = 13.8297 + (0.3140 * Total Number of Tournaments)

# Additionally, we can plot this regression in ggplot2. 
plotOfQ2B <- ggplot(lookupHighestEarningTeamsDF, aes(
             x = TotalTournaments, y = TotalUSDPrize)) + 
             geom_point(color = "blue") + geom_smooth(method = "lm", 
             formula = y ~ x, color = "red") + scale_x_continuous(labels = 
             label_comma()) + scale_y_continuous(labels = label_comma()) + 
             ggtitle("Predicted Prize Winnings Based on Tournament Participation")

# Printing the plot
plotOfQ2B

# When inspecting the results of the regression plot, it falls in line with our ...
# expectations. Teams that don't participate in as many tournaments do not have ...
# as much prize money as other teams. However, there are some outliers such as ...
# OG, a team with less than 200 tournaments and earnings above $35M USD





