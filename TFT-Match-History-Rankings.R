# Install Librarys
install.packages(c("httr","jsonlite","animation"))

# open librarys
library(httr)
library(jsonlite)
library(animation)

# Enter Summoner-Name
playerName = ""
# Enter your API-Key for the Riot-API
addApiKeyHere = ""
# Server for EUW
baseURL = "https://euw1.api.riotgames.com"


# Creating Request URL's
getIdByNameURL = "/tft/summoner/v1/summoners/by-name/"
getMatchHistoryById = "/tft/match/v1/matches/by-puuid/"#{puuid}/ids"
apiKeyPre = "api_key="
apiKey = paste0(apiKeyPre,addApiKeyHere)
getPlayerIdURL = paste0(baseURL,getIdByNameURL, playerName,"?",apiKey)

# Request Data for Account with given Summoner-Name
playerRequest = GET(getPlayerIdURL)

# Get the players 'PUUID'
contentPlayerRequest = content(playerRequest, as = 'parsed')
encryptedPUUID = contentPlayerRequest["puuid"]$puuid

# Creating Request URL's
ra = "start=0&count=30"
baseURLMatches = "https://europe.api.riotgames.com"
getPlayersGameIDsURL = paste0(baseURLMatches, getMatchHistoryById, encryptedPUUID,"/ids","?", ra ,"&",apiKey)

# Get player's match history
matchHistoryRequest = GET(getPlayersGameIDsURL)
contentMatchHistory = content(matchHistoryRequest)
print(contentMatchHistory)
getMatchByIDURL = "/tft/match/v1/matches/"

# Variable for all ranks the given Player achieved
placements = c()

for ( id in contentMatchHistory){
  print(id)
  # define Request-URL
  matchRequestURL = paste0(baseURLMatches,getMatchByIDURL,id,"?",apiKey)
  # Get Data for Match
  currentMatch = GET(matchRequestURL)
  contentMatch = content(currentMatch)
  # Get Info about all players in the match
  participantsInfo = contentMatch$info$participants
  
  # Check all Players for given Summoner-Name
  for (p in participantsInfo){
    if (p$puuid == encryptedPUUID){
      # Add Rank to variable
      placements = c(placements, p$placement)
      print(p$placement)
    }
  }
}


# Function for creating a .gif Animation
animatePlacements <- function(vec){
  saveGIF(
    # 1 Picture per Game. With every iteration, one more match is taken into account for the Graph
    for (count in 1:length(vec)){
      ranks = c(0,0,0,0,0,0,0,0)
      relevantVector = vec[1:count]
      # variable for computing mean rank
      sumRanks = 0
      countRanks = 0
      # Sort Ranks into vector
      for (x in relevantVector){
        ranks[x] = ranks[x]+1
        sumRanks = sumRanks + x
      }
      meanRank = sumRanks/length(relevantVector)
      
      
      # Make Barplot for sub-vector for all Games
      barplot(ranks,names.arg=c("1","2","3","4","5","6","7","8"), xlab = "Rank", ylab = "Count", main = paste0("Ranking from the last ", count ," TFT Games by ","'",playerName,"'"), legend.text = paste0("Average rank: ",meanRank))
    }
    , movie.name = paste0("placementInTFT-",playerName,".gif")
  )
}

# Call function
animatePlacements(placements)