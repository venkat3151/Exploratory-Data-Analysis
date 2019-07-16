# Part 3: Twitter Application Development -  Fetching Data

# Student Details
## Name - Miki Padhiary, UBID - mikipadh, Person Number - 50286289
## Name - Venkatesh Viswanathan, UBID - vviswana, Person Number - 50290589

#Installing packages
install.packages("rtweet")
install.packages("ggmap")
install.packages("ggplot2")
install.packages("usmap")
library(usmap)
library("rtweet")
library("ggmap")
library("maptools")
library(maps)
library(ggplot2)
library(dplyr)
library(stringr)

#Registering google key and creating token
register_google(key = "Google API Key", write = TRUE) 
create_token(
  app = "Miki_Lab1",
  consumer_key = "JzihmlwTcFf4cxWJsZNWYSGPm",
  consumer_secret = "mn0Axc5BOU87z99tqAnv1IL5V8cc9aahepkaGR6irQPBB2CV9W",
  access_token = "253428076-PVSu508vRLjKXK6MPmP6Iv7ZQoqBMLeavQ5bs7sn",
  access_secret = "GVIkI1heo1CDFNhI97u97l0d35tShORR52CrlzTsoCsrs"
)

#Looking for keyword
tweets <- search_tweets('cold AND flu', geocode = lookup_coords("usa"), n=3500, include_rts = FALSE)
newTweets <- lat_lng(tweets)
newTweets <- apply(tweets,2,as.character)

#Checking if data exists. Use this for downloading fresh tweets
if(file.exists("D:/MS/2ndSem/DIC/Lab1Docs/Lab1EDA/Problem3/influenza.csv"))
{
  oldTweets <- read.csv(file="D:/MS/2ndSem/DIC/Lab1Docs/Lab1EDA/Problem3/influenza.csv", header=TRUE, sep=",") 
  oldTweets <- apply(oldTweets,2,as.character) 
}
if(exists("oldTweets")){
  oldTweets = oldTweets[,c(-1)]
  oldTweets <- apply(oldTweets,2,as.character)
  allTweets <- merge(newTweets, oldTweets, all = TRUE)
  write.csv(allTweets, file = "D:/MS/2ndSem/DIC/Lab1Docs/Lab1EDA/Problem3/influenza1.csv")
  allTweets <- unique(allTweets)
  count<- sum(is.na(allTweets$stateName))
  for(i in 1:count)
  {
    print("----------------------------------Fetching Data and Running---------------------------------")
    print(i)
    locationVal = allTweets$location[i]
    geoCodeVal <- geocode(toString(locationVal))
    allTweets$lat[i] <- geoCodeVal$lat
    allTweets$lon[i] <- geoCodeVal$lon
    geoCodeAddress <- revgeocode(as.numeric(geocode(toString(locationVal))))
    stateCode <- str_extract(geoCodeAddress, "([A-Z]{2}) (\\d{5})")
    stateCode <- str_extract(stateCode, "([A-Z]{2})")
    stateName <- state.name[match(stateCode,state.abb)]
    allTweets$stateName <- as.character(allTweets$stateName)
    allTweets$stateName[i] <- stateName
    # print(allTweets$stateName[i])
  }
  allTweets <- filter(allTweets,stateName!="")
  write.csv(allTweets, file = "D:/MS/2ndSem/DIC/Lab1Docs/Lab1EDA/Problem3/influenza.csv")
}else
{
  allTweets <- newTweets
  allTweets <- unique(allTweets)
  allTweets <- cbind(allTweets, geocode(as.character(allTweets[, "location"])))
  allTweets <- filter(allTweets,lon!="", lat!="")
  geoCodeAddress <- do.call(rbind,
                   lapply(1:nrow(allTweets),
                          function(i)revgeocode(as.numeric(allTweets[i,89:90]))))
  stateCode <- str_extract(geoCodeAddress, "([A-Z]{2}) (\\d{5})")
  stateCode <- str_extract(stateCode, "([A-Z]{2})")
  stateName <- state.name[match(stateCode,state.abb)]
  allTweets <- cbind(allTweets,stateName)
  allTweets <- filter(allTweets,stateName!="")
  write.csv(allTweets, file = "D:/MS/2ndSem/DIC/Lab1Docs/Lab1EDA/Problem3/cold.csv")
}
# allTweets <- read.csv(file="D:/MS/2ndSem/DIC/Lab1Docs/Lab1EDA/Problem3/influenza.csv", header=TRUE, sep=",")
stateVal <- allTweets[, "stateName"]
stateVal <- table(stateVal)
finalData <-data.frame(result=names(stateVal),NumberofTweets=c(stateVal))
finalData$region <- tolower(finalData$result)
# states <- map_data("state")
# map.df <- merge(states,finalData, by="region", all.finalData=T)
# map.df <- map.df[order(map.df$order),]
# ggplot(map.df, aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=NumberofTweets))+
#   geom_path()+
#   scale_fill_gradientn(colours=terrain.colors(10),na.value="gray50", limits=c(0,200))+
#   coord_map()

# Generating map from existing tweets. Comment this if want to download fresh tweets
allTweets <- read.csv(file="DIC4.csv", header=TRUE, sep=",")
stateVal <- allTweets[, "stateName"]
stateVal <- table(stateVal)
finalData <-data.frame(result=names(stateVal),NumberofTweets=c(stateVal))
finalData$region <- tolower(finalData$result)
cols <- c("result","NumberOfTweets","state")
colnames(finalData) <- cols

# Plotting map
plot_usmap(data = finalData, values = "NumberOfTweets", lines = "cyan4") + 
  scale_fill_continuous(
    low = "Green", high = "Red", name = "Tweets", label = scales::comma
  ) + theme(legend.position = "right")

