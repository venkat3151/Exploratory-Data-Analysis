# Part 3: Twitter Application Development - Publishing app

# Student Details
## Name - Miki Padhiary, UBID - mikipadh, Person Number - 50286289
## Name - Venkatesh Viswanathan, UBID - vviswana, Person Number - 50290589

## Shiny app url : https://mikip.shinyapps.io/dic1/

library(usmap)
library(ggplot2)
library(shiny)
library(rsconnect)

# Tweets having keyword as flu and influenza
allTweets <- read.csv(file="DIC4.csv", header=TRUE, sep=",")
stateVal <- allTweets[, "stateName"]
stateVal <- table(stateVal)
finalData <-data.frame(result=names(stateVal),NumberofTweets=c(stateVal))
finalData$region <- tolower(finalData$result)
cols <- c("result","NumberOfTweets","state")
colnames(finalData) <- cols

# Tweets having keyword as flu
fluTweets <- read.csv(file="flu.csv", header=TRUE, sep=",")
stateVal <- fluTweets[, "stateName"]
stateVal <- table(stateVal)
fluData <-data.frame(result=names(stateVal),NumberofTweets=c(stateVal))
fluData$region <- tolower(fluData$result)
cols <- c("result","NumberOfTweets","state")
colnames(fluData) <- cols

# Tweets having keyword as influenza
influTweets <- read.csv(file="influenza.csv", header=TRUE, sep=",")
stateVal <- influTweets[, "stateName"]
stateVal <- table(stateVal)
influData <-data.frame(result=names(stateVal),NumberofTweets=c(stateVal))
influData$region <- tolower(influData$result)
cols <- c("result","NumberOfTweets","state")
colnames(influData) <- cols

# Tweets having keyword as cold and flu
coldTweets <- read.csv(file="cold.csv", header=TRUE, sep=",")
stateVal <- coldTweets[, "stateName"]
stateVal <- table(stateVal)
coldData <-data.frame(result=names(stateVal),NumberofTweets=c(stateVal))
coldData$region <- tolower(coldData$result)
cols <- c("result","NumberOfTweets","state")
colnames(coldData) <- cols

ui <- fluidPage(
  h1("Twitter_Data vs CDC_Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  label = "Charts",
                  choices = c("Keyword:all" = "all", 
                              "Keyword:flu" = "flu",
                              "Keyword:influenza" = "influ",
                              "Keyword:cold and flu" = "cold"),
                  selected = "Keyword:all")),
      mainPanel(verticalLayout(
        h2("Twitter Charts"),
        plotOutput("selected_var"),
        h4(textOutput("txt_input")),
        h2("CDC Chart"),
        uiOutput("cdc"))))
  )
server<- function(input, output){
  output$selected_var <- renderPlot({ 
    if(input$var =="all")
    {
      plot_usmap(data = finalData, values = "NumberOfTweets", lines = "Black") + 
      scale_fill_continuous(
      low = "Green", high = "Red", name = "Tweets", label = scales::comma
      ) + theme(legend.position = "right")
    }
    else if(input$var =="flu")
     {
      plot_usmap(data = fluData, values = "NumberOfTweets", lines = "Black") + 
        scale_fill_continuous(
          low = "Green", high = "Red", name = "Tweets", label = scales::comma
        ) + theme(legend.position = "right") 
     }
     else if(input$var =="influ")
     {
       plot_usmap(data = influData, values = "NumberOfTweets", lines = "Black") + 
         scale_fill_continuous(
           low = "Green", high = "Red", name = "Tweets", label = scales::comma
         ) + theme(legend.position = "right")
     }
    else if(input$var =="cold")
    {
      plot_usmap(data = coldData, values = "NumberOfTweets", lines = "Black") + 
        scale_fill_continuous(
          low = "Green", high = "Red", name = "Tweets", label = scales::comma
        ) + theme(legend.position = "right")
    }
  })
  output$txt_input <- renderText({
    if(input$var =="all")
    {
      print("From twitter data, out of 29783 unique tweets, the number of persons affected with flu 
            at Texas state is highest, next is california and then Kansas state.
            North Dakota, Vermont and Montana are the least affected states. CDC map for week ending January 26 2019, 
            shows that the most affected stats are Texas, Louisiana, Alaska. Moderate level of activity
            is found in California and Oregon with the least at North Dakota and Ohio.")
    }else if(input$var =="flu")
    {
      print("Out of 2536 unique tweets, the number of persons affected with flu 
            at Texas state is highest, next is california and then Kansas state.
            Montana, Vermont and South Dakota are the least affected states.CDC map for week ending January 26 2019, 
            shows that the most affected stats are Texas, Louisiana, Alaska. Moderate level of activity
            is found in California and Oregon with the least at North Dakota and Ohio.")
    }else if(input$var =="influ")
    {
      print("Out of 1618 unique tweets, the number of persons affected with flu 
            at Kansas state is highest, next is California and then NewYork state.
            Monatana, Alaska and New Hampshire are the least affected states.CDC map for week ending January 26 2019, 
            shows that the most affected stats are Texas, Louisiana, Alaska. Moderate level of activity
            is found in California and Oregon with the least at North Dakota and Ohio.")
    }else if(input$var =="cold")
    {
      print("Out of 3250 unique tweets, the number of persons affected with flu 
            at Kansas state is highest, next is California and then NewYork state.
            Montana and Vermont are the least affected states.CDC map for week ending January 26 2019, 
            shows that the most affected stats are Texas, Louisiana, Alaska. Moderate level of activity
            is found in California and Oregon with the least at North Dakota and Ohio.")
    }
    
  })
  output$cdc <- renderUI({ 
    img(src="cdc.jpg")
  })
}
#Running a server and publishing the app
shinyApp(server = server, ui=ui)
