library(stringi)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(DT)
library(data.table)
library(pdftools)
library(tm)
library(wordcloud2)
library(readtext)
#library(RWeka)
library(BBmisc)


#======= Header
header <- dashboardHeader(title = "Chat Analysis")

#======== Side Bar
sidebar <- dashboardSidebar(
  fileInput(inputId = "chatfile",label = "Choose the Chat text file",accept = c(".txt"),placeholder = "Browse the File"),
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("dashboard")),
    menuItem("Word Cloud", icon = icon("th"), tabName = "wordcloud"),
    menuItem("Clock Plots", icon = icon("th"), tabName = "clockplot"),
    menuItem("Sentiment Analysis", icon = icon("th"), tabName = "sentiment"),
    menuItem("Word2Vec Network", icon = icon("th"), tabName = "topicmodelling")
  )
)





#============ Body 
body <- dashboardBody(
  includeCSS("www/style.css"),
  tabItems(
    tabItem("home",
            fluidRow(valueBoxOutput("chatCount",width = 4),valueBoxOutput("user1Count",width = 4),valueBoxOutput("user2Count",width = 4)),
            fluidRow(textOutput(outputId ="plot1Header"),textOutput(outputId ="plot2Header")),
            br(),br(),
            fluidRow(splitLayout(cellWidths = c(500,500),plotlyOutput("weekPlot",width = 500,height = 500),plotlyOutput("attchmentPlot",width = 500,height = 500))) 
    ),
    tabItem("wordcloud",
            fluidRow(textOutput(outputId ="plot3Header")),
            br(),br(),
            fluidRow(wordcloud2Output("wordcloud",width = 1200,height = 600))
    ),
    
    tabItem("clockplot",
            
            tabsetPanel(type = "tabs",
                        tabPanel("Clock Plot",
                                 fluidRow(textOutput(outputId ="plot4Header")),
                                 fluidRow(plotOutput("clock",width = 600,height = 600))),
                        tabPanel("Session Graph",
                                 fluidRow(textOutput(outputId ="plot5Header")),
                                 fluidRow(plotlyOutput("session",width = 800,height = 600)))
                        )
            ),
    
    tabItem("sentiment",
            fluidRow(textOutput(outputId ="plot6Header")),
            fluidRow(textOutput(outputId ="plot7Header")),
            br(),br(),br(),
            fluidRow(splitLayout(cellWidths = c(500,600),
              plotlyOutput("sentiment",width = 500,height = 500),
              tabsetPanel(type = "tabs",
                    tabPanel("Top Neutral Words",plotOutput(outputId = "neutwords",width = 600,height = 500)),
                    tabPanel("Top Positive Words",plotOutput(outputId = "postwords",width = 600,height = 500)),
                    tabPanel("Top Negative Words",plotOutput(outputId = "negtwords",width = 600,height = 500))
                          )))),
    
    tabItem("topicmodelling",
            fluidRow(textOutput(outputId ="plot8Header")),
            br(),br(),br(),
            fluidRow(plotOutput("network",width = 1100,height = 600))
            )
            
            
            
    )
  )



#============== Creating UI
UI <- dashboardPage(header = header,sidebar = sidebar,body = body)

