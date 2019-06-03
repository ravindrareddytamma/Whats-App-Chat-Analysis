{
library(sentimentr)
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
library(topicmodels)
library(tidytext)
library(tidyr)
library(textstem)
library(igraph)
library(ggraph)
}

######################## Server Part
server <- function(input,output,session)
{
  # Data Cleaning
  data.celaning <- function(vect)
  {
    correct_pattern <- grep("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}",x = vect)
    incorrect_pattern <- seq(1:length(vect))[-correct_pattern]
    req_loc <- sapply(incorrect_pattern, function(x){
      max(correct_pattern[correct_pattern < x])
    })
    for(i in 1:length(req_loc)){
      vect[req_loc[i]] <- paste(vect[req_loc[i]],vect[incorrect_pattern[i]])
    }
    vect <- vect[-incorrect_pattern]
    return(vect)
  }
  
  
  # Generating Data Frame
  make.structured <- function(vect)
  {
    date <- str_extract(vect,"[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}")
    date <- dmy(date)
    
    time_sess <- str_extract(vect,", [0-9]{1,2}:[0-9]{1,2} [A|P]M")
    if(all(is.na(time_sess)))time_sess <- str_extract(vect,", [0-9]{1,2}:[0-9]{1,2} [a|p]m")
    hours <- as.numeric(gsub(pattern = ":","",str_extract(time_sess,"[0-9]{1,2}:")))
    min <- as.numeric(gsub(pattern = ":","",str_extract(time_sess,":[0-9]{1,2}")))
    session <- str_extract(toupper(time_sess),"[A|P]M")
    
    user <- trimws(gsub("-|:","",str_extract(vect,"- (.*?):")))
    
    res1 <- str_extract(vect,":(.*)")
    res2 <- substr(res1,2,nchar(res1))
    res3 <- str_extract(res2,":(.*)")
    chat_text <- substr(res3,2,nchar(res3))
    
    dataframe <- data.frame("Date" = date,"Hours" = hours,"Minutes" = min,"Session" = session,"User" = user,"ChatMessage" = chat_text)
    dataframe$Hours[dataframe$Session == "PM"] <- dataframe$Hours[dataframe$Session == "PM"] + 12
    dataframe$ChatMessage <- gsub('[^[:alpha:]| ]','',dataframe$ChatMessage)
    dataframe$ChatMessage <- iconv(dataframe$ChatMessage, "latin1", "ASCII", sub="")
    dataframe$ChatMessage <- trimws(tolower(dataframe$ChatMessage))
    dataframe <- dataframe %>% filter(nchar(ChatMessage) != 0)
    return(dataframe)
  }
  
  # Week Plot
  
  gen.weekplot <- function(dataframe)
  {
    p <- dataframe %>%
      group_by("Day" = weekdays(Date)) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Day, values = ~count,text = ~Day,
              textfont = list(color = '#000000', size = 12,family = "Bahnschrift SemiBold")) %>%
      add_pie(hole = 0.4) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(plot_bgcolor='transparent') %>% layout(paper_bgcolor='transparent')
    return(p)
  }
  
  # Generate Attachment Plot
  
  gen.attachments <- function(vect)
  {
    Videos <- length(vect[!is.na(str_extract(vect,'VID-(.*)(file attached)'))])
    Images <- length(vect[!is.na(str_extract(vect,'IMG-(.*)(file attached)'))])
    Audios <- length(vect[!is.na(str_extract(vect,'AUD-(.*)(file attached)'))])
    Docs <- length(vect[!is.na(str_extract(vect,'DOC-(.*)(file attached)'))])
    res <- data.frame("File" = c("Videos","Images","Audios","Documents"),
                      "Count" = c(Videos,Images,Audios,Docs))
    p <- res %>%
      plot_ly(labels = ~File, values = ~Count,text = ~File,
              textfont = list(color = '#000000', size = 12,family = "Bahnschrift SemiBold")) %>%
      add_pie(hole = 0.4) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(plot_bgcolor='transparent') %>% layout(paper_bgcolor='transparent') 
    return(p)
  }
  
  # Generate Word Cloud Plot
  
  gen.wordcloud <- function(vect)
  {
    docs = VCorpus(VectorSource(vect))
    docs = tm_map(docs,stripWhitespace)
    docs = tm_map(docs,removeWords,c(stopwords(),"ma'"))
    dtm <- DocumentTermMatrix(docs)
    df_dtm <- as.data.frame(as.matrix(dtm))
    word_freq <- data.frame("Word" = names(df_dtm),"Freq" = colSums(df_dtm))
    top100Words <- word_freq %>% arrange(-Freq) %>% head(100)
    return(top100Words)
  }
  
  # Clock Plot
  
  clock.plot <- function (x, col = rainbow(n)) {
    x <- BBmisc::normalize(x,method = "range",range = c(0,1))
    n <- length(x)
    if(is.null(names(x))) names(x) <- 0:(n-1)
    m <- 1.05
    par(bg = NA)
    plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '')
    a <- pi/2 - 2*pi/1000*0:1000
    polygon(cos(2*a),sin(2*a),lty = "solid",lwd = 5,border = 4)
    v <- .02
      a <- pi/2 - 2*pi/n*0:n
      segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a),col = "blue",lwd = 2)
      segments( cos(a), sin(a),0, 0, col = 'blue', lty = 3,lwd = 2) 
      ca <- -2*pi/n*(0:50)/50
      for (i in 1:n) {
        a <- pi/2 - 2*pi/n*(i-1)
        b <- pi/2 - 2*pi/n*i
        polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
        v <- .1
        text((1+v)*cos(a), (1+v)*sin(a), names(x)[i],col = "blue")
      }
  }
  
  my_theme <- function(){theme_bw() + 
      theme_light() + 
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA))}
    
    observe({
    
    file <- input$chatfile
    if (is.null(file))
      return(NULL)
    data <- readLines(con = file$datapath)
    vect <- data.celaning(data)
    dataframe <- make.structured(vect)
    
    
    output$chatCount <- renderValueBox({
      valueBox(nrow(dataframe), "Total Count of Chats", icon = icon("send"),color = "green")
    })
    
    userCount <- dataframe %>% group_by(User) %>% summarise("Count" = n()) %>% arrange(-Count)
    
    output$user1Count <- renderValueBox({
      valueBox(userCount$Count[1],userCount$User[1],icon = icon("cloud-upload"),color = "red")
    })
    
    output$user2Count <- renderValueBox({
      valueBox(userCount$Count[2],userCount$User[2],icon = icon("cloud-download"),color = "blue")
    })
  
    output$weekPlot <- renderPlotly({gen.weekplot(dataframe)})
    output$attchmentPlot <- renderPlotly({gen.attachments(vect)})
    output$plot1Header <- renderText("CHAT  ANALYSIS  BY  WEEKDAY")
    output$plot2Header <- renderText("ATTACHMENT  ANALYSIS")
    
    output$plot3Header <- renderText("COMMONLY USED WORDS IN CHAT")
    output$wordcloud <- renderWordcloud2({wordcloud2(gen.wordcloud(dataframe$ChatMessage),backgroundColor = 'rgb(237, 237, 255)',size = 1.3)})
    
    output$plot4Header <- renderText("HOUR WISE CHATS")
    time <- dataframe %>% group_by(Hours) %>% summarise("Count" = n())
    sub_df <- data.frame("Hours" = setdiff(1:24,time$Hours),"Count" = rep(0,length(setdiff(1:24,time$Hours))))
    time <- rbind(time,sub_df) %>% arrange(Hours) %>% select(Count) %>% unlist() %>% unname()
    names(time) <- seq(1,24)
    output$clock <- renderPlot({clock.plot(time)})
    
    output$plot5Header <- renderText("SESSION WISE CHATS")
    output$session <- renderPlotly({
      
      plot_data <- dataframe %>% group_by("Week" = weekdays(Date),Hours) %>% summarise("Count" = n())
      plot_data$Session <- ifelse(plot_data$Hours <= 12,"AM","PM")
      
      p <- ggplot(plot_data,aes(x = Hours,frame = Week)) + geom_density(aes(y =..count..,fill = Session)) +
        theme_bw() + theme(legend.position = "bottom",
                           axis.text = element_blank(),
                           axis.title = element_blank(),
                           axis.ticks = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank())
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.4, y = - 0.05)) %>% 
        layout(plot_bgcolor='transparent') %>% layout(paper_bgcolor='transparent') %>% 
        layout(autosize = F, width = 500, height = 500) %>% layout(autosize = F, width = 1000, height = 500)
      })
    
    output$plot6Header <- renderText("PROPORTION OF SENTIMENTS")
    output$plot7Header <- renderText("TOP WORDS ACROSS SENTIMENTS")
    output$sentiment <- renderPlotly({
      
      sentences <- get_sentences(as.character(dataframe$ChatMessage))
      res <- sentiment_by(sentences)
      res$sentiment <- ifelse(res$ave_sentiment > 0,"Positive",ifelse(res$ave_sentiment == 0,"Neutral","Negative"))
      
      
      res %>% group_by(sentiment) %>% summarise("Count" = n()) %>% 
        plot_ly(labels = ~sentiment, values = ~Count,text = ~paste0(sentiment,"-",Count),
                textfont = list(color = '#000000', size = 12,family = "Bahnschrift SemiBold")) %>%
        add_pie(hole = 0.4) %>%
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        layout(plot_bgcolor='transparent') %>% layout(paper_bgcolor='transparent')
      
      
    })
    
    sentences <- get_sentences(as.character(dataframe$ChatMessage))
    result <- extract_sentiment_terms(sentences)
    neutral <- table(unlist(result$neutral))
    df_neutral <- data.frame("Words" = names(neutral),"Freq" = neutral %>% as.integer())
    
    positive <- table(unlist(result$positive))
    df_positive <- data.frame("Words" = names(positive),"Freq" = positive %>% as.integer())
    
    negative <- table(unlist(result$negative))
    df_negative <- data.frame("Words" = names(negative),"Freq" = negative %>% as.integer())
    
    output$postwords <- renderPlot({
      df_positive %>% arrange(-Freq) %>% head(25) %>% ggplot(aes(x = reorder(Words,Freq), y = Freq)) +
        geom_bar(stat = "identity", fill = "steelblue4",width = 0.75) +
        geom_label(aes(label = Words),fill = "springgreen4",size = 4,fontface = "bold") + 
        geom_text(aes(label = Freq),position = position_stack(vjust = 0.5),color = "white") + 
        coord_flip() + my_theme()
    },bg = "transparent")
    
    output$negtwords <- renderPlot({
      df_negative %>% arrange(-Freq) %>% head(25) %>% ggplot(aes(x = reorder(Words,Freq), y = Freq)) +
        geom_bar(stat = "identity", fill = "steelblue4",width = 0.75) +
        geom_label(aes(label = Words),fill = "springgreen4",size = 4,fontface = "bold") + 
        geom_text(aes(label = Freq),position = position_stack(vjust = 0.5),color = "white") + 
        coord_flip() + my_theme()
    },bg = "transparent")
    
    output$neutwords <- renderPlot({
      df_neutral %>% arrange(-Freq) %>% head(25) %>% ggplot(aes(x = reorder(Words,Freq), y = Freq)) +
        geom_bar(stat = "identity", fill = "steelblue4",width = 0.75) +
        geom_label(aes(label = Words),fill = "springgreen4",size = 4,fontface = "bold") + 
        geom_text(aes(label = Freq),position = position_stack(vjust = 0.5),color = "white") + 
        coord_flip() + my_theme()
    },bg = "transparent")
    
    
    output$plot8Header <- renderText("MOST CONNECTED WORDS IN A CONTEXT")
    output$network <- renderPlot({
      #bigram
      t3 <- dataframe %>% unnest_tokens(bigram,ChatMessage,token="ngrams",n=2)
      t3_filt <- t3 %>% separate(bigram,c("word1","word2"),sep=" ") %>% 
        mutate(word1= lemmatize_words(word1), word2= lemmatize_words(word2)) %>%
        filter(!word1 %in% tm::stopwords()) %>% 
        filter(!word2 %in% tm::stopwords())
      
      t3_count <- t3_filt %>% count(word1,word2,sort=TRUE)
      t3 <- t3_filt %>% unite(bigram, word1,word2,sep=" ") 
      
      #Network graph
      bigram_graph <- t3_filt%>%count(word1, word2, sort = TRUE)%>%
        filter(n > 7)%>% graph_from_data_frame()
      
      a <- grid::arrow(type = "closed",length = unit(.15, "inches"))
      
      p <- ggraph(bigram_graph,layout = "nicely")+
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,arrow = a, end_cap = circle(.1, 'inches')) +
        geom_node_point(color = "steelblue4", size = 4) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1,color = "orangered2",size = 8,family = "bold") +
        my_theme()
        
      return(p)
      },bg = "transparent")
    
    
  })
    
  session$onSessionEnded(function() {
    stopApp()
  })
  
}



 
 