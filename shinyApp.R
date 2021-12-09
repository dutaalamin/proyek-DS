library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(plotly)
library(here)
library(vroom)
library(dplyr)
library(tidyverse)
library(tm)
library(wordcloud)
library(RTextTools)
library(e1071)
library(syuzhet)
library(caret)
library(tidymodels)
library(textdata)
library(tidytext)
library(forcats)

#mengambil df
tweetclean = read.csv("tweetclean_df.csv", stringsAsFactors = FALSE)

#mengubah kolom text sebagai char
tweet <- as.character(tweetclean$text)

##----------------------------------------------batas edit-----------------------------------------------------##

#Penentuan hasil sentimen analisis NRC dari tweet
sentiment<-get_nrc_sentiment(tweet, language = "english")
tweetsentiment<-cbind(tweetclean$text,sentiment)

#Pembentukan value untuk dibentuk menjadi df 'data' 
sentiment_class <- data.frame(negative=sentiment$negative,positive=sentiment$positive)
classify <- mutate(sentiment_class, text_sentiment = ifelse((sentiment_class$negative != sentiment_class$positive),
                                                     ifelse(sentiment_class$negative!=0,print("negative"),
                                                     print("positive")),
                                                     print("neutral")))

data <- data.frame(text=tweet,sentiment=classify$text_sentiment)



ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel( h1("Tweet about Travelling During Pandemic Situation")),
  mainPanel(
       tabsetPanel(type = "tabs",
                tabPanel("Tweet Data", DT::dataTableOutput('data')), # Output Data Dalam Tabel
                tabPanel("NRC Sentiment Analysis", plotOutput("nrc")), # Plot
                tabPanel("Bing Sentiment Analysis", plotOutput("bing")), # Plot
                tabPanel("Frequent Word", plotOutput('freq')), # Plot
                tabPanel("Wordcloud", plotOutput("Wordcloud")) # Plot Wordcloud
    ),
    checkboxInput(
      inputId = "themeToggle",
      label = icon("sun")
    )
  ),
  tags$script(
    "
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);


        // define event - checked === 'light'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
            }  else {
                // else add darktheme
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }
        })
        "
  )
)

# SERVER
server <- function(input, output, session) {
  
  
  
  # Output Data tabel
  output$data = DT::renderDataTable({
    
    DT::datatable(data, options = list(lengthChange = FALSE))
  })
  
  
  # Analisis sentimen NRC
  output$nrc <- renderPlot({
    tweetsentiment<-cbind(tweetclean$text,sentiment)
    par(mar=rep(3,4))
    barplot(colSums(sentiment),col=rainbow(5),ylab='count',main='Analisis Sentimen NRC')
  }, height=400)
  
  #Analisis sentimen bing
  output$bing <- renderPlot({
    
    separated <- data %>% 
     filter(text != "nan") %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) 
    
    bing_word_counts <- separated %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    bing_word_counts %>%
      group_by(sentiment) %>%
      slice_max(n, n=10) %>%
      ungroup() %>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(n,word,fill=sentiment))+
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(x = "Contribution to sentiment",
           y = NULL)
    
   }, height=400)
  
  #Output kata yang sering muncul
    output$freq <- renderPlot({
    corpus <- Corpus(VectorSource(tweetclean$text))
    corpus<-tm_map(corpus, removeWords, c("travelling","pandemic","travel"))

    tdm<-TermDocumentMatrix(corpus)
    m<-as.matrix(tdm)
    v<-sort(rowSums(m),decreasing = TRUE)
    d<-data.frame(word=names(v),freq=v)
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
           main ="Frequent words",
            ylab = "Frequency")
  })
  
  #wordcloud
  output$Wordcloud <- renderPlot({
    
    corpus <- Corpus(VectorSource(tweetclean$text))
    corpus<-tm_map(corpus, removeWords, c("travelling","pandemic","travel"))
   
    wordcloud(corpus, random.order = F, max.words=100 , col=rainbow(100))
    })
}


shinyApp(ui = ui, server = server, options = list(height = "1080px"))
