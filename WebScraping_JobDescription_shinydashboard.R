---
title: "Word Cloud for Job Description in Indeed"
output: html_document
runtime: shiny
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
#library(semantic.dashboard)
library(wordcloud2)
library(tm)
library(xml2)
library(rvest)
library(dbplyr)
```

```{r echo=FALSE, warning=FALSE}
ui <- dashboardPage( #skin = "yellow", # shinydashboard
  #theme = "slate", # sematic.dashboard
  dashboardHeader(title = "Word Cloud Indeed Job Description"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search ..."),
      menuItem("Instruction", tabName = "int" , icon = icon("cloud")),
      menuItem("Word Cloud", tabName = "wc" , icon = icon("cloud")))),
      #menuItem("Web Page", tabName = "page" , icon = icon("tree")))),
  dashboardBody(
    tabItems(
      tabItem("int",
              h1("Instruction"),
              p("Please enter you link in the search box"),
              p("Example link:"),
              a("https://nz.indeed.com/viewjob?jk=b6a4bad8be1b8082&tk=1ffgu5sd4rj8q800&from=serp&vjs=3")
              ),
      tabItem("wc",
              fluidRow(
              h2("Word Cloud"),
              box(
                  status = "primary",
                  width = 16, 
                  wordcloud2Output("wcplot")
                  )
              )
              ))
      #tabItem("page",
      #        fluidPage(
      #           h2("Web Page"),
      #           box(
      #              htmlOutput("page")
      #              #verbatimTextOutput(outputId = "page")
                 ))

```


```{r echo=FALSE, warning=FALSE}
server <- function(input, output){
   wc_data <- reactive({
      input$searchButton
      
      isolate({
         withProgress({
            setProgress(message = "Processing ...")
            wc_url <- input$searchText
            if (!is.null(wc_url)) {
               wc_text <- read_html(input$searchText) %>%
                            html_nodes("body")%>%
                            xml_find_all("//div[contains(@class, 'jobsearch-jobDescriptionText')]") %>%
                            html_text()
            } 
            else
            {
               wc_text <-"A word cloud is an image of words that together resemble a cloudy shape. The size of a word shows how important it is."
            }
             wc_corpus <- Corpus(VectorSource(wc_text)) %>%
                           tm_map(removeNumbers) %>%
                           tm_map(removePunctuation) %>%
                           tm_map(content_transformer(tolower)) %>%
                           tm_map(removeWords, stopwords("english"))%>%
                           tm_map(removeWords, stopwords("SMART"))%>%
                           tm_map(stripWhitespace)
         })
      })
   })
   

 
   
   
 output$wcplot <- renderWordcloud2({
    if (input$searchButton > 0 ){
        wc_corpus <- wc_data()
   tdm <- TermDocumentMatrix(wc_corpus) %>%
             as.matrix()
   words <- sort(rowSums(tdm), decreasing = TRUE)
   df <- data.frame(word = names(words), freq = words)
    wordcloud2(df)
      
    }
 })
 

# getPage <- function() {
#     return((HTML(readLines(input$searchText))))
# }
 
# getPage <- function(){
#   return(tags$iframe(src=input$searchText,
#                       id = "iframe",
#                       style = "width:100%;",
#                       frameborder = "0",
#                       height = "500px"))
# }
#output$page <- renderUI({
#                        validate(need(input$searchButton, message=FALSE))
#                        tags$iframe(src=isolate(input$searchText), 
#                        height=600, width=535)
#                       })
  
#output$page <- renderPrint({
#   shell.exec(input$searchText)
#})
 
} 
```


```{r echo=FALSE, warning=FALSE}
shinyApp(ui = ui, server = server, options = list(height=1300))
```
