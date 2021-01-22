#panggil library
library(shiny)
library(shinycssloaders)
library(wordcloud2)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(DT)

#memanggil data-data yang digunakan
nbClassifier <- load("NaiveBayesClassifier.rda")
attraction <- readRDS("attraction.rds")
option_rest <- attraction$name
source("attractionreview.R")
source("featureExtraction.R")

#tampilan-tampilan
ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "Review Wisata Yogyakarta"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("Perbandingan Wisata", tabName = "da", icon = icon("poll"))
    ), 
    fluidPage(
      hr(),
      helpText(
        "Data review wisata hasil dari web-scarpping ",
        a("Tripadvisor", href = "https://www.tripadvisor.com/"),
        " dan di klasifikasikan dengan Naive Bayes."
      )
    )  
  ),
  
  dashboardBody(
    tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#c68493
                    }
                    .box.box-solid.box-primary{
                     border-bottom-color:#c68493;
                    border-left-color:#c68493;
                    border-right-color:#c68493;
                    border-top-color:#c68493;
                    background:#c68493
                    }
                    ")),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            selectInput(
              "selectattraction",
              label = h5("Pilih wisata"),
              setNames(attraction$link, attraction$name)
            ),
            status = "primary",
            width=12,
            solidHeader = T
          )
        ),
        fluidRow(
          valueBoxOutput("total_review"),
          valueBoxOutput("happy_review"),
          valueBoxOutput("not_happy_review")
        ),
        fluidRow(
          box(
            title = "Wisata Review dan Klasifikasi Sentimen",
            solidHeader = T,
            width = 12,
            collapsible = T,
            div(DT::dataTableOutput("table_review") %>% withSpinner(color="#1167b1"), style = "font-size: 70%;")
          ),
        ),
        fluidRow(
          box(title = "Wordcloud",
              solidHeader = T,
              width = 6,
              collapsible = T,
              wordcloud2Output("wordcloud") %>% withSpinner(color="#1167b1")
          ),
          box(title = "Word Count",
              solidHeader = T,
              width = 6,
              collapsible = T,
              plotOutput("word_count") %>% withSpinner(color="#1167b1")
          )
        ),
        fluidRow(
          box(title = "Sentimen Negatif / Positif yang Paling Umum",
              solidHeader = T,
              width = 12,
              collapsible = T,
              plotOutput("kontribusi_sentimen") %>% withSpinner(color="#1167b1")
          )
        )
      ),
      tabItem(
        tabName = "da",
        fluidRow(
          box(
            solidHeader = T,
            width = 12,
            status = "primary",
            selectInput(
              "attraction_1",
              label = "Pilih Tempat Wisata", 
              #choices = attraction$link,
              setNames(attraction$link, attraction$name),
              multiple = TRUE
            )
          )
        ),
        fluidRow(
          box(title = "Plot Satisfied",
              status = "primary",
              solidHeader = T,
              width = 12,
              collapsible = T,
              plotOutput("banding") %>% withSpinner(color="#1167b1")
          ),
        ),
      )
    )
  )
)

server <- function(input, output) {
  
  #memanggil wisata yang dipilih  
  data <- reactive({
    get_attraction_reviews(input$selectattraction)
  })
  
  #data tersebut kita simpan dengan nama dataNB
  dataNB <- reactive({
    reviews <- data()$Review
    withProgress({
      setProgress(message = "Proses Ekstraksi Fitur...")
      newData <- extract_feature(reviews)
    })
    
    #diklasifikasikan sesuai prediksi
    withProgress({
      setProgress(message = "Klasifikasi...")
      pred <- predict(get(nbClassifier), newData)
    })
    
    #data tersebut disimpan dalam satu frame
    data.frame(name=data()$name, Review = data()$Review, Prediksi = as.factor(pred), stringsAsFactors = FALSE)
  })
  
  dataWord <- reactive({
    v <- sort(colSums(as.matrix(create_dtm(data()$Review))), decreasing = TRUE)
    data.frame(Kata=names(v), Jumlah=as.integer(v), row.names=NULL, stringsAsFactors = FALSE) %>%
      filter(Jumlah > 0)
  })
  
  #dataNB tadi kita tampilkan dalam sebuah tabel
  output$table_review <- renderDataTable(datatable({
    dataNB()
  }))
  
  output$total_review <- renderValueBox({
    valueBox(
      "Total", 
      paste0(nrow(dataNB()), " Review"),
      color = "red"
    )
  })
  
  output$happy_review <- renderValueBox({
    valueBox(
      "Satisfied", 
      paste0(nrow(dataNB() %>% filter(Prediksi == "1")), " Pengunjung Senang"),
      icon = icon("smile"),
      color = "fuchsia")
  })
  
  output$not_happy_review <- renderValueBox({
    valueBox(
      "Unsatisfied",
      paste0(nrow(dataNB() %>% filter(Prediksi == "0")), " Pengunjung Tidak Senang"), 
      icon = icon("frown"),
      color = "blue")
  })
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(top_n(dataWord(), 50, Jumlah))
  })
  
  output$word_count <- renderPlot({
    countedWord <- dataWord() %>%
      top_n(10, Jumlah) %>%
      mutate(Kata = reorder(Kata, Jumlah))
    
    ggplot(countedWord, aes(Kata, Jumlah, fill = -Jumlah)) +
      geom_col() +
      guides(fill = FALSE) +
      theme_minimal()+
      labs(x = NULL, y = "Word Count") +
      ggtitle("Most Frequent Words") +
      coord_flip()
  })
  
  output$kontribusi_sentimen <- renderPlot({
    sentiments <- dataWord() %>% 
      inner_join(get_sentiments("bing"), by = c("Kata" = "word"))
    
    positive <- sentiments %>% filter(sentiment == "positive") %>% top_n(10, Jumlah) 
    negative <- sentiments %>% filter(sentiment == "negative") %>% top_n(10, Jumlah)
    sentiments <- rbind(positive, negative)
    
    sentiments <- sentiments %>%
      mutate(Jumlah=ifelse(sentiment =="negative", -Jumlah, Jumlah))%>%
      mutate(Kata = reorder(Kata, Jumlah))
    
    ggplot(sentiments, aes(Kata, Jumlah, fill=sentiment))+
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("Kontibusi Sentimen")
  })
  
  
  banding <- reactive({
    dataa = unlist(input$attraction_1)
    dataa<-c(dataa)
    dataa <- data.frame(nama=dataa)
    jum <- nrow(dataa)
    a<- c()
    b<-c()
    d<- c()
    j=1
    while (jum!=0) {
      data_1 <- reactive({
        get_attraction_reviews(dataa$nama[j])
      })
      dataNB_1 <- reactive({
        reviews <- data_1()$Review
        withProgress({
          setProgress(message = "Proses Ekstraksi Fitur...")
          newData <- extract_feature(reviews)
        })
        withProgress({
          setProgress(message = "Klasifikasi...")
          pred <- predict(get(nbClassifier), newData)
        })
        
        data.frame(name=data_1()$name, Review = data_1()$Review, Prediksi = as.factor(pred), stringsAsFactors = FALSE)
      })
      tot=nrow(dataNB_1())
      for (i in 1:tot) {
        b<-c(b,paste("wisata",j))
      }
      a<-c(a,dataNB_1()$Review)
      d<-c(d,dataNB_1()$Prediksi)
      j=j+1
      jum=jum-1
    }
    hasil <- data.frame(name=b, Review = a, prediksi = as.factor(d), stringsAsFactors = FALSE)
    hasil$prediksi<- ifelse(hasil$prediksi=='1',hasil$prediksi<-"Unsatiesfied",hasil$prediksi<-"Satisfied")
    
    hasil %>%
      ggplot() +
      geom_bar(aes(x = name, fill=as.factor(prediksi)), 
               position = "dodge", stat = "count") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(
        x= "wisata",
        y= "Jumlah",
        fill="Klasifikasi"
      )+
      theme_light()
    
  })
  output$banding <- renderPlot({
    banding()
  })
  output$banding_2 <- renderPlot({
    banding_2()
  })
}

shinyApp(ui=ui, server=server)