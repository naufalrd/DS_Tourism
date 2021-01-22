library(rvest)
library(purrr)
library(textclean)
library(tokenizers)
library(wordcloud)
library(corpus)
library(dplyr)
library(tm)

baseUrl <- "https://www.tripadvisor.com"
attractionUrl <- "/Attractions-g294230-Activities-a_allAttractions.true-Yogyakarta_Region_Java.html"
url <- paste(baseUrl, attractionUrl, sep = "")
webpage <- read_html(url)

attractionName <- webpage %>% html_nodes('[class="_1QKQOve4"]') %>% html_text()
attractionReviewURL <- webpage %>% html_nodes('[class="_1QKQOve4"]') %>% html_attr('href')

dfattraction <- data.frame(name = attractionName, link = attractionReviewURL, stringsAsFactors = FALSE)

View(dfattraction)

# set direktori untuk simpan data
setwd("C:/Users/PAVILION GAMING/OneDrive/Desktop/Pariwisata Yogyakarta")

# simpan data
saveRDS(dfattraction, "wisatajogja.rds")
write.csv(dfattraction,"wisatajogja.csv", row.names = FALSE)
dok<-read.csv("wisatajogja.csv" , stringsAsFactors = TRUE)

#ubah file csv dalam bentuk corpus
corpusdok <- Corpus(VectorSource(dok$name))
inspect(corpusdok[1:10])
#Cleaning Hashtag
remove.hashtag <- function(x) gsub("#\\S+", "", x)
dok_hashtag <- tm_map(corpusdok, remove.hashtag)
inspect(dok_hashtag[1:10])
#Cleaning Punctuation
dok_punctuation<-tm_map(dok_hashtag,content_transformer(removePunctuation))
inspect(dok_punctuation[1:10])
#Cleaning Number
dok_nonumber<-tm_map(dok_punctuation, content_transformer(removeNumbers))
inspect(dok_nonumber[1:10])


df_attraction=data.frame(name=unlist(sapply(dok_nonumber, `[`)),link = attractionReviewURL, stringsAsFactors=F) 
saveRDS(df_attraction, "attraction.rds")

View(df_attraction)

# ambil semua review dari wisata pertama (diterapkan di shinynya)
dfattraction$name[1]
reviewUrl <- paste(baseUrl, dfattraction$link[1], sep = "")
reviewPage <- read_html(reviewUrl)

review <- reviewPage %>%
  html_nodes('.IRsGHoPm') %>%
  html_text()

reviewer <- reviewPage %>%
  html_nodes('._1r_My98y') %>%
  html_text()

reviews <- character()
reviewers <- character()
reviews <- c(reviews, review)
reviewers <- c(reviewers, reviewer)

nextPage <- reviewPage %>%
  html_nodes('.next') %>%
  html_attr('href')

while (!is.na(nextPage)) {
  reviewUrl <- paste(baseUrl, nextPage, sep = "")
  reviewPage <- read_html(reviewUrl)
  
  review <- reviewPage %>%
    html_nodes('.IRsGHoPm') %>%
    html_text()
  
  reviewer <- reviewPage %>%
    html_nodes('._1r_My98y') %>%
    html_text()
  
  reviews <- c(reviews, review)
  reviewers <- c(reviewers, reviewer)
  
  nextPage <- reviewPage %>%
    html_nodes('.next') %>%
    html_attr('href')
}
p <- length(reviewers)
reviews <- reviews[1:p]
datareview <- data.frame(reviews, reviewers, stringsAsFactors = FALSE)

length(reviews)
View(datareview)
