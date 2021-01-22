library(rvest)

#pengambilan data review
baseUrl <- "https://www.tripadvisor.com"
get_attraction_reviews <- function(attractionUrl) {
  withProgress(message = 'Scrape Tripadvisor', value = 0, {
    
    reviewPage <- read_html(paste(baseUrl, attractionUrl, sep = ""))
    
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
    
    #kita batasi setiap attraction diambil data 100 review saja 
    #dengan melihat jml link review dan jml review atau komentar
    while (!is.na(nextPage) & length(reviews) < 100) {
      incProgress(1/100, detail = paste("jumlah review : ", length(reviews)))
      
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
    data.frame(name =reviewers, Review = reviews, stringsAsFactors = FALSE)
  })
}