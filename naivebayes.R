# Load library
library(tidyverse)
library(tm)
library(e1071)
library(caret)
library(dplyr)

# Load dataset
my_Dataset <- read.csv("Attraction_Reviews.csv", stringsAsFactors = FALSE)

glimpse(my_Dataset)

# Ambil kolom reviewnya dan Liked 
#kolom review kita beri nama text sedang kolom liked class
attraction_review <- my_Dataset %>%
  select(text = Review, class = Liked)

attraction_review$class <- as.factor(attraction_review$class)

glimpse(attraction_review)

# kita lakukan filter berdasar class data 
like_review <- attraction_review %>%
  filter(class == "1") 


dislike_review <- attraction_review %>%
  filter(class == "0") 


#kita gabungkan dataframe tsb dengan rbind shg nanti akan urut 
#500 data yg masuk like dan 500 data dislike
attraction_review <- rbind(like_review, dislike_review)

attraction_review %>% count(class)

view(attraction_review)

# Acak data set agar tidak berurutan
set.seed(10)
attraction_review <- attraction_review[sample(nrow(attraction_review)), ]

# CLEANING DATASET
# Mengubah data reviewnya ke bentuk corpus
corpus <- Corpus(VectorSource(attraction_review$text))

# Cleaning
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

corpus[[5]]$content
corpus_clean[[5]]$content

# Mengubah corpus jadi dtm
dtm <- DocumentTermMatrix(corpus_clean)

# Partisi 3:1 data untuk training dan test
attraction_review_train <- attraction_review[1:700,]
attraction_review_test <- attraction_review[701:1000,]

corpus_clean_train <- corpus_clean[1:700]
corpus_clean_test <- corpus_clean[701:1000]

dtm_train <- dtm[1:701,]
dtm_test <- dtm[701:1000,]

dim(dtm_train)
dim(dtm_test)

#karna reviewnya panjang, maka u/ memudahkan kita akan menggunakan data yg diseleksi
# Feature Selection, ambil kata yang muncul minimal 5 kali
fiveFreq <- findFreqTerms(dtm_train, 5)
fiveFreq
length(fiveFreq)

# save featurenya
saveRDS(fiveFreq, "features.rds")


# Sesuaikan fitur pada data train dan test dengan fitur yang sudah diseleksi sebelumnya
dtm_train_nb <- corpus_clean_train %>%
  DocumentTermMatrix(control=list(dictionary = fiveFreq))

dtm_test_nb <- corpus_clean_test %>%
  DocumentTermMatrix(control=list(dictionary = fiveFreq))

dim(dtm_train_nb)
dim(dtm_test_nb)

# Funsi untuk menampilkan kemunculan kata dgn yes (ada) dan no (ga ada)
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm_train_nb, 2, convert_count)
testNB <- apply(dtm_test_nb, 2, convert_count)

view(testNB)

# Membuat model naive bayes dari data training
classifier <- naiveBayes(trainNB, attraction_review_train$class, laplace = 1)

# save model untuk di gunakan pada aplikasi
save(classifier , file = 'NaiveBayesClassifier.rda')

# test model naivebayes nya
pred <- predict(classifier, newdata=testNB)

# Buat table hasil prediksi
table("Predictions"= pred,  "Actual" = attraction_review_test$class)

# Confusion Matrix
conf_mat <- confusionMatrix(pred, attraction_review_test$class)
conf_mat$overall['Accuracy']