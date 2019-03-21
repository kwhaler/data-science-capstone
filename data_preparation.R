
## Load the applicable libraries

library(tidyverse)
library(tidytext)
library(knitr);
library(NLP);
library(tm);
library(tau);
library(slam);
library(RColorBrewer);
library(wordcloud);
library(SnowballC);

setwd("~/Quest/OneDrive - Quest/Data Science Capstone")

## Get the data
file_dest <- "./source/Coursera-SwiftKey.zip"
file_src  <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

## Save time, download only if you haven't
if(!file.exists(file_dest)) {
  download.file(file_src, file_dest)
}

## Save time, unzip only if you haven't
if(file.exists(file_dest)) {
  unzip(file_dest, exdir = "./source/")
  list.files("./source/final/")
  list.files("./source/final/en_US/")
}

## Read as UTF-8 and create tibbles (dataframes)
text_blog <- readLines("./source/final/en_US/en_US.blogs.txt", encoding = "UTF-8")
df_text_blog <- as_tibble(text_blog)
colnames(df_text_blog) <- "text"

text_news <- readLines("./source/final/en_US/en_US.news.txt", encoding = "UTF-8")
df_text_news <- as_tibble(text_news)
colnames(df_text_news) <- "text"

text_twit <- readLines("./source/final/en_US/en_US.twitter.txt", encoding = "UTF-8")
df_text_twit <- as_tibble(text_twit)
colnames(df_text_twit) <- "text"

files_list <- list.files();
sample_size <- 0.3; ## Sample 30%

for (file_name in files_list) {
   file_con <- file(file_name, "rb");
   file_lines <- readLines(file_con, encoding="UTF-8", skipNul=TRUE);
   file_sample_lines <-  sample(file_lines, length(file_lines)*sample_size);
   file_tmp <- gsub("[^[:print:]]","",file_sample_lines);
   file_tmp2 <- gsub("?|!+|\\.+|;|#|&","",file_tmp);
   file_tmp <- gsub("\\|\\/", " ", file_tmp2);
   file_corpus <- gsub(":\\)|\\(:|;\\)|\\(;", "", file_tmp);
   doc_vs <- VectorSource(file_corpus);
   close(file_con);
}

docs <- Corpus(doc_vs);
rm(file_lines);
rm(file_sample_lines);
rm(file_corpus);

docs <- tm_map(docs, content_transformer(tolower));
docs <- tm_map(docs, removeNumbers);
docs <- tm_map(docs, stripWhitespace);

term_matrix_one_gram <- TermDocumentMatrix(docs);
term_matrix_one_gram_r <- rollup(term_matrix_one_gram, 2, na.rm=TRUE, FUN = sum);
term_matrix_one_gram_frequency <- sort((as.matrix(term_matrix_one_gram_r)[,1]),decreasing=TRUE);
dfile_one_gram <- data.frame(
  words=names(term_matrix_one_gram_frequency), 
  freq=term_matrix_one_gram_frequency, 
  stringsAsFactors = FALSE);
one_gram_term <- subset(df_one_gram, freq > 3);
one_gram_number <- rownames(one_gram_term);

two_gram <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE);
term_matrix_two_gram <- TermDocumentMatrix(docs, control = list(tokenize = two_gram, bounds = list(global = c(1,Inf))));
term_matrix_two_gram_r <- rollup(term_matrix_two_gram, 2, na.rm=TRUE, FUN = sum);
term_matrix_two_gram_frequency <- sort((as.matrix(term_matrix_two_gram_r)[,1]),decreasing=TRUE);
df_two_gram <- data.frame(
  words=names(term_matrix_two_gram_frequency), 
  freq=term_matrix_two_gram_frequency, 
  stringsAsFactors = FALSE);
two_gram_term <- subset(df_two_gram, freq > 3);
two_gram_number <- rownames(two_gram_term);

three_gram <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE);
term_matrix_three_gram <- TermDocumentMatrix(docs, control = list(tokenize = three_gram, bounds = list(global = c(1,Inf))));
term_matrix_three_gram_r <- rollup(term_matrix_three_gram, 2, na.rm=TRUE, FUN = sum);
term_matrix_three_gram_frequency <- sort((as.matrix(term_matrix_three_gram_r)[,1]),decreasing=TRUE);
df_three_gram <- data.frame(
  words=names(term_matrix_three_gram_frequency), 
  freq=term_matrix_three_gram_frequency, 
  stringsAsFactors = FALSE);
three_gram_term <- subset(df_three_gram, freq > 3);
three_gram_number <- rownames(three_gram_term);

four_gram <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE);
term_matrix_four_gram <- TermDocumentMatrix(docs, control = list(tokenize = four_gram, bounds = list(global = c(1,Inf))));
term_matrix_four_gram_r <- rollup(term_matrix_four_gram, 2, na.rm=TRUE, FUN = sum);
term_matrix_four_gram_frequency <- sort((as.matrix(term_matrix_four_gram_r)[,1]),decreasing=TRUE);
df_four_gram <- data.frame(
  words=names(term_matrix_four_gram_frequency), 
  freq=term_matrix_four_gram_frequency, 
  stringsAsFactors = FALSE);
four_gram_term <- subset(df_four_gram, freq > 3);
four_gram_number <- rownames(four_gram_term);

save(list=c('one_gram_term','one_gram_number'), file="one-gram.Rda");
save(list=c('two_gram_term','two_gram_number'), file="two-grams.Rda");
save(list=c('three_gram_term','three_gram_number'), file="three-grams.Rda");
save(list=c('four_gram_term','four_gram_number'), file="four-grams.Rda");

save("df_three_gram", file="df_three_gram.Rda");
save("df_four_gram", file="df_four_gram.Rda");
save("df_two_gram", file="df_two_gram.Rda");
save("df_one_gram", file="df_one_gram.Rda")