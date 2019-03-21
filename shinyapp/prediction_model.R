
## For shiny
## setwd("~/")

## Save space with Rds
load(file = "texts/one-gram.Rda");
load(file = "texts/two-gram.Rda");
load(file = "texts/three-gram.Rda");
load(file = "texts/four-gram.Rda");

last_word <- function(x) {
  dat <- vector();
  lgt <- length(x);
  if ( lgt == 0 ) {
    return(dat)
  }

  for ( i in 1:lgt ) {
    wds <- unlist(strsplit(x[i]," +"));    
    dat <- c(dat,toString(wds[length(wds)]));
  } 
  return(dat);
}

last_two_words <- function(x) {
  dat <- vector();
  lgt <- length(x);
  if ( lgt == 0 ) {
    return(dat)
  }

  for ( i in 1:lgt ) {
    wds <- unlist(strsplit(x[i]," +"));
    lgt <- length(wds);
    dat <- c(dat,toString(paste(wds[lgt - 1], wds[lgt] )));
  } 
  return(dat);
}

predicted_word <- function(text_input, n) {
  
  if ( nchar(text_input) == 0 ) { 
    return("Type a few words to get going") 
    }

  unlist_word <- unlist(strsplit(text_input," +"));
  number_of_words <- length(unlist_word);

  if (number_of_words >= 2) {
    two_word_string_pattern <- paste0("^", unlist_word[number_of_words - 1], " ", unlist_word[number_of_words], " "); 
    one_word_string_pattern <- paste0("^", unlist_word[number_of_words], " "); 
    dat <- last_two_words(head(t4[grep(two_word_string_pattern, n4, value = TRUE),]$words, n));
    if (length(dat) == 0 ) {
      dat <- last_two_words(head(t3[grep(one_word_string_pattern, n3, value = TRUE),]$words, n));
    }
  }
  else {
    one_word_string_pattern <- paste("^", unlist_word[1], " ", sep="");
    dat <- last_two_words(head(t3[grep(one_word_string_pattern, n3, value = TRUE),]$words, n));
  }
  
  return(dat);
  
}