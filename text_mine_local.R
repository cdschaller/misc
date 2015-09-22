# libraries
library(foreach)
library(doMC)

# vector to be used
v <- pbp$provision

# helper functions
# clean the vector
clean = function(vector) {
  vector <- tolower(vector)
  vector <- gsub('([0-9])\\.|\\,([0-9])', '\\1\\2', vector)
  vector <- gsub('[[:space:][:punct:]]+', ' ', vector)
  gc()
  return(vector)
}
# get bi-, tri-, etc grams
ngrams = function(vector, n) {
  once = function(splits, n) {
    size <- length(splits)
    w1 <- splits[1:(size - n + 1)]
    for (i in 2:n) {
      t <- splits[i:(size - n + i)]
      w1 <- paste(w1, t)
    }
    return(w1)
  }
  ng <- lapply(vector, function(x) once(x, n))
  ng <- table(unlist(ng))
  ng <- sort(ng, decreasing = TRUE)
  gc()
  return(ng)
}
# create tfidf matrices
tfidf = function(strings, vector) {
  vec = function(string, vec) {
    s <- gregexpr(string, vec)
    l <- lapply(s, function(x) sum(x > 0))
    l <- unlist(l)
    return(l)
  }
  vec1 <- paste('', vector, '')
  str1 <- paste('', strings, '')
  registerDoMC(cores = 30) # register cores
  freq <- foreach(i = str1) %dopar% vec(i, vec1)
  freq <- do.call('cbind', freq)
  idf <- log(nrow(freq)/colSums(freq>0))
  tfidf <- t(freq)*idf
  tfidf <- as.data.frame(t(tfidf))
  setnames(tfidf, strings)
  gc()
  return(tfidf)
}

# execute
vector <- clean(v) # clean vector
splits <- strsplit(vector, ' ') # split it
words <- table(unlist(splits)) # get unique words & frequencies
words <- sort(words, decreasing = TRUE) # sort
bigrams <- ngrams(splits, 2) # get bigrams
trigrams <- ngrams(splits, 3) # get trigrams
w_tfidf <- tfidf(names(words), vector) # create tfidf from words
bg_tfidf <- tfidf(names(bigrams), vector) # "     "     " bigrams
tg_tfidf <- tfidf(names(trigrams), vector) #"     "     " trigrams