#install.packages("tm")
#install.packages("SnowballC")
#install.packages("RWeka")
library(tm)
library(SnowballC)
library(RWeka)
library(dplyr)
# Read all the datasets into R
twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)

# Convert UTF-8 to ASCII
twitter <- iconv(twitter, 'UTF-8', 'ASCII', "byte")
blogs <- iconv(blogs, 'UTF-8', 'ASCII', "byte")
news <- iconv(news, 'UTF-8', 'ASCII', "byte")

# Not really necessary but just in case there are still empty entries
twitter <- (twitter[!is.na(twitter)])
blogs <- (blogs[!is.na(blogs)])
news <- (news[!is.na(news)])

# Save them for faster loading
save(twitter, file="twitter.RData")
save(blogs, file="blogs.RData")
save(news, file="news.RData")

all <- c(twitter,blogs,news)
save(all, file="./final/all.RData")

# Load whichever is appropriate
# Note that load() will read and restore the variable also
load("./final/all.RData")
#load("twitter.RData")
#load("blogs.RData")
#load("news.RData")

# All data has more than 4 million entries, this will kill" my poor
# little machine, just choose whichever is appropriate for your
# machine.
smpl <- sample(all,100000)
save(smpl, file="sample-100k.RData")
smpl <- sample(all,10000)
save(smpl, file="sample-10k.RData")
smpl <- sample(all,5000)
save(smpl, file="sample-5k.RData")
smpl <- sample(all,2000)
save(smpl, file="sample-2k.RData")
smpl <- sample(all,1000)
save(smpl, file="sample-1k.RData")
smpl <- sample(all,100)
save(smpl, file="sample-100.RData")

#length(all)
# To use 10% of the available data, which is around 400k
smpl <- sample(all,round(0.1*length(all)))
save(smpl, file="sample-10p.RData")

smpl <- sample(all,round(0.02*length(all)))
save(smpl, file="sample-2p.RData")

# To use 1% of the available data, which is around 40k
smpl <- sample(all,round(0.01*length(all)))
save(smpl, file="sample-1p.RData")

# To use 0.1% of the available data, which is around 4k
smpl <- sample(all,round(0.001*length(all)))
save(smpl, file="sample-0.1p.RData")

# Just load() it and it will load very quickly into the appropriate variable
load("sample-0.1p.RData")

# Let's clean up the R Workspace
rm(all)
rm(news)
rm(blogs)
rm(twitter)
gc()

# Now we create the Corpus from it
# You can create a file with the profanity words and read it into a vector of char

#profanity <- c("bad", "words", "that", "cannot", "display", "here")

corpus <- Corpus(VectorSource(smpl))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
#corpus <- tm_map(corpus, stemDocument)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, removeWords, profanity)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

# Save the corpus for quick loading.
save(corpus,file="sampleCorpus.RData")
save(corpus,file="sampleCorpus2p.RData")
# Let's create a few n-gram functions
# We will use the RWeka library

#library(RWeka)
unigram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 3, max = 3))
fourgram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 4, max = 4))
fivegram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 5, max = 5))

load("sampleCorpus2p.RData")
#library(tm)
unigram <- TermDocumentMatrix(corpus, control=list(tokenize=unigram_token))
save(unigram,file="unigram.RData")

bigram <- TermDocumentMatrix(corpus, control=list(tokenize=bigram_token))
save(bigram,file="bigram.RData")

trigram <- TermDocumentMatrix(corpus, control=list(tokenize=trigram_token))
save(trigram,file="trigram.RData")

fourgram <- TermDocumentMatrix(corpus, control=list(tokenize=fourgram_token))
save(fourgram,file="fourgram.RData")

fivegram <- TermDocumentMatrix(corpus, control=list(tokenize=fivegram_token))
save(fivegram,file="fivegram.RData")

install.packages('slam')
library(slam)
load('ngram.RData')
load('unigram.RData')
load('bigram.RData')
load('trigram.RData')
load('fourgram.RData')
load('fivegram.RData')
inspect(trigram)

unigramDF <- as.data.frame(row_sums(unigram, na.rm = T))
unigramDF <- add_rownames(unigramDF, "unigram")
unigramDF <- rename(unigramDF, freq = `row_sums(unigram, na.rm = T)`)

bigramDF <- as.data.frame(row_sums(bigram, na.rm = T))
bigramDF <- add_rownames(bigramDF, "bigram")
bigramDF <- rename(bigramDF, freq = `row_sums(bigram, na.rm = T)`)
bigramDF$name <- lapply(strsplit(bigramDF$bigram," "), function(x){tail(x, n=1)})
bigramDF$unigram <- lapply(strsplit(bigramDF$bigram," "), 
                           function(x){paste(head(x, n =1), collapse = " ")})

trigramDF <- as.data.frame(row_sums(trigram, na.rm = T))
trigramDF <- add_rownames(trigramDF, "trigram")
trigramDF <- rename(trigramDF, freq = `row_sums(trigram, na.rm = T)`)
trigramDF$name <- lapply(strsplit(trigramDF$trigram," "), function(x){tail(x, n=1)})
trigramDF$bigram <- lapply(strsplit(trigramDF$trigram," "), 
                           function(x){paste(head(x, n =2), collapse = " ")})

fourgramDF <- as.data.frame(row_sums(fourgram, na.rm = T))
fourgramDF <- add_rownames(fourgramDF, "fourgram")
fourgramDF <- rename(fourgramDF, freq = `row_sums(fourgram, na.rm = T)`)
fourgramDF$name <- lapply(strsplit(fourgramDF$fourgram," "), function(x){tail(x, n=1)})
fourgramDF$trigram <- lapply(strsplit(fourgramDF$fourgram," "), 
                             function(x){paste(head(x, n =3), collapse = " ")})

fivegramDF <- as.data.frame(row_sums(fivegram, na.rm = T))
fivegramDF <- add_rownames(fivegramDF, "fivegram")
fivegramDF <- rename(fivegramDF, freq = `row_sums(fivegram, na.rm = T)`)
fivegramDF$name <- lapply(strsplit(fivegramDF$fivegram," "), function(x){tail(x, n=1)})
fivegramDF$fourgram <- lapply(strsplit(fivegramDF$fivegram," "), 
                              function(x){paste(head(x, n =4), collapse = " ")})

object.size(unigramDF)/1024/1024
object.size(bigramDF)/1024/1024
object.size(trigramDF)/1024/1024
object.size(fourgramDF)/1024/1024
object.size(fivegramDF)/1024/1024
unigramDF <- unigramDF[unigramDF$freq != 1,]
bigramDF <- bigramDF[bigramDF$freq != 1,]
trigramDF <- trigramDF[trigramDF$freq != 1,]
fourgramDF <- fourgramDF[fourgramDF$freq != 1,]
fivegramDF <- fivegramDF[fivegramDF$freq != 1,]

save(list=c("unigramDF", "bigramDF", "trigramDF", "fourgramDF", "fivegramDF"), file="ngrams.RData")

