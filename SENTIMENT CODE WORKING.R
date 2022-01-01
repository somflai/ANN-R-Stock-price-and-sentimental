
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)

score.sentiment <- function(sentences, pos.words, neg.words, progress='none')
{
  require(plyr)
  require(stringer)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]',"",sentence)
    sentence <- gsub('[[:cntrl:]]',"",sentence)
    sentence <- gsub('\\d+',"",sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence,'\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches)-sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress~.progress)
  scores.df <- data.frame(score=scores, text = sentences)
  return(scores.df)
}

setwd("C:/Users/mahe/Desktop/Sentimental Analysis/opinion-lexicon-English")
getwd()
pos.words=scan('C:/Users/mahe/Desktop/Sentimental Analysis/opinion-lexicon-English/positive-words.txt',what='character',comment.char=';')
#hu.liu.pos
neg.words=scan('C:/Users/mahe/Desktop/Sentimental Analysis/opinion-lexicon-English/negative-words.txt',what='character',comment.char=';')
#hu.liu.neg

nscore <- score.sentiment(News_Tata$Article1, pos.words, neg.words, .progress='none')






require(plyr)
require(stringer)
scores1 <- laply(News_Tata$Article1, function(sentence, pos.words, neg.words){
  sentence <- gsub('[[:punct:]]',"",sentence)
  sentence <- gsub('[[:cntrl:]]',"",sentence)
  sentence <- gsub('\\d+',"",sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence,'\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches)-sum(neg.matches)
  return(score)
}, pos.words, neg.words, .progress='none')
#scores.df <- data.frame(score=scores, text = sentences)
scores1



require(plyr)
require(stringer)
scores2 <- laply(News_Tata$Article2, function(sentence, pos.words, neg.words){
  sentence <- gsub('[[:punct:]]',"",sentence)
  sentence <- gsub('[[:cntrl:]]',"",sentence)
  sentence <- gsub('\\d+',"",sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence,'\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches)-sum(neg.matches)
  return(score)
}, pos.words, neg.words, .progress='none')
#scores.df <- data.frame(score=scores, text = sentences)
scores2



require(plyr)
require(stringer)
scores3 <- laply(News_Tata$Article2__1, function(sentence, pos.words, neg.words){
  sentence <- gsub('[[:punct:]]',"",sentence)
  sentence <- gsub('[[:cntrl:]]',"",sentence)
  sentence <- gsub('\\d+',"",sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence,'\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches)-sum(neg.matches)
  return(score)
}, pos.words, neg.words, .progress='none')
#scores.df <- data.frame(score=scores, text = sentences)
scores3



require(plyr)
require(stringer)
scores4 <- laply(News_Tata$Article4, function(sentence, pos.words, neg.words){
  sentence <- gsub('[[:punct:]]',"",sentence)
  sentence <- gsub('[[:cntrl:]]',"",sentence)
  sentence <- gsub('\\d+',"",sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence,'\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches)-sum(neg.matches)
  return(score)
}, pos.words, neg.words, .progress='none')
#scores.df <- data.frame(score=scores, text = sentences)
scores4



require(plyr)
require(stringer)
scores5 <- laply(News_Tata$Article5, function(sentence, pos.words, neg.words){
  sentence <- gsub('[[:punct:]]',"",sentence)
  sentence <- gsub('[[:cntrl:]]',"",sentence)
  sentence <- gsub('\\d+',"",sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence,'\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches)-sum(neg.matches)
  return(score)
}, pos.words, neg.words, .progress='none')
#scores.df <- data.frame(score=scores, text = sentences)
scores5

TotalScore <- scores1+scores2+scores3+scores4+scores5
TotalScore

NewsScoreTATA <- data.frame(Date=Tata_News_Stock$Date, Close=Tata_News_Stock$Close, NewsScore = TotalScore)
NewsScoreTATA

help(laply)
??laply
