#' Title: Cakk Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 22 2021


# Set the working directory
setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/teamTimeline")


# Libs
library(tm)
library(lexicon)
library(tidytext)
library(dplyr)
library(qdap)
library(radarchart)


# Bring in our supporting functions
source('~/Tex_Analitics-Class/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

#read
Overview <- read.fst("student_TeamTimelines.fst")

Overview %>% 
  count()

Overview_text <- 1:nrow(Overview)
set.seed(123)
Overview_text  <- sample(Overview_text, 10000)
overview_T = Overview[Overview_text,]

# Deleting non- AcSII in text
gsub("[^\x01-\x7F]", "", Overview$text)

#saving the files to harddrive
write.fst(overview_T,'overview_tweets_teams.fst')

overview_teams <- read.fst("overview_tweets_teams.fst")

# Create custom stop words
stops <- c(stopwords('SMART'),'en','http://t',"ðÿ”¥")

# Clean and Organize
overview_txtDTM <- cleanMatrix('overview_tweets_teams.fst',
                               'text',
                               collapse        = F, 
                               customStopwords = stops, 
                               type            = 'DTM', 
                               wgt             = 'weightTf')

#Examine Tidy & Compare
overview_tmp      <- as.DocumentTermMatrix(overview_txtDTM, weighting = weightTf ) 
overview_tidyCorp <- tidy(overview_tmp)
overview_tidyCorp
dim(overview_tidyCorp)

# Get bing lexicon
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(overview_tidyCorp, bing, by=c('term' = 'word'))
bingSent

# Quick Analysis
table(bingSent$sentiment, bingSent$count)
aggregate(count~sentiment,bingSent, sum)

# Compare original with qdap::Polarity
polarity(read.fst('overview_tweets_teams.fst')$text)

# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Get nrc lexicon; deprecated in tidytext, use library(lexicon)
nrc <- nrc_emotions
head(nrc)

# Clean this up
#nrc <- read.csv('nrcSentimentLexicon.csv')
#Use apply (rowwise) to find columns having value > 0
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))
head(sent)

# Reshape
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)
head(nrcLex)

# Perform Inner Join
nrcSent <- inner_join(overview_tidyCorp,nrcLex, by=c('term' = 'term'))
nrcSent

# Quick Analysis
table(nrcSent$sentiment)
overview_teams_emos <- data.frame(table(nrcSent$sentiment))
#emos <- emos[-c(6,7),] #drop columns if needed
chartJSRadar(scores = overview_teams_emos, labelSize = 10, showLegend = F)
