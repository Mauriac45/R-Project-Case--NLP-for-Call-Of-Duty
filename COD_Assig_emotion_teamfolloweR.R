
#' Title: Call Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 25 2021
# Libs

library(tm)
library(lexicon)
library(tidytext)
library(dplyr)
library(qdap)
library(radarchart)

# Set the working directory

setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/teamFollowerTimelines")

# Bring in our supporting functions
source('~/Tex_Analitics-Class/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

#read
LAthieves_team <- read.fst("student_2020-12-28_LAThieves2_followers_timelines.fst")
optic_chicago_team <- read.fst("student_2020-12-28_OpTicCHI2_followers_timelines.fst")
immortals_team <- read.fst("student_2020-12-29_Immortals2_followers_timelines.fst")

LAthieves_team %>% 
  count()
optic_chicago_team %>% 
  count()
immortals_team %>% 
  count()

Ldx <- 1:nrow(LAthieves_team)
set.seed(123)
Ldx  <- sample(Ldx, 6000)
LAthieves_text = LAthieves_team[Ldx,]

odx <- 1:nrow(optic_chicago_team)
set.seed(123)
odx  <- sample(odx, 6000)
optic_chicago_text = optic_chicago_team[odx,]

idx <- 1:nrow(immortals_team)
set.seed(123)
idx  <- sample(idx, 5000)
immortals_text = immortals_team[idx,]


#saving the files to harddrive
write.fst(LAthieves_text,'LAthieves_teams_text.fst')
write.fst(optic_chicago_text,'optic_chicago_text.fst')
write.fst(immortals_text,'immortals_teams_text.fst')


LAthieves_teams <- read.fst("LAthieves_teams_text.fst")
optic_chicago_teams <- read.fst("optic_chicago_text.fst")
immortals_teams <- read.fst("immortals_teams_text.fst")




# Create custom stop words
stops <- c(stopwords('SMART'),'en','http://t',"ðÿ”¥","amp","bro")

# Clean and Organize
LAthieves_txtDTM <- cleanMatrix('LAthieves_teams_text.fst',
                               'text',
                               collapse        = F, 
                               customStopwords = stops, 
                               type            = 'DTM', 
                               wgt             = 'weightTf')
optic_chicago_txtDTM <- cleanMatrix('optic_chicago_text.fst',
                                'text',
                                collapse        = F, 
                                customStopwords = stops, 
                                type            = 'DTM', 
                                wgt             = 'weightTf')
immortals_txtDTM <- cleanMatrix('immortals_teams_text.fst',
                                    'text',
                                    collapse        = F, 
                                    customStopwords = stops, 
                                    type            = 'DTM', 
                                    wgt             = 'weightTf')

#Examine Tidy & Compare
LAthieves_tmp      <- as.DocumentTermMatrix(LAthieves_txtDTM, weighting = weightTf ) 
optic_chicago_tmp      <- as.DocumentTermMatrix(optic_chicago_txtDTM, weighting = weightTf ) 
immortals_tmp      <- as.DocumentTermMatrix(immortals_txtDTM, weighting = weightTf )

LAthieves_tidyCorp <- tidy(LAthieves_tmp)
optic_chicago_tidyCorp <- tidy(optic_chicago_tmp)
immortals_tidyCorp <- tidy(immortals_tmp)

LAthieves_tidyCorp
optic_chicago_tidyCorp
immortals_tidyCorp

dim(LAthieves_tidyCorp)
dim(optic_chicago_tidyCorp)
dim(immortals_tidyCorp)

# Get bing lexicon
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent_LAthieves <- inner_join(LAthieves_tidyCorp, bing, by=c('term' = 'word'))
bingSent_optic <- inner_join(optic_chicago_tidyCorp, bing, by=c('term' = 'word'))
bingSent_immortals <- inner_join(immortals_tidyCorp, bing, by=c('term' = 'word'))

bingSent_LAthieves
bingSent_optic
bingSent_immortals

# Quick Analysis
table(bingSent_LAthieves$sentiment, bingSent_LAthieves$count)
table(bingSent_optic$sentiment, bingSent_optic$count)
table(bingSent_immortals$sentiment, bingSent_immortals$count)

aggregate(count~sentiment,bingSent_LAthieves, sum)
aggregate(count~sentiment,bingSent_optic, sum)
aggregate(count~sentiment,bingSent_immortals, sum)

# Compare original with qdap::Polarity
polarity(read.fst('LAthieves_teams_text.fst')$text)
polarity(read.fst('optic_chicago_text.fst')$text)
polarity(read.fst('immortals_teams_text.fst')$text)

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
LAthieves_nrcSent <- inner_join(LAthieves_tidyCorp,nrcLex, by=c('term' = 'term'))
optic_chicago_nrcSent <- inner_join(optic_chicago_tidyCorp,nrcLex, by=c('term' = 'term'))
immortals_nrcSent <- inner_join(immortals_tidyCorp,nrcLex, by=c('term' = 'term'))

LAthieves_nrcSent
optic_chicago_nrcSent
immortals_nrcSent

# Quick Analysis
table(LAthieves_nrcSent$sentiment)
LAthieves_teams_emos <- data.frame(table(LAthieves_nrcSent$sentiment))

table(optic_chicago_nrcSent$sentiment)
optic_chicagoteams_emos <- data.frame(table(optic_chicago_nrcSent$sentiment))

table(immortals_nrcSent$sentiment)
immortals_teams_emos <- data.frame(table(immortals_nrcSent$sentiment))

#emos <- emos[-c(6,7),] #drop columns if needed
chartJSRadar(scores = LAthieves_teams_emos, labelSize = 10, showLegend = F)
chartJSRadar(scores = optic_chicagoteams_emos, labelSize = 10, showLegend = F)
chartJSRadar(scores = immortals_teams_emos, labelSize = 10, showLegend = F)
