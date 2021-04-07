
#' Title: Cakk Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 22 2021
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
mutineer_team <- read.fst("student_2020-12-28_mutineers2_followers_timelines.fst")
minessota_team <- read.fst("student_2020-12-28_rokkr2_followers_timelines.fst")
Subliners_team <- read.fst("student_2020-12-28_Subliners2_followers_timelines.fst")

mutineer_team %>% 
  count()
minessota_team %>% 
  count()
Subliners_team %>% 
  count()

mdx <- 1:nrow(mutineer_team)
set.seed(123)
mdx  <- sample(mdx, 7000)
mutineer_text = mutineer_team[mdx,]

Rdx <- 1:nrow(minessota_team)
set.seed(123)
Rdx  <- sample(Rdx, 7000)
minessota_text = minessota_team[Rdx,]

sdx <- 1:nrow(Subliners_team)
set.seed(123)
sdx  <- sample(sdx, 7000)
subliners_text = Subliners_team[sdx,]


#saving the files to harddrive
write.fst(mutineer_text,'mutineer_teams_text.fst')
write.fst(minessota_text,'minessota_team_text.fst')
write.fst(subliners_text,'subliners_team_text.fst')


mutineer_teams <- read.fst("mutineer_teams_text.fst")
minessota_teams <- read.fst("minessota_team_text.fst")
subliners_teams <- read.fst("subliners_team_text.fst")

# Deleting non- AcSII in text
gsub("[^\x01-\x7F]", "", mutineer_teams$text)
gsub("[^\x01-\x7F]", "", minessota_teams$text)
gsub("[^\x01-\x7F]", "", subliners_teams$text)



# Create custom stop words
stops <- c(stopwords('SMART'),'en','http://t',"ðÿ”¥")

# Clean and Organize
mutineer_txtDTM <- cleanMatrix('mutineer_teams_text.fst',
                                'text',
                                collapse        = F, 
                                customStopwords = stops, 
                                type            = 'DTM', 
                                wgt             = 'weightTf')
minessota_txtDTM <- cleanMatrix('minessota_team_text.fst',
                                    'text',
                                    collapse        = F, 
                                    customStopwords = stops, 
                                    type            = 'DTM', 
                                    wgt             = 'weightTf')
subliners_txtDTM <- cleanMatrix('subliners_team_text.fst',
                                'text',
                                collapse        = F, 
                                customStopwords = stops, 
                                type            = 'DTM', 
                                wgt             = 'weightTf')

#Examine Tidy & Compare
mutineer_tmp      <- as.DocumentTermMatrix(mutineer_txtDTM, weighting = weightTf ) 
minessota_tmp      <- as.DocumentTermMatrix(minessota_txtDTM, weighting = weightTf ) 
subliners_tmp      <- as.DocumentTermMatrix(subliners_txtDTM, weighting = weightTf )

mutineer_tidyCorp <- tidy(mutineer_tmp)
minessota_tidyCorp <- tidy(minessota_tmp)
subliners_tidyCorp <- tidy(subliners_tmp)

mutineer_tidyCorp
minessota_tidyCorp
subliners_tidyCorp

dim(mutineer_tidyCorp)
dim(minessota_tidyCorp)
dim(subliners_tidyCorp)

# Get bing lexicon
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent_mutineer <- inner_join(mutineer_tidyCorp, bing, by=c('term' = 'word'))
bingSent_minessota <- inner_join(minessota_tidyCorp, bing, by=c('term' = 'word'))
bingSent_subliners <- inner_join(subliners_tidyCorp, bing, by=c('term' = 'word'))

bingSent_mutineer
bingSent_minessota
bingSent_subliners

# Quick Analysis
table(bingSent_mutineer$sentiment, bingSent_mutineer$count)
table(bingSent_minessota$sentiment, bingSent_minessota$count)
table(bingSent_subliners$sentiment, bingSent_subliners$count)

aggregate(count~sentiment,bingSent_mutineer, sum)
aggregate(count~sentiment,bingSent_minessota, sum)
aggregate(count~sentiment,bingSent_subliners, sum)

# Compare original with qdap::Polarity
polarity(read.fst('mutineer_teams_text.fst')$text)
polarity(read.fst('minessota_team_text.fst')$text)
polarity(read.fst('subliners_team_text.fst')$text)


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
mutineer_nrcSent <- inner_join(mutineer_tidyCorp,nrcLex, by=c('term' = 'term'))
minessota_nrcSent <- inner_join(minessota_tidyCorp,nrcLex, by=c('term' = 'term'))
subliners_nrcSent <- inner_join(subliners_tidyCorp,nrcLex, by=c('term' = 'term'))

mutineer_nrcSent
minessota_nrcSent
subliners_nrcSent

# Quick Analysis
table(mutineer_nrcSent$sentiment)
mutineer_emos <- data.frame(table(mutineer_nrcSent$sentiment))

table(minessota_nrcSent$sentiment)
minessota_emos <- data.frame(table(minessota_nrcSent$sentiment))

table(subliners_nrcSent$sentiment)
subliners_emos <- data.frame(table(subliners_nrcSent$sentiment))
#emos <- emos[-c(6,7),] #drop columns if needed
chartJSRadar(scores = mutineer_emos, labelSize = 10, showLegend = F)
chartJSRadar(scores = minessota_emos, labelSize = 10, showLegend = F)
chartJSRadar(scores = subliners_emos, labelSize = 10, showLegend = F)
