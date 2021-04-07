
#' Title: Cakk Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 22 2021
# Libs

library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(pbapply)

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
Ldx  <- sample(Ldx, 10000)
LAthieves_text = LAthieves_team[Ldx,]

odx <- 1:nrow(optic_chicago_team)
set.seed(123)
odx  <- sample(odx, 10000)
optic_chicago_text = optic_chicago_team[odx,]

idx <- 1:nrow(immortals_team)
set.seed(123)
idx  <- sample(idx, 10000)
immortals_text = immortals_team[idx,]


#saving the files to harddrive
#write.fst(LAthieves_text,'LAthieves_teams_text.fst')
#write.fst(optic_chicago_text,'optic_chicago_text.fst')
#write.fst(immortals_text,'immortals_teams_text.fst')


#LAthieves_teams <- read.fst("LAthieves_teams_text.fst")
#optic_chicago_teams <- read.fst("optic_chicago_text.fst")
#immortals_teams <- read.fst("immortals_teams_text.fst")

# Deleting non- AcSII in text
gsub("[^\x01-\x7F]", "", LAthieves_text$text)
gsub("[^\x01-\x7F]", "", optic_chicago_text$text)
gsub("[^\x01-\x7F]", "", immortals_text$text)



# Create custom stop words
stops <- c(stopwords('SMART'),'en','http://t',"ðÿ”¥","cdl","amp","tomorrow", "callofduty")


# Vector Corpus; omit the meta data
LAthieves_text <- VCorpus(VectorSource(LAthieves_text$text))
optic_chicago_text <- VCorpus(VectorSource(optic_chicago_text$text))
immortals_text <- VCorpus(VectorSource(immortals_text$text))


# Clean up the data
LAthieves_text <- cleanCorpus(LAthieves_text, stops)
optic_chicago_text <- cleanCorpus(optic_chicago_text, stops)                      
immortals_text <-  cleanCorpus(immortals_text, stops) 

# Another way to extract the cleaned text 
LAthieves  <- unlist(pblapply(LAthieves_text, content))
optic_chicago   <- unlist(pblapply(optic_chicago_text, content)) 
immortals  <- unlist(pblapply(immortals_text, content))

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
LAthieves <- paste(LAthieves, collapse = ' ')
optic_chicago <- paste(optic_chicago, collapse = ' ')
immortals <- paste(immortals, collapse = ' ')

# Combine the subject documents into a corpus of *3* documents
allTeams <- c(LAthieves, optic_chicago,immortals)
allTeams <- VCorpus((VectorSource(allTeams)))

# Make TDM
CODTDM  <- TermDocumentMatrix(allTeams)
CODTDMm <- as.matrix(CODTDM)

# Make sure order is correct!
colnames(CODTDMm) <- c('LAthieves','optic_chicago','immortals')

# Examine
head(CODTDMm)

commonality.cloud(CODTDMm, 
                  max.words=75, 
                  random.order=FALSE,
                  colors='blue',
                  scale=c(1.5,0.25))


comparison.cloud(CODTDMm, 
                 max.words=70, 
                 random.order=FALSE,
                 title.size=0.6,
                 colors=brewer.pal(ncol(CODTDMm),"Dark2"),
                 scale=c(2,0.1))
