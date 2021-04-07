
#' Title: Call Of Duty League Case
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

setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/playerTimeline")

# Bring in our supporting functions
source('~/Tex_Analitics-Class/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

#read
LAthieves_players <- read.fst("student_2020-12-28_ATLFaZe_player_timelines.fst")
optic_chicago_players <- read.fst("student_2020-12-28_OpTicCHI_player_timelines.fst")
immortals_players <- read.fst("student_2020-12-28_Immortals_player_timelines.fst")

LAthieves_players %>% 
  count()
optic_chicago_players %>% 
  count()
immortals_players %>% 
  count()

Ldx <- 1:nrow(LAthieves_players)
set.seed(123)
Ldx  <- sample(Ldx, 7000)
LAthieves_text = LAthieves_players[Ldx,]

odx <- 1:nrow(optic_chicago_players)
set.seed(123)
odx  <- sample(odx, 7000)
optic_chicago_text = optic_chicago_players[Ldx,]

idx <- 1:nrow(immortals_players)
set.seed(123)
idx  <- sample(idx, 4397)
immortals_text = immortals_players[idx,]


#saving the files to harddrive
write.fst(LAthieves_text,'LAthieves_players_text.fst')
write.fst(optic_chicago_text,'optic_players_text.fst')
write.fst(immortals_text,'immortals_players_text.fst')


LAthieves_play <- read.fst("LAthieves_players_text.fst")
optic_chicago_play <- read.fst("optic_players_text.fst")
immortals_play <- read.fst("immortals_players_text.fst")

# Deleting non- AcSII in text
LAthieves_play$text <- gsub("[^\x01-\x7F]", "", LAthieves_play$text)
optic_chicago_play$text <- gsub("[^\x01-\x7F]", "", optic_chicago_play$text)
immortals_play$text <- gsub("[^\x01-\x7F]", "", immortals_play$text)



# Create custom stop words
stops <- c(stopwords('SMART'),'en','http://t',"ðÿ”¥")


  
# Vector Corpus; omit the meta data
LAthieves_corpus <- VCorpus(VectorSource(LAthieves_play$text))
optic_chicago_corpus <- VCorpus(VectorSource(optic_chicago_play$text))
immortals_corpus <- VCorpus(VectorSource(immortals_play$text))

  
# Clean up the data
LAthieves_corpus <- cleanCorpus(LAthieves_corpus, stops)
optic_chicago_corpus <- cleanCorpus(optic_chicago_corpus, stops)                      
immortals_corpus <-  cleanCorpus(immortals_corpus, stops) 

# Another way to extract the cleaned text 
#LAthieves_text  <- unlist(pblapply(LAthieves_text, content))
#optic_chicago_text   <- unlist(pblapply(optic_chicago_text, content)) 
#immortals_text  <- unlist(pblapply(immortals_text, content))

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
#LAthieves_text <- paste(LAthieves_text, collapse = ' ')
#optic_chicago_text <- paste(optic_chicago_text, collapse = ' ')
#immortals_text <- paste(immortals_text, collapse = ' ')

# Make a Term Document Matrix
LAthieves_txtTDM  <- TermDocumentMatrix(LAthieves_corpus)
LAthieves_txtTDMm <- as.matrix(LAthieves_txtTDM)
optic_chicago_txtTDM  <- TermDocumentMatrix(optic_chicago_corpus)
optic_chicago_txtTDMm <- as.matrix(optic_chicago_txtTDM)
immortals_text_txtTDM  <- TermDocumentMatrix(immortals_corpus)
immortals_text_txtTDMm <- as.matrix(immortals_text_txtTDM)

# Frequency Data Frame
LAthieves_txtTDMmv <- rowSums(LAthieves_txtTDMm)
topterm_LAthieves_DF <- data.frame(word=names(LAthieves_txtTDMmv),frequency=LAthieves_txtTDMmv)
optic_txtTDMmv <- rowSums(optic_chicago_txtTDMm)
topterm_optic_DF <- data.frame(word=names(optic_txtTDMmv),frequency=optic_txtTDMmv)
immortals_txtTDMmv <- rowSums(immortals_text_txtTDMm)
topterm_immortals_DF <- data.frame(word=names(immortals_txtTDMmv),frequency=immortals_txtTDMmv)

# Remove the row attributes meta 
rownames(topterm_LAthieves_DF) <- NULL
rownames(topterm_optic_DF) <- NULL
rownames(topterm_immortals_DF) <- NULL

# Simple barplot
topWords_LAthieves  <- subset(topterm_LAthieves_DF, topterm_LAthieves_DF$frequency >= 118) 
topWords_LAthieves  <- topWords_LAthieves[order(topWords_LAthieves$frequency, decreasing=F),]
topWords_optic  <- subset(topterm_optic_DF, topterm_optic_DF$frequency >= 90) 
topWords_optic  <- topWords_optic[order(topWords_optic$frequency, decreasing=F),]
topWords_immortals  <- subset(topterm_immortals_DF, topterm_immortals_DF$frequency >= 75) 
topWords_immortals  <- topWords_immortals[order(topWords_immortals$frequency, decreasing=F),]

#remove row attributes
rownames(topWords_LAthieves) <- NULL
rownames(topWords_optic) <- NULL
rownames(topWords_immortals) <- NULL

topWords_LAthieves$word <- factor(topWords_LAthieves$word, 
                                 levels=unique(as.character(topWords_LAthieves$word)))
topWords_optic$word <- factor(topWords_optic$word, 
                                  levels=unique(as.character(topWords_optic$word)))
topWords_immortals$word <- factor(topWords_immortals$word, 
                                  levels=unique(as.character(topWords_immortals$word)))

#top teams bar-chart

#LA Thieves
ggplot(topWords_LAthieves, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words LA Thieves ")

ggplot(topWords_optic, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words Optic Chicago ")

ggplot(topWords_immortals, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words immortals")
