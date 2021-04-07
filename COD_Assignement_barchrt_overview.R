#' Title: Call Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 25 2021


# Set the working directory
setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/teamTimeline")


# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(pbapply)
library(plotrix)


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

# Vector Corpus; omit the meta data
overview_team_clean <- VCorpus(VectorSource(overview_teams$text))

# Clean up the data
overview_team_text <- cleanCorpus(overview_team_clean, stops)

# Make a Term Document Matrix
overview_txtTDM  <- TermDocumentMatrix(overview_team_text)
overview_txtTDMm <- as.matrix(overview_txtTDM)


# Frequency Data Frame
overview_txtTDMmv <- rowSums(overview_txtTDMm)
topterm_overview_DF <- data.frame(word=names(overview_txtTDMmv),frequency=overview_txtTDMmv)

# Remove the row attributes meta 
rownames(topterm_overview_DF) <- NULL

# Simple barplot
topWords_overview  <- subset(topterm_overview_DF, topterm_overview_DF$frequency >= 200) 
topWords_overview  <- topWords_overview[order(topWords_overview$frequency, decreasing=F),]

#remove row attributes
rownames(topWords_overview) <- NULL


topWords_overview$word <- factor(topWords_overview$word, 
                            levels=unique(as.character(topWords_overview$word)))

#top teams bar-chart

ggplot(topWords_overview, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words ")


# Reduce TDM
reducedTDM <- removeSparseTerms(overview_txtTDM, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')
