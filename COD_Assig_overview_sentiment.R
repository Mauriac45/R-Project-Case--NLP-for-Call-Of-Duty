#' Title: Cakk Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 25 2021


# Set the working directory
setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/teamTimeline")

# Libs
library(tm)
library(lexicon)
library(tidytext)
library(dplyr)
library(qdap)
library(radarchart)

source('~/Tex_Analitics-Class/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

emoji <- read.csv("C:/Users/mauri/Documents/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/emojis.csv")

# Create custom stop words
stops <- c(stopwords('SMART'),'en','http://t',"ðÿ”¥")

#read
Overview <- read.fst("student_TeamTimelines.fst")

Overview %>% 
  count()

Overview_text <- 1:nrow(Overview)
set.seed(123)
Overview_text  <- sample(Overview_text, 10000)
overview_T = Overview[Overview_text,]


#saving the files to hard drive
write.fst(overview_T,'overview_tweets_teams.fst')

overview_teams <- read.fst("overview_tweets_teams.fst")



# sub emojis
overview_teams$text <- pbsapply(as.character(overview_teams$text), mgsub, emoji$emoji, emoji$name)

# Clean and Organize
overview_text_corpus <- VCorpus(VectorSource(overview_teams$text))
overview_text_corpus <- cleanCorpus(overview_text_corpus, stops)
content(overview_text_corpus[[1]])

# Extract the clean and subbed text to use polarity 
clean_overview <- data.frame(document = seq_along(overview_teams$user_id), #simple id order
                         postID = overview_teams$user_id, # keep track of posts
                         text = unlist(sapply(overview_text_corpus, `[`, "content")),stringsAsFactors=F)

# Polarity & append each forum post score
pol <- polarity(clean_overview$text)
pol$group

# Append to the clean data
clean_overview$polarityValue <- pol$all$polarity

# Some documents returns NA from polarity, could be only stop words, screenshots etc, chg to 0 
clean_overview$polarityValue[is.na(clean_overview$polarityValue)] <- 0

# Classify the polarity scores
clean_overview$polarityClass <- ifelse(clean_overview$polarityValue>0, 'positive',
                                   ifelse(clean_overview$polarityValue<0, 'negative', 'neutral'))

# Now let's assign an emotion to each post; no need to "re-clean" the text
overview_DTM   <- DocumentTermMatrix(VCorpus(VectorSource(clean_overview$text)))
overview_tidyCorp <- tidy(overview_DTM)
overview_tidyCorp
dim(overview_tidyCorp)

# get nrc
nrc     <- get_sentiments(lexicon = c("nrc"))
nrcSent <- inner_join(overview_tidyCorp,nrc, by=c('term' = 'word'))
nrcSent

# Now group by document and select the most numerous 
grpSent <- nrcSent %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpSent$document <- as.numeric(as.character(grpSent$document))
grpSent

# Cast to wide format
wideSent <- dcast(grpSent, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideSent) #rowsum of 1 should be 6 based on grpSent
wideSent[grep('\\b100\\b',wideSent$document),] #"negative" should be 5 based on grpSent

# Drop positive/negative & get maximum column, but need to use  if else in case some docs were only pos/neg
wideSent <- wideSent[,-c(7,8)]
wideSent$maxEmotion <- ifelse(rowSums(wideSent[,2:ncol(wideSent)])>0,
                              names(wideSent)[2:ncol(wideSent)][max.col(wideSent[,2:ncol(wideSent)])],
                              'noEmotion')
head(wideSent)

#  left_join
clean_overview <- left_join(clean_overview, wideSent, by = c('document'='document'))
clean_overview$maxEmotion[is.na(clean_overview$maxEmotion)] <- 'noEmotion' #NA introduced from join on docs that had no emotion

# Finally, a clean text, with ID, moderators, polarity, and emotional sentiment
head(clean_overview)

# Now let's make a comparison cloud using a loop
polarityLst <- list()
for(i in 1:length(unique(clean_overview$polarityClass))){
  x <- subset(clean_overview$text, clean_overview$polarityClass == unique(clean_overview$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst[[unique(clean_overview$polarityClass)[i]]] <- x
}

# Using the list
allPolarityClasses <- do.call(rbind, polarityLst)
allPolarityClasses <- VCorpus(VectorSource(allPolarityClasses))
allPolarityClasses <- TermDocumentMatrix(cleanCorpus(allPolarityClasses, stops))
allPolarityClasses <- as.matrix(allPolarityClasses)

# Add the names from the list, get the order right!
colnames(allPolarityClasses) <- names(polarityLst)

# Make comparison cloud
comparison.cloud(allPolarityClasses, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses),"Dark2"),
                 scale=c(3,0.1))
dev.off()

# Repeat for the max emotion
emotionLst <- list()
for(i in 1:length(unique(clean_overview$maxEmotion))){
  x <- subset(clean_overview$text, clean_overview$maxEmotion == unique(clean_overview$maxEmotion)[i])
  x <- paste(x, collapse = ' ')
  emotionLst[[unique(clean_overview$maxEmotion)[i]]] <- x
}

# Using the list
allEmotionClasses <- do.call(rbind, emotionLst)
allEmotionClasses <- VCorpus(VectorSource(allEmotionClasses))
allEmotionClasses <- TermDocumentMatrix(allEmotionClasses)
allEmotionClasses <- as.matrix(allEmotionClasses)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(allEmotionClasses) <- names(emotionLst)


comparison.cloud(allEmotionClasses, 
                 max.words=300, 
                 random.order=FALSE,
                 title.size=1,
                 colors=viridis(10),
                 scale=c(3,0.1))
