
#' Title: Call Of Duty League Case
#' Purpose: Cleaning and data frequency
#' Author: Mauriac
#' Date: Jan 25 2021
# Libs

library(tm)
library(qdap)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(ggalt)

# Set the working directory

setwd("~/Tex_Analitics-Class/hult_NLP_student/cases/Call of Duty E-Sport/playerTimeline")

# Bring in our supporting functions
source('~/Tex_Analitics-Class/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Create custom stop words
stops <- c(stopwords('SMART'), 'amp','en','http://t',"ðÿ”¥", "bro", 'brotha')

#read
immortals_team <- read.fst("student_2020-12-28_Immortals_player_timelines.fst")
Subliners_team <- read.fst("student_2020-12-28_Subliners_player_timelines.fst")

#count
immortals_team %>% 
  count()

Subliners_team %>% 
  count()


# Clean and Organize
immortals_txtTDM <- cleanMatrix('student_2020-12-28_Immortals_player_timelines.fst',
                                'text',
                                collapse        = T, 
                                customStopwords = stops, 
                                type            = 'TDM', 
                                wgt             = 'weightTf')

subliners_txtTDM <- cleanMatrix('student_2020-12-28_Subliners_player_timelines.fst',
                                'text',
                                collapse        = T, 
                                customStopwords = stops, 
                                type            = 'TDM', 
                                wgt             = 'weightTf')


df <- merge(immortals_txtTDM, subliners_txtTDM, by ='row.names')
names(df) <- c('terms', 'immortals', 'subliners')

# Examine
df[6:10,]

# Calculate the absolute differences among in common terms
df$diff <- abs(df$immortals - df$subliners)

# Organize df for plotting
df<- df[order(df$diff, decreasing=TRUE), ]
top19 <- df[1:19, ]

# Pyarmid Plot
pyramid.plot(lx         = top19$immortals, #left
             rx         = top19$subliners,    #right
             labels     = top19$terms,  #terms
             top.labels = c('immortals', 'Terms', 'subliners'), #corpora
             gap        = 120, # space for terms to be read
             main       = 'Words in Common', # title
             unit       = 'wordFreq') 
dev.off
