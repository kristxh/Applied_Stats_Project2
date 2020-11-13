library(MASS)
library(ggplot2)
library(dplyr)
library(e1071)
library(caret)
library(psych) 
library(Amelia)#missmap for graphing missing values
library(broom) #tidy
library(DescTools)
library(car) #outliertest
library(readxl)
library(knitr)#nicer tables for ppt
library(rminer)#variable importance for NB
library(rms)
library(GGally)
library(tidyverse)


set.seed(1234)

#set your working directory here
setwd('C:/Users/Ronny Sherga/Google Drive/SMU_Masters/Term_2/Stats 2/Project 2/Data/')

#import csv using semicolon as separator, and stringsasfactors=true so our categ vars will have levels.
bankaddlfull<-read.csv('bank-additional-full.csv', header = T, sep = ";", stringsAsFactors = T)

#export the df to a standard excel table format so we can quickly and easily investigate more about it.
write.csv(bankaddlfull, "rewrite.csv", row.names = F)

#verify vars that should be factors
tibble(bankaddlfull)

#rename y to success
bankaddlfull <- rename(bankaddlfull, success = y)

#attach dataframe to easily call variables
attach(bankaddlfull)

#check correlation between pdays and y
chisq.test(pdays,success)
ContCoef(pdays, success, correct = T)

#what are all the levels of the categorical variables?
sapply((Filter(is.factor,bankaddlfull)), levels)

#'unknown' is a value for some factors, which to me is the same as NA. should we remove these? Below is all rows that have "unknown" in 3 or more columns. 
#This value can be changed as we like.
bankaddlfull[rowSums(bankaddlfull == "unknown") >= 3,]

#any rows actually missing data (show up as NA instead of "unknown")?
missmap(bankaddlfull)
bankaddlfull[which(is.na(bankaddlfull)),]

#convert all "unknown" and "nonexistent" to NA in dataframe.
bankaddlfull$job[which(job == "unknown")] <- NA
bankaddlfull$marital[which(marital == "unknown")] <- NA
bankaddlfull$education[which(education == "unknown")] <- NA
bankaddlfull$default[which(default == "unknown")] <- NA
bankaddlfull$housing[which(housing == "unknown")] <- NA
bankaddlfull$loan[which(loan == "unknown")] <- NA
bankaddlfull$poutcome[which(poutcome == "nonexistent")] <- NA

#drop unused levels
bankaddlfull[] <- lapply(bankaddlfull, function(x) if (is.factor(x)) factor(x) else x)
sapply((Filter(is.factor,bankaddlfull)), levels)

#check for uneven response variable
table(bankaddlfull$y)
#how many pdays are 999?
nrow(bankaddlfull[which(bankaddlfull$pdays == 999),])




#downsample to randomly give sample with same number of Yes/No
dfNo <- bankaddlfull[which(bankaddlfull$y == "no"),]
dfYes <- bankaddlfull[which(bankaddlfull$y == "yes"),]
downsamp <-sample(seq(1:length(dfNo$y)), length(dfYes$y))
downsamp <- dfNo[downsamp,]
downsamp <- rbind(downsamp,dfYes)

table(downsamp$y)
attach(bankaddlfull)
downdf <- downSample(bankaddlfull, y)

table(downdf$Class)
