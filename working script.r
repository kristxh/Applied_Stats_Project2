library(MASS)
library(ggplot2)
library(ggthemes)
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
library(gridExtra)
library(ISLR)
library(ResourceSelection)
library(glmnet)
library(bestglm)
library(car)
library(naniar)

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

#n/a plot
gg_miss_var(bankaddlfull)

#drop unused levels
bankaddlfull[] <- lapply(bankaddlfull, function(x) if (is.factor(x)) factor(x) else x)
sapply((Filter(is.factor,bankaddlfull)), levels)

#check for uneven response variable
table(bankaddlfull$success)

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
downdf <- downSample(bankaddlfull, success)

table(downdf$Class)

#aggregate for summary stats by groups for continous predictors
aggregate(education~success,data=downdf,summary) 
aggregate(job~success,data=downdf,summary) 
aggregate(marital~success,data=downdf,summary) 
aggregate(default~success,data=downdf,summary) 
aggregate(housing~success,data=downdf,summary) 
aggregate(loan~success,data=downdf,summary) 
aggregate(contact~success,data=downdf,summary) 
aggregate(month~success,data=downdf,summary) 
aggregate(day_of_week~success,data=downdf,summary) 
aggregate(poutcome~success,data=downdf,summary) 

#table of counts for categorcal predictors
ftable(addmargins(table(education,success))) 
ftable(addmargins(table(job,success))) 
ftable(addmargins(table(marital,success))) 
ftable(addmargins(table(default,success))) 
ftable(addmargins(table(housing,success))) 
ftable(addmargins(table(loan,success))) 
ftable(addmargins(table(contact,success))) 
ftable(addmargins(table(month,success))) 
ftable(addmargins(table(day_of_week,success))) 
ftable(addmargins(table(poutcome,success)))

#proportions
prop.table(table(success,education),2)
prop.table(table(success,job),2)
prop.table(table(success,marital),2)
prop.table(table(success,default),2)
prop.table(table(success,housing),2)
prop.table(table(success,loan),2)
prop.table(table(success,contact),2)
prop.table(table(success,month),2)
prop.table(table(success,day_of_week),2)
prop.table(table(success,poutcome),2)

#visualize proportions for categorical variables
line_colors <- c("orange", "dodgerblue4")
plot(success~education,col=line_colors) 
plot(success~job,col=line_colors) # Retired and student
plot(success~marital,col=line_colors) # Single and unknown slightly higher
plot(success~default,col=line_colors) # No
plot(success~housing,col=line_colors)
plot(success~loan,col=line_colors)
plot(success~contact,col=line_colors) # cellular
plot(success~month,col=line_colors) # Dec, Sep, Oct, Mar
plot(success~day_of_week,col=line_colors)
plot(success~poutcome,col=line_colors) # Success

#boxplots for continuous variables
plot(age~success,col=line_colors)
plot(duration~success,col=line_colors)
plot(campaign~success,col=line_colors)
plot(pdays~success,col=line_colors) # Need to do something about the 999
plot(previous~success,col=line_colors)
plot(emp.var.rate~success,col=line_colors)
plot(cons.price.idx~success,col=line_colors)
plot(cons.conf.idx~success,col=line_colors)
plot(euribor3m~success,col=line_colors)
plot(nr.employed~success,col=line_colors)

#pca analysis on continuous variables
cont_cols <- c('age','duration','campaign','pdays','previous','emp.var.rate','cons.price.idx','cons.conf.idx','euribor3m','nr.employed','success')
cont_bankadd <- bankaddlfull[cont_cols]
pc.result<-prcomp(cont_bankadd[,1:10],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$success<-cont_bankadd$success

#plot the first few pc's
ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=success), size=1) +
  ggtitle("PCA of Bank Data") +
  scale_color_manual(values=line_colors)

#model
bankclean<-na.omit(bankaddlfull)
attach(bankclean)
model.main<-glm(success ~ ., data=bankclean,family = binomial(link="logit"))

#vif
(vif(model.main)[,3])^2 # not working

#hoslem lack of fit test
hoslem.test(model.main$y, fitted(model.main), g=10)

#summary of current fit
summary(model.main)

#generate ci using the summary coefficients
exp(cbind("Odds ratio" = coef(model.main), confint.default(model.main, level = 0.95)))

#manually fit some models
#note: if one were to conduct forward selection (see below (NO cv/ test set, just AIC selected)), R would want to keep all of the 
#highly correlated predictors in questions and the same interpretation problem occurs
model.null<-glm(success ~ 1, data=bankclean,family = binomial(link="logit"))

#start with null model and build using forward selection up to all the predictors that were specified in the main model previously
step(model.null,
     scope = list(upper=model.main),
     direction="forward",
     test="Chisq",
     data=modelclean)

#residual diagnostics - 4th plot for leverage and cook's d
plot(model.main)

#with a simplistic model with no lack of fit issues, we can beging providing statistical inference if no interactions are present
summary(model.main)

#using the summary coefficients we can generate CIs
exp(cbind("Odds ratio" = coef(model.main), confint.default(model.main, level = 0.95)))

#add complexity and repeat steps above
#pick up at line 204 from example code
