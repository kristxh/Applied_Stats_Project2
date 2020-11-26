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
#write.csv(bankaddlfull, "rewrite.csv", row.names = F)

#verify vars that should be factors
tibble(bankaddlfull)

#rename y to success
bankaddlfull <- rename(bankaddlfull, success = y)

#attach dataframe to easily call variables
attach(bankaddlfull)

#check correlation between pdays and y
chisq.test(pdays,success)
ContCoef(pdays, success, correct = T)

#convert pdays to a factor so we can better address the 999. split by weeks.
bankaddlfull$pdays <- cut(bankaddlfull$pdays, breaks=c(0, 7, 14, 21, 28, Inf),labels=c("1_week", "2_weeks", "3_weeks", "4_weeks", "no_contact"))



#what are all the levels of the categorical variables?
sapply((Filter(is.factor,bankaddlfull)), levels)

#'unknown' is a value for some factors, which to me is the same as NA. should we remove these? Below is all rows that have "unknown" in 3 or more columns. 
#This value can be changed as we like.
bankaddlfull[rowSums(bankaddlfull == "unknown") >= 3,]

#any rows actually missing data (show up as NA instead of "unknown")?
missmap(bankaddlfull)
bankaddlfull[which(is.na(bankaddlfull)),]

#convert all "unknown" and "nonexistent" to NA in dataframe.
bankaddlfull[bankaddlfull=="unknown"] <-NA
bankaddlfull$poutcome[which(poutcome == "nonexistent")] <- NA

#n/a plot
gg_miss_var(bankaddlfull)

#add full name months as levels, and change names accordingly
levels(bankaddlfull$month) <- c(levels(bankaddlfull$month), month.name)
bankaddlfull$month[which(bankaddlfull$month == "apr")] <- "April"
bankaddlfull$month[which(bankaddlfull$month == "aug")] <- "August"
bankaddlfull$month[which(bankaddlfull$month == "dec")] <- "December"
bankaddlfull$month[which(bankaddlfull$month == "jul")] <- "July"
bankaddlfull$month[which(bankaddlfull$month == "jun")] <- "June"
bankaddlfull$month[which(bankaddlfull$month == "mar")] <- "March"
bankaddlfull$month[which(bankaddlfull$month == "may")] <- "May"
bankaddlfull$month[which(bankaddlfull$month == "nov")] <- "November"
bankaddlfull$month[which(bankaddlfull$month == "oct")] <- "October"
bankaddlfull$month[which(bankaddlfull$month == "sep")] <- "September"

#drop unused levels
bankaddlfull[] <- lapply(bankaddlfull, function(x) if (is.factor(x)) factor(x) else x)
sapply((Filter(is.factor,bankaddlfull)), levels)

#check for uneven response variable
table(bankaddlfull$success)

#how many pdays are 999?
nrow(bankaddlfull[which(bankaddlfull$pdays == 999),])

#downsample to randomly give sample with same number of Yes/No
# dfNo <- bankaddlfull[which(bankaddlfull$y == "no"),]
# dfYes <- bankaddlfull[which(bankaddlfull$y == "yes"),]
# downsamp <-sample(seq(1:length(dfNo$y)), length(dfYes$y))
# downsamp <- dfNo[downsamp,]
# downsamp <- rbind(downsamp,dfYes)
# table(downsamp$y)


attach(bankaddlfull)
downdf <- downSample(bankaddlfull, success)
downdf <- subset(downdf, select = -c(Class))

attach(downdf)

#pair plots
ggpairs(downdf, columns = 1:10, aes(color=success))
ggpairs(downdf, columns = 11:20, aes(color=success))

#aggregate for summary stats by groups for continous predictors. 

####################################################these are all categorical though. come back to this########################################
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
aggregate(pdays~success,data=downdf,summary)


#table of counts for categorical predictors
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
ftable(addmargins(table(pdays,success)))

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
prop.table(table(success,pdays),2)



#visualize proportions for categorical variables
line_colors <- c("orange", "dodgerblue4")
plot(success~education,col=line_colors) 
plot(success~job,col=line_colors) # Retired and student
plot(success~marital,col=line_colors) # Single and unknown slightly higher
plot(success~default,col=line_colors) # No
plot(success~housing,col=line_colors)
plot(success~loan,col=line_colors)
plot(success~contact,col=line_colors) # cellular
plot(success~downdf$month,col=line_colors)# not sure why, but needed to specify dataframe even though it's attached
###reorder day levels###
downdf$day_of_week <- factor(downdf$day_of_week, levels= c("mon", "tue", "wed", "thu", "fri"))
plot(success~downdf[order(downdf$day_of_week), ]$day_of_week,col=line_colors)#plot ordered by day of week
plot(success~poutcome,col=line_colors) # Success
plot(success~pdays,col=line_colors)



#boxplots for continuous variables
plot(age~success,col=line_colors)
plot(duration~success,col=line_colors)
plot(campaign~success,col=line_colors)
plot(previous~success,col=line_colors)
plot(emp.var.rate~success,col=line_colors)
plot(cons.price.idx~success,col=line_colors)
plot(cons.conf.idx~success,col=line_colors)
plot(euribor3m~success,col=line_colors)
plot(nr.employed~success,col=line_colors)

#pca analysis on continuous variables
cont_cols <- c('age','duration','campaign','pdays','previous','emp.var.rate','cons.price.idx','cons.conf.idx','euribor3m','nr.employed','success')
cont_bankadd <- downdf[cont_cols]
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
bankclean<-na.omit(downdf)
attach(bankclean)
bankclean <- subset(downdf, select = -Class)
model.main<-glm(success ~ ., data=bankclean,family = binomial(link = "logit"))

#vif
(vif(model.main))^2

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

##############LDA########################
mylda<-lda(success~age+duration+campaign+previous+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed,data=train)
pred_l<-predict(mylda,newdata=test)$class  #Predictions can come in many forms, the class form provides the categorical level of your response.
truth_l<-test$success
x_l<-table(pred_l,truth_l) # Creating a confusion matrix
x_l
dim(test)
nrow(test)

#Missclassification Error
ME_lda<-(x_l[2,1]+x_l[1,2])/nrow(test)
ME_lda
#Calculating overall accuracy
1-ME_lda

#Sensitivity
SEN_lda <- x_l[2,2]/(x_l[1,2]+x_l[2,2])
SEN_lda

#specificity
SPEC_lda <- x_l[1,1]/(x_l[1,1]+x_l[2,1])
SPEC_lda

##############QDA########################

#QDA
myqda<-qda(success~age+duration+campaign+previous+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed,data=train)
pred_q<-predict(myqda,newdata=test)$class  #Predictions can come in many forms, the class form provides the categorical level of your response.
truth_q<-test$success
x_q<-table(pred_q,truth_q) # Creating a confusion matrix
x_q

#Missclassification Error
ME_qda<-(x_q[2,1]+x_q[1,2])/nrow(test)
ME_qda
#Calculating overall accuracy
1-ME_qda

#sensitivity
SEN_qda <- x_l[2,2]/(x_l[1,2]+x_l[2,2])
SEN_qda

#specificity
SPEC_qda <- x_l[1,1]/(x_l[1,1]+x_l[2,1])
SPEC_qda

############Random Forest#################

rf <- randomForest(
  success ~ .,
  data=train
)

pred_rf = predict(rf, newdata=test)
cm_rf = table(test[,20], pred_rf)

#Missclassification Error
ME_rf<-(cm_rf[2,1]+ cm_rf[1,2])/nrow(test)
ME_rf
#Calculating overall accuracy
1-ME_rf

#sensitivity
SEN_rf <- x_l[2,2]/(x_l[1,2]+x_l[2,2])
SEN_rf

#specificity
SPEC_rf <- x_l[1,1]/(x_l[1,1]+x_l[2,1])
SPEC_rf

# Tuning to find mtry value
tuneRF(downdf, success)

# Importance of variables
importance(rf)
varImpPlot(rf)

# Print
print(rf)

