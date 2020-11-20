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
library(pscl)
library(ROCR)#prediction

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

#convert pdays to a factor so we can better address the 999. split by weeks.
bankaddlfull$pdays <- cut(bankaddlfull$pdays, breaks=c(0, 7, 14, 21, 28, Inf),labels=c("1_week", "2_weeks", "3_weeks", "4_weeks", "no_contact"))

#what are all the levels of the categorical variables?
sapply((Filter(is.factor,bankaddlfull)), levels)

#'unknown' is a value for some factors, which to me is the same as NA. should we remove these? Below is all rows that have "unknown" in 3 or more columns. 
#This value can be changed as we like.
bankaddlfull[rowSums(bankaddlfull == "unknown") >= 3,]

#any rows actually missing data (show up as NA instead of "unknown")?
bankaddlfull[which(is.na(bankaddlfull)),]

#convert all "unknown" and "nonexistent" to NA in dataframe.
bankaddlfull[bankaddlfull=="unknown"] <-NA
bankaddlfull[bankaddlfull=="nonexistent"] <- NA

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

index<-sample(1:nrow(downdf),.8*nrow(downdf),replace=FALSE)
test<-downdf[-index,]
train<-downdf[index,]

attach(train)



#visualize proportions for categorical variables
line_colors <- c("orange", "dodgerblue4")
plot(success~education,col=line_colors) #related
plot(success~job,col=line_colors) # related
plot(success~marital,col=line_colors) # Single is higher
plot(success~default,col=line_colors) #very weak maybe not worth including
plot(success~housing,col=line_colors) #very weak maybe not worth including
plot(success~loan,col=line_colors) #very weak
plot(success~contact,col=line_colors) # strong
plot(success~month,col=line_colors)# strong
###reorder day levels###
train$day_of_week <- factor(train$day_of_week, levels= c("mon", "tue", "wed", "thu", "fri"))
#plot ordered by day of week
plot(success~train[order(day_of_week), ]$day_of_week,col=line_colors)#not very strong
plot(success~poutcome,col=line_colors) # strong
plot(success~pdays,col=line_colors)#strong


#boxplots for continuous variables
line_colors <- c("dodgerblue4", "orange")
plot(age~success,col=line_colors) #weak
plot(duration~success,col=line_colors) #strong, but not useful for predictions. remove?
plot(campaign~success,col=line_colors) #hard to see much of a difference
hist(campaign)#major bulk of data is from about 15 days and less. try removing higher.
train<- train[campaign<16,]#removed rows
attach(train) #reattached df
plot(campaign~success,col=line_colors)#replot. easier to see. there is a slight relationship.
plot(emp.var.rate~success,col=line_colors) #maybe good predictor
plot(cons.price.idx~success,col=line_colors) #weak predictor
plot(cons.conf.idx~success,col=line_colors) #maybe good
plot(euribor3m~success,col=line_colors) #very weak maybe not worth including
plot(nr.employed~success,col=line_colors) #not bad

plot(previous~success,col=line_colors)#doesn't tell us very much. changing it to a factor is more reflective of a relationship. it is a discrete variable, so it works as a factor too.
#replot as factor#
train$previous<-factor(train$previous)
attach(train) #reattached df
plot(success~previous,col=line_colors)#much better


#aggregate for summary stats by groups for continuous predictors. 
t(aggregate(age~success,data=train,summary))#again shows YES with higher ages
t(aggregate(duration~success,data=train,summary))#as duration increases, there are more YES
t(aggregate(campaign~success,data=train,summary))#don't see much of a difference
t(aggregate(emp.var.rate~success,data=train,summary))#lower in general for YES, but hard to tell from this
t(aggregate(cons.price.idx~success,data=train,summary))#very weak.
t(aggregate(cons.conf.idx~success,data=train,summary))#hard to tell from the numbers.
t(aggregate(euribor3m~success,data=train,summary))#doesn't look useful
t(aggregate(nr.employed~success,data=train,summary))#not enough information


#table of counts for categorical predictors
ftable(addmargins(table(education,success))) #useful
ftable(addmargins(table(job,success))) #useful
ftable(addmargins(table(marital,success))) #cant tell
ftable(addmargins(table(default,success))) #not useful at all
ftable(addmargins(table(housing,success))) #not useful at all
ftable(addmargins(table(loan,success))) #not useful at all
ftable(addmargins(table(contact,success))) #useful
ftable(addmargins(table(month,success))) #looks useful
ftable(addmargins(table(day_of_week,success))) #looks pretty weak as a classifier. no change within or across. probably can remove.
ftable(addmargins(table(poutcome,success)))#poutcome success is a strong predictor of YES
ftable(addmargins(table(pdays,success)))#any previous contact dramatically increases the YES count
ftable(addmargins(table(previous,success)))#as the previous contacts increases, the YES classification is greater

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
prop.table(table(success,previous),2)




#pca analysis on continuous variables
cont_cols <- c('age','duration','campaign','emp.var.rate','cons.price.idx','cons.conf.idx','euribor3m','nr.employed','success')
cont_bankadd <- train[cont_cols]
pc.result<-prcomp(cont_bankadd[,1:8],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$success<-cont_bankadd$success

#plot the first few pc's
ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=success), size=1) +
  ggtitle("PCA of Bank Data") +
  scale_color_manual(values=line_colors)

#model
bankclean<-train
attach(bankclean)
model.main<-glm(success ~ ., data=bankclean,family = binomial(link = "logit"))

#vif
#the GVIF^(1/(2*Df)) can be squared to compare to standard VIF rules. 
(car::vif(model.main))^2

#hoslem goodness of fit test
hoslem.test(model.main$y, fitted(model.main), g=10) #very bad fit according to the p-value for goodness of fit test.


#there are some HUGE values here, so let's try again after removing some of the more questionable variables we noticed above.
bankclean<-subset(train, select = -c(default, housing, loan, day_of_week, age, duration, cons.price.idx, euribor3m))
attach(bankclean)
model.main<-glm(success ~ ., data=bankclean,family = binomial)
#summary of current fit
summary(model.main)

#vif

#the GVIF^(1/(2*Df)) can be squared to compare to standard VIF rules. 
#for poutcome and cons.conf.idx both are on the high end, but don't quite exceed 10 when squared.
car::vif(model.main)

#hoslem lack of fit test
hoslem.test(model.main$y, fitted(model.main), g=10) #looks like it'll be a good fit

##########################################################################################
###########################HUNG UP HERE FOR NOW#########################################
#################HAVING TROUBLE GETTING MY PREDICTION TO WORK###########################
# test$previous<-factor(test$previous)
# colnames(bankclean)
# colnames(testclean)
# testclean<-subset(test, select = colnames(bankclean))
# modeltest<-subset(testclean, select = -c(success))
# fit.pred.manual<-predict(model.main, newdata = na.omit(modeltest))
# results.manual<- prediction(fit.pred.manual, testclean$success, label.ordering=c("No", "Yes"))
# 
# bankclean$previous
# modeltest$previous
###########################################################################################
##########################################################################################
#########################################################################################

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

################################Bodie's code######################################



#Now model time

full.model <- glm(success~.,data=downdfculled,family=binomial)
coef(full.model)

#Step.model

step_model <- full.model %>% stepAIC(trace=FALSE)
coef(step_model)

summary(step_model)

hoslem.test(step_model$y, fitted(step_model), g=10)

####

back_model <- step(full.model,direction="backward",trace=FALSE)



back_final <-glm(success ~ education + housing + contact + month + day_of_week + 
                   campaign + pdays + poutcome + emp.var.rate + cons.price.idx + 
                   euribor3m,data=downdf,family=binomial)

summary(back_final)

hoslem.test(back_final$y, fitted(back_final), g=10)

####

forward_model <-step(full.model,direction="forward",trace=FALSE)
forward_model

forward_final <-glm(formula = success ~ age + job + marital + education + housing + 
                      loan + contact + month + day_of_week + campaign + pdays + 
                      previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
                      euribor3m + nr.employed, family = binomial, data = down_bank)

hoslem.test(forward_final$y, fitted(forward_final), g=10)

####

step_model <-step(full.model,direction="both",trace=FALSE)
step_model

step_final <-glm(formula = success ~ education + housing + contact + month + 
                   day_of_week + campaign + pdays + poutcome + emp.var.rate + 
                   cons.price.idx + euribor3m, family = binomial, data = down_bank)

hoslem.test(step_final$y, fitted(step_final), g=10)


### value of .4 or higher incidates good fit
list(stepmodel = pscl::pR2(step_final)["McFadden"],backmodel = pscl::pR2(back_final)["McFadden"],
     forwardmodel = pscl::pR2(forward_final)["McFadden"])


###Doing KNN as compare
###KNN

# Fit the model on the training set


KNN_model <- train(success~.,data = down_bank, method = "knn",
                   trControl = trainControl("cv", number = 10),
                   preProcess = c("center","scale"),
                   tuneLength = 10)

#This line of code will tell us the best value of K to use.
KNN_model
# Best tuning parameter k that minimize the RMSE
KNN_model$bestTune
KNN_model$results



=======
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

#add complexity and repeat steps above
#pick up at line 204 from example code
