# Capstone Project

# Train of thought
# First I have to be able to read in a set of known emails and already know whether or not they are good or bad emails.
# Do the standard divide into training and test data sets and create a model with the training data set and use it on the test data set. See whether or not the model is sufficent.

# Based on experience, emails with urls in the subject or message portion should be marked as suspicious. 


# Found package that may be useful "edeR". Claims to extract from, to, cc, subject, and datetime. Not sure about message portion, need to look into that.
# https://cran.r-project.org/web/packages/edeR/edeR.pdf
# Issue found: seems to only work for 5 mails at a time. Maybe have it on a loop to get more???

# Found two data sets that can be helpful. One is a txt file that contains all the information we need. I am concerned on how we are going to split the data.Also concerend on how random this data set is.
# The second data set seems to be the first one, but with the body of the email extracted and assigned a binomial value to determnine whether or not it is fraud.

# Important varaibles to consider: From, Subject, Body, Whether or not there are hyperlinks, etc.

# Trying to read in the file. 


## Used python to make it into a csv file and trying that instead
## Current problems include Body text is still messy. Have to think of ways to clean it up a bit more.



fmail<- read.csv("fmail.csv")
fraud<- rep(1,3977)
fmail<-cbind(fmail,fraud)
fmail$Body<-as.character(fmail$Body)


gmail<- read.csv("gmail.csv")
fraud<-rep(0,length(gmail$From))
gmail<- cbind(gmail,fraud)
gmail$Body<-as.character(gmail$Body)

# Cleaning up the text in the body to make sense.
# Removing special characters

rmspchr<- function(x) gsub("[^a-zA-Z0-9]"," ", x)

fmail$Body<- sapply(fmail$Body, rmspchr)
gmail$Body<- sapply(gmail$Body, rmspchr)

#fgmail$lexden<- n_distinct(word)/n()

library(plyr)
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(tm)

library(glmnet)

library(SuperLearner)
library(randomForest)

library(tree)
library(randomForest)
library(gbm)
library(e1071)


fgmail<-rbind(fmail,gmail)



summary(fgmail)

## Let's start by looking at what kind of words tend to appear in our fraud and nonfraud datasets.

undesirable_words<- c("From", "Date", "From:", "Date:", "Subject:","message", "thyme", "javamail", "evans","enron","subject","2001","2000")

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

fraud_words_filtered <- fmail %>%
  unnest_tokens(word, Body) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

fraud_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Email Count") +
  ggtitle("Most Frequently Used Words in Fraudulent Emails") +
  coord_flip()


## Too many emails in enron dataset. Will look into the sample instead for frequent words.

sampleamt<-5000
gmailsamp2<-gmail[sample(nrow(gmail),sampleamt),]

nonfraud_words_filtered <- gmailsamp2 %>%
  unnest_tokens(word, Body) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

nonfraud_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Email Count") +
  ggtitle("Most Frequently Used Words in Nonfraudulent Emails") +
  coord_flip()

## TFIDF

## Following instruction from https://rstudio-pubs-static.s3.amazonaws.com/541380_d83c993cd44743ba8f1785ccf6cd8304.html

sampleamt<-3000

fmailsamp<-fmail[sample(nrow(fmail),sampleamt),]
gmailsamp<-gmail[sample(nrow(gmail),sampleamt),]
fgmailsamp<-rbind.fill(fmailsamp,gmailsamp)
fgmailsamp[is.na(fgmailsamp)]<-0

train<-fgmailsamp[sample(nrow(fgmailsamp),2*sampleamt),]

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}



popular_tfidf_words <- train %>%
  unnest_tokens(word, Body) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  count(fraud, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word,fraud, n)

head(popular_tfidf_words)

top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(fraud) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(fraud, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = fraud)) +
  geom_col(show.legend = NA) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Fraud Type") +
  theme_lyrics() +  
  facet_wrap(~fraud, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()

# Following instructions from https://kharshit.github.io/blog/2017/08/25/email-spam-filtering-text-analysis-in-r

# Reduce size of gmail dataset so it doesn't take forever

gmailred<-gmail[sample(nrow(gmail),nrow(fmail)),]

corpus= VCorpus(VectorSource(fmail$Body))
corpus2= VCorpus(VectorSource(gmailred$Body))

corpus = tm_map(corpus, content_transformer(tolower))
corpus2 = tm_map(corpus2, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus2 = tm_map(corpus2, PlainTextDocument)

# remove all punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)
corpus2 = tm_map(corpus2, removePunctuation)
# remove all English stopwords from the corpus
corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus2 = tm_map(corpus2, removeWords, stopwords("en"))
# stem the words in the corpus
corpus = tm_map(corpus, stemDocument)
corpus2 = tm_map(corpus2, stemDocument)

dtm =DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
dtm2 =DocumentTermMatrix(corpus2)
spdtm2 = removeSparseTerms(dtm2, 0.95)

fmailSparse = as.data.frame(as.matrix(spdtm))
colnames(fmailSparse) = make.names(colnames(fmailSparse))
gmailSparse = as.data.frame(as.matrix(spdtm2))
colnames(gmailSparse) = make.names(colnames(gmailSparse))

fmailSparse$fraud<- fmail$fraud
gmailSparse$fraud<- gmailred$fraud

fmailSparse$fraud<- as.factor(fmailSparse$fraud)
gmailSparse$fraud<- as.factor(gmailSparse$fraud)

# Seperating the data into training and testing models. 
# Will have equal amounts of fraud emails as good emails in first training set.

sampleamt<-3000

fmailsamp<-fmailSparse[sample(nrow(fmailSparse),sampleamt),]
gmailsamp<-gmailSparse[sample(nrow(gmailSparse),sampleamt),]
fgmailsamp<-rbind.fill(fmailsamp,gmailsamp)
fgmailsamp[is.na(fgmailsamp)]<-0

train<-fgmailsamp[sample(nrow(fgmailsamp),2*sampleamt),]

fgmailred<-rbind.fill(fmailSparse,gmailSparse)
fgmailred[is.na(fgmailred)]<-0

#Some observations in test missing why? Also not randomized.
test<-suppressMessages(anti_join(fgmailred,train))


#Building a logistic model

# glm.train<-suppressWarnings(glm(fraud~.,data=train,family=binomial(link='logit')))
# 
# summary(glm.train)
# 
# n<-length(train$fraud)
# training<-suppressWarnings(step(glm.train,fraud~.,direction = 'both',k=log(n),trace=0))
# summary(training)



# Let's try a LASSO model instead


grid=10^seq(10,-2,length=100)

x=model.matrix(fraud~.,train)[,-which(names(train) %in% c("fraud"))]

lassotrainx<-as.matrix(train[,-which(names(train) %in% c("fraud"))])
lassotrainy<-as.numeric(train$fraud)
lassotrainy[lassotrainy==2]<-10


lassotestx<-as.matrix(test[,-which(names(test) %in% c("fraud"))])
lassotesty<-as.numeric(test$fraud)
lassotesty[lassotesty==2]<-10


lasso.mod=glmnet(x=lassotrainx,y=lassotrainy,alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(lassotrainx,lassotrainy,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

lasso.pred=predict(lasso.mod,s=bestlam,newx=lassotestx)
lasso.pred<-ifelse(lasso.pred<=5.5,1,10)
lassoerr<-mean(lasso.pred != lassotesty)

out=glmnet(x,lassotrainy,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef<-lasso.coef [which(rowSums(lasso.coef) > 0), ]
lasso.coef<-as.data.frame(t(lasso.coef))
lasso.coef.names<-names(lasso.coef)


# Let's attempt a Ridge model

ridge_model <- glmnet(lassotrainx, lassotrainy, alpha=0, lambda=grid)
plot(ridge_model, xvar = "lambda", main = "Ridge")
ridge_model_cv <- cv.glmnet(lassotrainx,lassotrainy, alpha=0)
plot(ridge_model_cv)

ridge_model_train <- glmnet(lassotrainx,lassotrainy,alpha=0,
                            lambda=grid,thresh=1e-12)
ridge_pred <- predict(ridge_model_train, s=ridge_model_cv$lambda.min,
                      newx=lassotestx)
ridge_pred<-ifelse(ridge_pred<=5.5,1,10)
ridgeerr<-mean(ridge_pred!=lassotesty)

# Principle Component

library(pls)

train$fraud<-as.numeric(train$fraud)
pcr.fit=pcr(lassotrainy~lassotrainx, scale=TRUE, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit,lassotestx,ncomp=91)
pcr.pred<-ifelse(pcr.pred<=5.5,1,10)
pcrerr<-mean(pcr.pred!=lassotesty)

# pcr.fit=pcr(y~X,scale=TRUE,ncomp=7)
# summary(pcr.fit)

###################################################################################################
# partial

pls_train <- plsr(lassotrainy~ lassotrainx, scale=TRUE, validation="CV")
summary(pls_train)
pls.pred <-predict(pls_train, lassotestx,ncomp=2)
pls.pred<-ifelse(pls.pred<=5.5,1,10)
plserr<-mean(pls.pred!=lassotesty)



lassoerr
ridgeerr
pcrerr
plserr

# Trying classification trees

train$fraud<-as.factor(train$fraud)

test$fraud<-as.numeric(test$fraud)
test$fraud<-as.factor(test$fraud)


train$fraud<-as.numeric(train$fraud)
train$fraud[train$fraud!=1]<-0
train$fraud[is.na(train$fraud)]<-0
train$fraud<-as.factor(train$fraud)
test$fraud<-as.numeric(test$fraud)
test$fraud[test$fraud!=1]<-0
test$fraud[is.na(test$fraud)]<-0
test$fraud<-as.factor(test$fraud)

#train$dear<-NULL
#test$dear<-NULL


tree.train<-tree(fraud~.,train)
summary(tree.train)

par(mfrow=c(1,1))
plot(tree.train)
text(tree.train,pretty=0)

tree.test<-predict(tree.train,test,type = "class")
table(tree.test,test$fraud)
tree.err<-mean(tree.test!=test$fraud)

(705+940)/(705+940+13)

# Now let's try some pruning methods

cv.names_tree<-cv.tree(tree.train,FUN=prune.misclass)
names(cv.names_tree)

cv.names_tree$size
cv.names_tree$dev
cv.names_tree$k
cv.names_tree$method

par(mfrow=c(1,2))
plot(cv.names_tree$size,cv.names_tree$dev,type="b")
plot(cv.names_tree$k,cv.names_tree$dev,type="b")

prune.names_tree<-prune.misclass(tree.train,best=8)
plot(prune.names_tree)
text(prune.names_tree,pretty=0)

tree.test.pred<-predict(prune.names_tree,test,type = "class")
table(tree.test.pred,test$fraud)

#No difference because best was at size 8

# Attempting a bagging model

bag.email<-randomForest(fraud~.,data=train,mtry=801,importance=TRUE)
bag.email

bag.yhat<-predict(bag.email,newdata=test)
plot(bag.yhat,test$fraud)
abline(0,1)
bag.err<-mean(bag.yhat!=test$fraud)

# Attempting a random forest model

pred<-ncol(train)-1

rf.email<-randomForest(fraud~.,data=train,mtry=round(sqrt(pred)),importance=TRUE)
rf.email

rf.yhat<-predict(rf.email,newdata=test)
plot(rf.yhat,test$fraud)
abline(0,1)
rf.err<-mean(rf.yhat!=test$fraud)
table(rf.yhat,test$fraud)


varImpPlot(rf.email)

# Excellent result of about .1%

# Attempting a boosting model

train$fraud<-as.numeric(as.character(train$fraud))
test$fraud<-as.numeric(as.character(test$fraud))

set.seed(1)

boost.email<-gbm(fraud~.,data=train,shrinkage=0.01,distribution = "bernoulli",n.trees=5000,verbose = F)
summary(boost.email)

par(mfrow=c(1,2))
plot(boost.email,i="rm")
plot(boost.email,i="lstat")

boost.yhat=predict(boost.email,newdata=test,n.trees=5000)
boost.yhat<-ifelse(boost.yhat>=0.5,1,0)
boost.err<-mean(boost.yhat!=test$fraud)
boost.err

shrink.boost.email<-gbm(fraud~.,data=train,distribution = "bernoulli",n.trees=5000,interaction.depth = 4,
                        shrinkage = 0.2,verbose = F)
shrink.boost.yhat=predict(shrink.boost.email,newdata=test,n.trees=5000)
shrink.boost.err<-mean(shrink.boost.yhat!=test$fraud)

# Attempting nonlinear SVM
# 
# svmfit<-svm(fraud~.,data=train,kernel="radial",gamma=1,cost=1)
# plot(svmfit,train)
# 
# set.seed(1)
# #tune.out<-tune(svm,fraud~.,data=train,kernel="radial",ranges= list(cost=c(0.1,1,2,5,10,100,300,500,1000)),gamma=c(0.5,1,2,3,4))
# 
# tune.out=tune(svm , fraud~., data=train, kernel ="radial",
#               ranges =list(cost=c(0.1 ,1 ,10),
#                            gamma=c(0.1,0.25,0.5) ))
# summary(tune.out)
# 
# 
# table(truth=test$fraud,pred=predict(tune.out$best.model,newdata = subset(test,select = -fraud)))




#Building a logistic model with variables "selected" by LASSO

# logtrain<-train[,which(names(train) %in% cbind(lasso.coef.names,"fraud"))]
# logtest<-test[,which(names(test) %in% cbind(lasso.coef.names,"fraud"))]
# 
# glm.train<-suppressWarnings(glm(fraud~.,data=logtrain,family=binomial(link='logit')))
# 
# summary(glm.train)
# 
# n<-length(logtrain$fraud)
# training<-suppressWarnings(step(glm.train,fraud~.,direction = 'both',k=log(n),trace=0))
# summary(training)

#Trying superlearner package


#Using the superlearner to compare mean, lasso, and random forest model, knn. If mean model is best, something is wrong
# sl = SuperLearner(Y = as.numeric(train$fraud), X = train[,which(!(names(train) %in% "fraud"))], family = binomial(),
#                  SL.library = c("SL.mean", "SL.glmnet", "SL.ranger","SL.knn"))
# sl