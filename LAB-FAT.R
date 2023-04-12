
####### NORMALIZING VALUES IN R #######


# Create data
gfg <-c(244, 753, 596, 645, 874, 141,639, 465, 999, 654)

# : Normalize data with log transformation in base R
# normalizing data
gfg <-log(gfg)
gfg

#  Normalize Data with Standard Scaling in R

# Create data
gfg <- c(244,753,596,645,874,141,639,465,999,654)

# normalizing data
gfg <- as.data.frame(scale(gfg)) 
gfg


# Normalize Data using Min-Max Scaling

library(caret)

# Create data
gfg <- c(244,753,596,645,874,141,639,465,999,654)

# normalizing data
ss <- preProcess(as.data.frame(gfg), method=c("range"))

gfg <- predict(ss, as.data.frame(gfg))
gfg



#####################################


###### READING XLSX FILE IN R #############
library(readxl)
library(ggplot2)

Data_gfg <- read_excel("Data_gfg.xlsx")

############################

#1. Describe the dataset and visualize the data using suitable plots and write the inference.
df<-read.csv("glass.csv")
head(df)
colnames(df)
df[1]
hist(df$X1)
plot(df)
boxplot(df)

# this is the variable which we have to classify
unique(df$X1.2)

tb<-table(df$X1.2)
pie(tb)

barplot(df$X1.2)


# 2. Perform the exploratory data analytics with necessary explanation
ncol(df)
nrow(df)
names(df)
str(df)
class(df[,1])
class(df[,2])
summary(df)
sd(df$X1.2)
quantile(df$X1.2)
table(df$X1.2)


# 3. Perform the necessary pre-processing steps and mention them clearly.

# cleaning the dataset
df<-na.omit(df)
sum(is.na(df))

## encoding the categorical data
# df$Country=factor(df$Couuntry,
                 # levels=c('France','Spain','Germany')
                  # labels=c(1.0,2.0,3.0))



# 4. Perform correlation analysis and visualize using necessary plots. Write the inference 
# clearly.

library(corrplot)

t<-cor(df)
t
corrplot(t,method="color")



# 5. Select the appropriate variables and justify your selection.

df<-df[,c(1,3,4,5,9,11)]
names(df)
corrplot(cor(df))

# spliting the dataset into 80-20 %.
set.seed(123)
train_index<-sample(1:nrow(df),0.8*nrow(df))

train_data<-df[train_index,]
test_data<-df[-train_index,]
nrow(train_data)
nrow(test_data)


# 6. Apply the following classification algorithms to predict the target variable that yields 
# best performance by using 80/20 split.

# a) Logistic Regression
# b) Naïve Bayes Classifier


###### LOGISTIC REGRESSION  ######

library(caret)
library(nnet)

logis_reg<-glm(df$X1.2~.,data=df,family="binomial")
logis_reg
summary(logis_reg)

predict_reg<-predict(logis_reg,test_data,type="response")
head(predict_reg)
predict_reg<-ifelse(predict_reg>0.5,1,0)

tb<-table(test_data$X1.2,predict_reg)
acc<- mean(predict_reg == test_data$X1.2)
acc

logis_mul_reg<-nnet::multinom(df$X1.2~.,data=df)
summary(logis_mul_reg)

predict_reg<-predict(logis_mul_reg,newdata = df)
tb<-table(df$X1.2,predict_reg)
confusionMatrix(tb)


###############################


#### NAIVE BAYES #############

library(e1071)
library(caTools)
library(caret)
naive_bay<-naiveBayes(df$X1.2~.,data=df)
naive_bay
y_pred<-predict(naive_bay,newdata=test_data)
cm<-table(test_data$X1.2,y_pred)
confusionMatrix(cm, mode = "everything")
acc<-mean(y_pred==test_data$X1.2)
acc



###########################



##### DECISION TREE


# Load the rpart package
library(rpart)

# Use Information Gain as the splitting criterion
df_tree_info_gain <- rpart(train_data$X1.2 ~ ., data = train_data, 
                           method = "class", parms = list(split = "information"))

# Use Gini Index as the splitting criterion
df_tree_gini_index <- rpart(train_data$X1.2 ~ ., data = train_data, 
                            method = "class", parms = list(split = "gini"))


# 2
# Load the rpart.plot package
library(rpart.plot)

# Visualize the decision tree using Information Gain as the splitting criterion
rpart.plot(df_tree_info_gain, 
           main = "Decision Tree - Information Gain", type = 2, extra = 101)

# Visualize the decision tree using Gini Index as the splitting criterion
rpart.plot(df_tree_gini_index, 
           main = "Decision Tree - Gini Index", type = 2, extra = 101)

pred = predict(df_tree_info_gain, test_data, type = "class")
confusionMatrix(factor(test_data$X1.2), factor(pred))
acc<-mean(pred==test_data$X1.2)
acc



###################



############ random forest #######


library(caret)
library(randomForest, warn.conflicts = FALSE,quietly = TRUE)
train_data$X1.2 <- as.factor(train_data$X1.2)
test_data$X1.2 <- as.factor(test_data$X1.2)
rf1 <- randomForest(X1.2 ~., data =train_data, ntree = 100, mtry = 5)
plot(rf1)

y_pred<-predict(rf1,newdata=test_data)
a<-table(test_data$X1.2,y_pred)
cm<-confusionMatrix(a)
cm


# data.pred <- predict(data.rf, newdata=data.test[-14])
# table(data.test[,14],data.pred



################################



##### SVM ##############

train_data$X1.2 <- as.factor(train_data$X1.2)
test_data$X1.2 <- as.factor(test_data$X1.2)
svm_model<-svm(train_data$X1.2~.,data=train_data,type="C-classification",kernel="linear")
svm_model

actual <- train_data$X1.2
svm_model$fitted
tb<-table(svm_model$fitted,train_data$X1.2)
confusionMatrix(tb)


svm_model<-svm(train_data$X1.2~.,data=train_data,kernel="radial")
tb<-table(svm_model$fitted,train_data$X1.2)
confusionMatrix(tb)


svm_model<-svm(train_data$X1.2~.,data=train_data,kernel="sigmoid")
tb<-table(svm_model$fitted,train_data$X1.2)
confusionMatrix(tb)

svm_model<-svm(train_data$X1.2~.,data=train_data,kernel="poly")
tb<-table(svm_model$fitted,train_data$X1.2)
confusionMatrix(tb)




##############################



###### CLUSTERING ALGORITHM #########


## K-MEANS CLUSTERING

library(caret)
library(cluster)
kmeansClu<-kmeans(train_data,centers=10,nstart=20)
kmeansClu
kmeansClu$cluster
cm<-table(train_data$X1.2,kmeansClu$cluster)
cm
confusionMatrix(cm)

wss<-c()

for(i in 1:10){
  kmeans_fit<-kmeans(train_data,centers =i,nstart=10)
  wss[i]<-kmeans_fit$tot.withinss
}
plot(1:10,wss,type="b")

clusplot(train_data,kmeansClu$cluster)


## k-mediod clustering

pam.res<-pam(train_data,2)
print(pam.res)
pam.res$medoids
head(pam.res$clustering)
plot(pam.res,data=df)


## hirerachical clustering

distance_mat<-dist(train_data,method="euclidean")
distance_mat
hierarchical_clust<-hclust(distance_mat,method="average")
hierarchical_clust
plot(hierarchical_clust)



###################################



##################################

# 7.Compare both the classification models and tabulate the results in terms of suitable 
# performance metrics. Also, plot the comparison graph ( X-axis : Method names, Y-axis: 
                                                        # Performance metrics) 
acc_mod1<-96
acc_mod2<-97
y<-c(acc_mod1,acc_mod2)
barplot(y,xlab="Model-1 and Model-2",ylab="Accuracy of Models",main="Comparing both models")


###############################


#################################

# 8. Write the conclusion mentioning the best model for the given data set, by mentioning 
# the F1 score of both the models.


install.packages("MLmetrics")
library("MLmetrics")


## A low F1 score is an indication of both poor precision and poor recall.
## F1 value can be calculated using the confusion matrix

###################################


############ TIME SERIES FORECASTING IN R ##########


install.packages("forecast")
library('forecast')
library(ggplot2)
library(dplyr)
library(fable)

class(AirPassengers)
plot(AirPassengers)
head(AirPassengers)

## plotting observed,trend,seasonal and radom trend 
data<-ts(AirPassengers,frequency = 12)
d<-decompose(data,"multiplicative")
d1<-decompose(data,"additive")
plot(d1)
plot(d)

## (METHOD-1) Now we forecast 10 years of data by using Arima() function.

model<-auto.arima(AirPassengers)
f<-forecast(model,level=c(95),h=10*12)
# plot(f)
accuracy(f)

## (METHOD-2) ETS MODEL
data_ts <- as_tsibble(data, key = "date")
ets_model<-data_ts%>%model(ETS(value))
# Plot the forecast
autoplot(data_ts, level = "series") + 
  ggtitle("ETS Model Forecast Plot") +
  xlab("Year") + 
  ylab("Value") + 
  geom_point() +
  geom_line() 

## apply autocorrelation
acf(data)

####################################################



########## SENTIMENTAL ANALYSIS ##################


# load required libraries
library(tidytext)
library(dplyr)

# load data
data <- read.csv("reviews.csv")

# perform sentiment analysis using Bing lexicon
data_sentiment <- data %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(review_id) %>%
  summarise(sentiment = sum(ifelse(sentiment == "positive", 1, -1)))

# classify reviews as positive or negative based on sentiment score
data_sentiment$review_sentiment <- ifelse(data_sentiment$sentiment >= 0, "Positive", "Negative")

# select top 5 positive and negative reviews
top_positive_reviews <- data_sentiment %>%
  filter(review_sentiment == "Positive") %>%
  arrange(desc(sentiment)) %>%
  top_n(5)

top_negative_reviews <- data_sentiment %>%
  filter(review_sentiment == "Negative") %>%
  arrange(sentiment) %>%
  top_n(5)

# print top 5 positive and negative reviews
cat("Top 5 positive reviews:\n")
cat("Review ID\tSentiment Score\n")
cat(paste(top_positive_reviews$review_id, "\t", top_positive_reviews$sentiment, "\n"))

cat("Top 5 negative reviews:\n")
cat("Review ID\tSentiment Score\n")
cat(paste(top_negative_reviews$review_id, "\t", top_negative_reviews$sentiment, "\n"))





#################################################




######################### ENSEMBLE TECHNIQUE ##############


## LOGISTIC REGRESSION
ele_logistic<-glm(train_data$X1.2~.,data=train_data)
summary(ele_logistic)

predict_reg<-predict(ele_logistic,newdata = test_data,type="response")
predict_reg<-ifelse(predict_reg>0.5,1,0)
tb<-table(test_data$X1.2,predict_reg)
confusionMatrix(tb)
acc<-mean(test_data$X1.2==predict_reg)
acc

## naive bayes
naive_bay<-naiveBayes(test_data$X1.2~.,data=test_data)
naive_bay
predict_reg<-predict(naive_bay,newdata = test_data)
tb<-table(test_data$X1.2,predict_reg)
confusionMatrix(tb)


###########################################################



######### LINEAR REGRESSION IN R ##########

data <- read.csv("GrocerySales.csv");
df <- data
head(data)

hist(data$Sales,col = "orange",main="Sales",xlab = "Sales")

data$Sales <- as.data.frame(scale(data$Sales))
head(data$Sales)

cor(data$Size,data$AdvertisingRate)
library(ggplot2)
ggplot(df,aes(x=Size,y=Sales))+geom_point()+geom_smooth(method='lm');


set.seed(7)
training.samples <- createDataPartition(df$Sales, p = 0.80, list = FALSE)
train.data <- df[training.samples, ]
test.data <- df[-training.samples, ]


### Linear regression
model1 <- lm(Sales ~., data = train.data)
summary(model1)

### Non Linear Regression Model
library(stats)

# Create a dataset with two variables: x and y
x <- df$Store
y <- df$Sales

# Define the nonlinear regression model
model2 <- nls(y ~ a * x^b, start=list(a=1, b=1))

# Fit the model to the data
fit <- nls(model2, data=data.frame(x,y))
# Print the model coefficients
coef(fit)

# Predict the values of y for a new set of x values
x_new <- 70:140
predict(fit, newdata=data.frame(x=x_new))


### ploynomial Regression analysis
x <- df$Store
y <- df$Sales

# Create a new data frame with additional columns for the powers of x
df <- data.frame(x, y)
df$x2 <- x^2
df$x3 <- x^3

# Fit a linear regression model to the data
model3 <- lm(y ~ x + x2 + x3, data=df)

# Print the model coefficients
coef(model3)

x_new <- 70:140
df_new <- data.frame(x=x_new, x2=x_new^2, x3=x_new^3)
predict(model3, newdata=df_new)


# Compare all the regression models in terms of the following performance metrics
# i) Mean Absolute Error
# ii) Mean Square Error 
# iii) Root Mean Square Error
# iv) R-Squared
# v) Adjusted R-squared.
s1 <- summary(model1)
s1
s2 <- summary(model2)
s2
s3 <- summary(model3)
s3

rbind(s1,s2,s3)

##########################################




############ DECISION TREE IN R ############



library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)


data("readingSkills")
head(readingSkills)

sample_data = sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)
nrow(train_data)
nrow(test_data)

model<-ctree(nativeSpeaker~.,train_data)
plot(model)

library(caret)
# predict 
predict_model<-predict(model,test_data)
m_at <- table(test_data$nativeSpeaker, predict_model)
m_at
confusionMatrix(m_at)


###########################################




########## Radom Forest in R #############


library(caTools)
library(randomForest)

# Splitting data in train and test data
split <- sample.split(iris, SplitRatio = 0.7)
split

train <- subset(iris, split == "TRUE")
test <- subset(iris, split == "FALSE")
nrow(train)
nrow(test)

set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[-5],
                             y = train$Species,
                             ntree = 500)
plot(classifier_RF)
classifier_RF

y_pred = predict(classifier_RF, newdata = test[-5])

# Confusion Matrix
confusion_mtx = table(test[,5], y_pred)
confusion_mtx
acc<-mean(y_pred==test$Species)
acc




######################################



########### KNN IN R #################


library(e1071)
library(caTools)
library(class)


data(iris)
head(iris)

# Splitting data into train
# and test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 1)
classifier_knn

cm <- table(test_cl$Species, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))




#####################################





################################ Ensembling Learning in R ###############



library(caret)
library(caretEnsemble)


ir = iris[iris$Species!="setosa", ]
ir$Species = factor(ir$Species)

unique(ir$Species)

indexes = createDataPartition(ir$Species, p = .90, list = FALSE)
train = ir[indexes, ]
test = ir[-indexes, ]

methods = c("gbm", 'rpart','rf')


tc = trainControl(method = "repeatedcv", number = 5, 
                  repeats = 3, classProbs = TRUE)


models = caretList(Species~., data = train, 
                   trControl = tc, methodList = methods)

output = resamples(models)
summary(output)



###########################################################





########## SVM WITH GRID SEARCH AND CROSS VALIDATION #########


library(caret)
library(e1071)

# load data
data(iris)
x <- iris[, 1:4]
y <- iris[, 5]

# define the model
svm_model <- train(x, y,
                   method = "svmRadial",
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "cv", number = 5))

# print the results
print(svm_model)

# make predictions on the test set
test_pred <- predict(svm_model, newdata = x)

# calculate accuracy
acc <- sum(test_pred == y) / length(y)
cat("Accuracy:", acc)





#####################################################


