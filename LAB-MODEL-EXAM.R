
### SPLITTING DATASET

set.seed(123)
split=sample.split(df$Purchased,SplitRatio = 0.70)

training_set=subset(df,split==TRUE)
test_set=subset(df,split=FALSE)

#######

###############################

# WHEAT DATASET CODE

seeds_df <- read.csv(file = "../../../seeds.csv", header = T)
summary(seeds_df)

plot(seeds_df)

## conclusion: We can see some pretty discernable clustering with respect to area and perimeter. We can dig in a little deeper and use ggplot’s density function to observe the distribution of area for each seed type.
## Note that we convert the seed type to a factor, since ggplot sometimes doesn’t play well with mapping continuous variables to color.

library(ggplot2)
seeds_df$TypeFac <- as.factor(seeds_df$Type)
ggplot(seeds_df, aes(x = Area, color = TypeFac, fill = TypeFac)) + geom_density(alpha = 0.2) + theme_minimal()


library(gridExtra)
seeds_df$TypeFac <- as.factor(seeds_df$Type)
p1 <- ggplot(seeds_df, aes(x = Kernel.Groove, color = TypeFac, fill = TypeFac)) + geom_density(alpha = 0.2) + theme_minimal()
p2 <- ggplot(seeds_df, aes(x = Kernel.Groove, y = Kernel.Length, color = TypeFac)) + geom_point() + theme_minimal()
grid.arrange(p1,p2, ncol = 1)

## CONSLUSION: There seems to exist a significant overlap between the distribution of seed types 1 & 3, 
## but there is almost no overlap between those two and the distribution for type 2. 
## If we look at the color coded scatter plot, we can tell that the three clusters are segregated particularly well in the ‘Kernel Groove’ - ‘Kernel Length’ plane.


plot1 <- ggplot(seeds_df, aes(x = Kernel.Groove, y = Perimeter, color = TypeFac)) + geom_point() + theme_minimal() +theme(legend.position="none")
plot2 <- ggplot(seeds_df, aes(x = Asymmetry.Coeff, y = Perimeter, color = TypeFac)) + geom_point() + theme_minimal() +theme(legend.position="none")
plot3 <- ggplot(seeds_df, aes(x = Kernel.Width, y = Compactness, color = TypeFac)) + geom_point() + theme_minimal() +theme(legend.position="none")
plot4 <- ggplot(seeds_df, aes(x = Kernel.Length, y =Compactness, color = TypeFac)) + geom_point() + theme_minimal() +theme(legend.position="none")
grid.arrange(plot1,plot2,plot3,plot4, ncol = 2)


## CONCLUSION: The most visible seperation we can note is on the ‘Kernel Groove - Perimeter’ plane, and the ‘Compactness - Kernel.Width’ plane. Lets train our SVM model to classify seed type based off of ‘Kernel Groove’, ‘Kernel Length’, ‘Asymmetry Coeff’ and ’Perimeter.
## We’ll start by randomly splitting our dataset into a training and test set.

SampleIndex <- sample(1:nrow(seeds_df), 145)
seeds_training <- seeds_df[SampleIndex,]
seeds_testing <- seeds_df[-SampleIndex,]
library(e1071)


### SVM

set.seed(151)
svm_model <- svm(Type ~  Kernel.Length + Kernel.Groove + Perimeter + Asymmetry.Coeff, data = seeds_training, scale = FALSE)
actual <- seeds_training$TypeFac
table(round(svm_model$fitted), actual)

tunesvm <- tune(svm, kernel = "radial", data = seeds_training, Type ~ Kernel.Length + Kernel.Groove + Perimeter + Asymmetry.Coeff, ranges = list(epsilon = seq(0,.5,0.1), cost = 2^(1:7)))
plot(tunesvm)

svm_model <- tunesvm$best.model

svm_model2 <- svm(Type ~., data = seeds_training)
table(round(svm_model2$fitted), seeds_training$Type)

## Wow 100%! Lets see how this model fares with our testing set! Using the predict() function,
## we can predict the outcomes of our testing set based on the ‘svm_model’ object.
## table(round(predict(svm_model2,seeds_testing)), seeds_testing$Type)


### RANDOM FOREST

library(randomForest, warn.conflicts = FALSE,quietly = TRUE)
seeds_training$Type <- as.factor(seeds_training$Type)
rf1 <- randomForest(Type ~., data = seeds_training, ntree = 100, mtry = 6)
plot(rf1)

table(rf1$predicted, seeds_training$Type)

## Wow, random forest gave us spot on results without having to tune our ML model. 
## Let’s see how the testing set turns out.

table(predict(rf1, seeds_testing), seeds_testing$Type)



### DECISION TREE

colnames(df)[8]<-"Class"
colnames(df)

# Split the dataset into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(df), 0.7*nrow(df))
train_data <- df[train_index,]
test_data <- df[-train_index,]

# Load the rpart package
library(rpart)

# Use Information Gain as the splitting criterion
df_tree_info_gain <- rpart(Class ~ ., data = train_data, 
                           method = "class", parms = list(split = "information"))

# Use Gini Index as the splitting criterion
df_tree_gini_index <- rpart(Class ~ ., data = train_data, 
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
confusionMatrix(factor(test_data$Class), factor(pred))




### CLUSTERING 

library(factoextra)
library(NbClust)

fviz_nbclust(seed_pcs_dat[, c(1, 2, 4, 5)], kmeans, method = "wss")

## After computing the clusters, the points are plotted on the first 2 PCs, with the type of wheat as well as the cluster number both labeled. In the plot, the clusters closely match to the wheat type, 
## suggesting that the most notably grouping of the data points are indeed the type of wheat seed.


k <- 3
k_clusters <-
  kmeans(
    seed_pcs_dat[, c(1, 2, 4, 5)],
    centers = k,
    nstart = 25,
    iter.max = 1000
  )
seed_pcs_cluster <- data.frame(seed_pcs_dat)
seed_pcs_cluster$cluster <- as.factor(k_clusters$cluster)
ggplot(seed_pcs_cluster, aes(x = PC1, y = PC2, color = type, shape = cluster)) +
  geom_point(size = 1.5) + ggtitle("Cluster Type Plot")

seed_pcs_cluster_renamed <- data.frame(seed_pcs_cluster)

# Summarize results of clustering.
ggplot(data = seed_pcs_cluster_renamed, aes(fill = type)) + aes(x = cluster) + 
  geom_bar() + xlab("Cluster") + ylab("Count") + ggtitle("Clusters with 3 PCs")

## The mean and standard deviation of the clusters with their corresponding types are listed below. Cluster 2, 3 and type 1, 2 have almost identical mean and standard deviation. This suggests that the distribution between the clusters and the types are similar. 
## However, the mean and standard deviation of the first cluster is a little different from type 0, which corresponds to the fact that it is intermixed with other types more than the other clusters.


#######################################


# READING XLSX FILE IN R

library(readxl)
library(ggplot2)

Data_gfg <- read_excel("Data_gfg.xlsx")


# RANDOM FORST 



# LAB-10 SENTIMENTAL ANALYSIS

library(dplyr)
library(forcats)
library(ggplot2)
library(caTools)
library("tm")
library("SnowballC")
library("wordcloud")
library("wordcloud")
library("RColorBrewer")
library(randomForest)

df<-read.csv("E:/VIT SEM-6/EDA/LAB/LAB10/Tweets.csv")
head(df)

ggplot(df,aes(x=airline_sentiment_confidence,fill=airline_sentiment))+geom_density(alpha=0.5)


ggplot(df, aes(x = airline,fill = airline_sentiment ))+
  geom_bar(stat = "count")

# load the data as a corpus
# Load the data as a corpus
docs <- VCorpus(VectorSource(df$text))
#To replace special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#convert to lower case
docs <- tm_map(docs,content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("flight","unit","usairway","americanair","southwestair","jetblu","get","can","virginamerica","united","delta")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

#Text to Matrix
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


dtm = DocumentTermMatrix(docs)
dtm = removeSparseTerms(dtm,sparse = 0.99)
model_data = as.data.frame(as.matrix(dtm))
model_data$sentiment = df$airline_sentiment
set.seed(123)

split = sample.split(model_data$sentiment,SplitRatio = 0.9)
train = subset(model_data,split = TRUE)
test = subset(model_data,split = FALSE)


# LAB-9 CLUSTERING ALGORITHMS

df<-read.csv("E:/VIT SEM-6/EDA/LAB/LAB9/Credit Card Customer Data.csv")
head(df)

## K-MEANS CLUSTERING
library(ModelMetrics)
set.seed(240)
kmeans.re<-kmeans(df,centers=10,nstart=20)
kmeans.re
kmeans.re$cluster
cm<-table(df$Total_Credit_Cards,kmeans.re$cluster)
cm

### determining the optimal number of cluster(elbow method)

x<-df[,c("Total_Credit_Cards","Total_visits_bank")]

x_scaled<-scale(x)

wss<-c()

for(i in 1:10){
  kmeans_fit<-kmeans(x_scaled,centers =i,nstart=10)
  wss[i]<-kmeans_fit$tot.withinss
}
plot(1:10,wss,type="b")

y_kmeans <- kmeans.re$cluster

library(cluster)

clusplot(df[, c("Total_visits_bank", "Total_Credit_Cards")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Credit card dataset"),
         xlab = 'Total_visits_bank',
         ylab = 'Total_Credit_Cards')


# k-mediod Clustering

pam.res<-pam(df,2)
print(pam.res)
pam.res$medoids
head(pam.res$clustering)
plot(pam.res,data=df)

# hierarchical clustering method
distance_mat<-dist(df,method="euclidean")
table(fit)
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl
plot(Hierar_cl)
abline(h=110,col="green")
fit<-cutree(Hierar_cl,k=3)
fit
rect.hclust(Hierar_cl,k=3,border="green")


# LAB-8 (Ensemble methods)


# LAB-7 (DECISION TREE)

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tree)

train <- read.csv("train.csv", na.strings=c(""," ","NA"))
head(train,2)

test <- read.csv("test.csv", na.strings=c(""," ","NA"))
tail(test,2)

decision_tree = rpart(train$Survived ~ ., data = train)
plotcp(decision_tree)

printcp(decision_tree)

fig(10, 6)
fit <- rpart(Survived~., data = train, method = "class")
rpart.plot(fit, extra = 106)

decision_tree = rpart(train$Survived ~ ., data = train, cp = 0.015)

pre = predict(decision_tree, test)

Survived <- ifelse(pre >= 0.5, 1,0)

result <- data.frame(PassengerId, Survived)
glimpse(result)

confusionMatrix(factor(Survived), factor(train$Survived[1:418]))


# LAB-7 (SVM CLASSIFIER)

library(naivebayes)
library(dplyr)
library(ggplot2)

df<-read.csv("E:/VIT SEM-6/EDA/LAB/LAB7/suv_data.csv",stringsAsFactors = TRUE)

library(naivebayes)
library(dplyr)
library(ggplot2)
library(caTools)
library(caret)


df=df[3:5]
head(df)

df<-na.omit(df)
sum(is.na(df))

df%>%ggplot(aes(x=Purchased,y=EstimatedSalary))+geom_line()

df%>% ggplot(aes(x=Purchased,y=EstimatedSalary,fill=Purchased,group=Purchased))+geom_boxplot()


# SPLITTING THE DATASET INTO TRAINING AND TESTING DATASET

library(caTools)
set.seed(123)
split=sample.split(df$Purchased,SplitRatio = 0.70)

training_set=subset(df,split==TRUE)
test_set=subset(df,split=FALSE)

training_set[-3]=scale(training_set[-3])
test_set[-3]=scale(test_set[-3])

# LINEAR KERNEL

library(e1071)
classifier_svm=svm(Purchased~.,
                   data=training_set,
                   type="C-classification",
                   kernel="linear")

classifier_svm

test_set[-3]

y_pred=predict(classifier_svm,newdata =test_set[-3])
y_pred

cm=table(test_set[,3],y_pred)
cm

confusionMatrix(cm)

# RADIAL KERNEL

classifier_svm=svm(formula=Purchased~.,
                   data=training_set,
                   type="C-classification",
                   kernel="radial")
classifier_svm

y_pred<-predict(classifier_svm,test_set[-3])
y_pred

cm<-table(test_set[,3],y_pred)
cm

precision<-28/(28+7)
recall<-28/(28+15)
f1<-(2*precision*recall)/(precision+recall)
precision


# LAB-6 (NAIVEBAYES CLASSIFIER)

# Binary Classification using naive bayes

df<-na.omit(df)
sum(is.na(df))

split<-sample.split(df$Purchased,SplitRatio = 0.70)

train_data<-subset(df,split==TRUE)
test_data<-subset(df,split==FALSE)

classifier_cl <- naiveBayes(Purchased ~ ., data = train_data)
classifier_cl

y_pred<-predict(classifier_cl,newdata = test_data)

cm <- table(test_data$Purchased, y_pred)
cm
confusionMatrix(cm)

# Multiclass classification using NAIVE BAYES

# do the same as binary classificaton


# LAB-5 (losigtic regression)

df<-read.csv("adv_newspaper.csv")
df<-read.csv("E:/VIT SEM-6/EDA/LAB/LAB7/suv_data.csv")
sum(is.na(df))

# binary classification 

split <- sample.split(df, SplitRatio = 0.8)
split

train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")

head(train_reg)

lm <- glm(Purchased ~ Gender + Age+EstimatedSalary, 
          data = train_reg, 
          family = "binomial")
lm

predict_reg <- predict(lm,test_reg, type = "response")
predict_reg

predict_reg <- ifelse(predict_reg >0.5, 1, 0)
predict_reg

table(test_reg$Purchased, predict_reg)


## MULTI-CLASS CLASSIFICATION

df<-read.csv('Date_Fruit_Datasets.csv')
head(df)

library(caTools)
library(tidyverse)
library(caret)
library(nnet)

set.seed(123)
training.samples <- df$Class %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

model <- nnet::multinom(Class ~., data = train.data)

# Model accuracy
mean(predicted.classes == test.data$Class)

table(test.data$Class, predicted.classes)


# LAB-4 (ANOVA)

df<-read.csv("adv_newspaper.csv")
head(df)

## ONE-WAY ANOVA

av<-aov(data=df,Business~News)
av

av<-aov(data=df,Business~Sports)
av

av<-aov(data=df,News~Sports)
av

## TWO-WAY ANOVA

av<-aov(data=df,News~Sports+Business)
av


# LAB-3 (LOSIGTS REGRESSION)

df<-read.csv('suv_data.csv')
head(df)

train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")

head(train_reg)

lm <- glm(Purchased ~ Gender + Age+EstimatedSalary, 
          data = train_reg, 
          family = "binomial")
lm

predict_reg <- predict(lm,test_reg, type = "response")
predict_reg


predict_reg <- ifelse(predict_reg >0.5, 1, 0)
predict_reg

table(test_reg$Purchased, predict_reg)



# LAB-3 (Time series forecasting)

data = read.csv("monthly-beer-production-in-austr.csv")
head(data, 24)

ts_data = ts(data$Monthly.beer.production, start = c(1956, 1), end = c(1995, 12), frequency = 12)

# Decompose the time series
decomposed = decompose(ts_data)

# Plot the trend, seasonal, and residual components
plot(decomposed)

acf(ts_data)

acf(ts_data, lag.max = 50, main = "Autocorrelation Function")

fit = auto.arima(ts_data)

# Perform forecasting
forecasted = forecast(fit, h = 10)

# Evaluate the model
accuracy(forecasted)

additive_decomposed_ts <- decompose(ts_data, type = "additive")
plot(additive_decomposed_ts)

library(forecast)


# Convert the data into a tsibble
data_ts <- as_tsibble(ts_data, key = "date")

# Fit the ETS model
ets_model <- data_ts %>% model(ETS(value))

# Plot the forecast
autoplot(data_ts, level = "series") + 
  ggtitle("ETS Model Forecast Plot") +
  xlab("Year") + 
  ylab("Value") + 
  geom_point() +
  geom_line() 