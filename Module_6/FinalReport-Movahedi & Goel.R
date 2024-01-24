install.packages('tidyverse')
install.packages('MASS')
install.packages('car')
install.packages('e1071')
install.packages('caret')
install.packages('carTools')
install.packages('cowplot')
install.packages('pROC')
install.packages('ggcorrplot')
install.packages('corrplot')
install.packages('dplyr')
install.packages('janitor')
install.packages("mlbench")
install.packages('repr')
install.packages("caTools")
install.packages("randomForest")
install.packages('effects')
# loading libraries
library(repr)
library(tidyverse)
library(MASS)
library(car)
library(e1071)
library(caret)
library(carTools)
library(cowplot)
library(pROC)
library(ggcorrplot)
library(dplyr)
library(janitor)
library(mlbench)
library(data.table)
library(matrixStats)



# loading the training dataset
d15 <- read.csv("2015_Financial_Data.csv")
#looking at the dataset
dim(d15)
summary(d15)


#Gather missing data and plot graph to visualize missing data within variables
data_miss <- d15 %>% summarise_all(funs(sum(is.na(.))/n()))
data_miss <- gather(data_miss, key = "variables", value = "percent_missing")
data_miss2=subset(data_miss,data_miss$percent_missing>0.1)
ggplot(data_miss2, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

##Deleting variables from original dataset 
data2=subset(d15,select = -c(43,53,76,77,78,82,83,84,85,87,88,90,91,96,97,99,100,101,
                             103,105,112,114,120,127,128,129,130,132,143,146,148,149,150,
                             151,152,153,155,159,172,173,174,175,186,201,202,204,205,207,208,210,211,213,214))
data3=subset(data2,select = -c(80))
data4=subset(data3,select = -c(15,19,21,33,34,38,45,47,49,50,51,94,103,126,150))

#Modifying variable names
data4<-data4 %>% clean_names()
colnames(data4)

#Categorizing companies as small, medium, or large cap
market_cap_cat<-cut(data4$market_cap,breaks = c(0,1e+9,1e+10,1e+20),labels =c("Small","Mid","Large") )
data4$market_cap_cat=market_cap_cat
table(market_cap_cat)
data5=subset(data4,select = -c(100,138,136))

#Performing structural adjustments to the data
data5$x1=as.character(data5$x1)
data5$market_cap_cat=as.factor(data5$market_cap_cat)
data5$class=as.factor(data5$class)
data5$sector=as.factor(data5$sector)

#eliminate colinearity/non-essential variables 
fit01 = lm(x2016_price_var~.- x - sector - class - market_cap_cat,data5)
summary(fit01)

#Create new dataset elminating "N/A" variables
data6=subset(data5,select = -c(11,18,23,28,29,67,68,86,87,88,93,94,100,101,102,104,
                               105,107,110,124,125,126,128,129))

#Create a new variable to categorize price variance into broader baskets 
summary(data4$x2016_price_var)
AnalystRating<-cut(data4$x2016_price_var,breaks = c(-100,-50,-5.17,17.28,40.57,4000),
                   labels =c("Strong Sell","Sell","Hold","Buy","Strong Buy") )
AnalystRating[1:10]
data6$AnalystRating=AnalystRating

#Use tree based algorithm r-part to determine the most important variables in the dataset
rpartMod<-train(AnalystRating~.- x - sector - class - market_cap_cat-x2016_price_var,data = data6,method="rpart",na.action = na.exclude)
rpartImp<-varImp(rpartMod)
print(rpartImp)

#Putting selected variables into a dataset  
VariableCorr2=subset(data6,select = c('operating_cash_flow_sales_ratio' ,'operating_cash_flow','stock_based_compensation_to_revenue',
                                               'return_on_equity','net_profit_margin_2','net_income_per_share','eps','cash_per_share_2',
                                               'shareholders_equity_per_share','book_value_per_share' ))
colnames(VariableCorr2)
summary(VariableCorr2)


#Check that selected variables are not heavily correlated 
corr2<-round(cor(VariableCorr2),1)
ggcorrplot(corr2,lab = TRUE)

finalData=subset(data6,select = c('x','operating_cash_flow_sales_ratio' ,'operating_cash_flow','stock_based_compensation_to_revenue',
                                  'return_on_equity','net_profit_margin_2','net_income_per_share','eps','cash_per_share_2',
                                  'shareholders_equity_per_share','book_value_per_share',"AnalystRating",'sector','class'))
summary(finalData)
colnames(finalData)
# choosing the best variables based on random forest to make final variable selection better
# Loading package
library(caTools)
library(randomForest)
# Splitting data in train and test data
d15[is.na(d15)] <- 0
d15 <- subset(d15, select = - c(X2016.PRICE.VAR....)) 
split <- sample.split(d15, SplitRatio = 0.7)
split

train <- subset(d15, split == "TRUE")
test <- subset(d15, split == "FALSE")

# Fitting Random Forest to the train dataset
set.seed(100)  # Setting seed
classifier_RF = randomForest(x = train[-224],
                             y = train$Class,
                             ntree = 100)
# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-224])

# Plotting model
plot(classifier_RF)

varImpPlot(classifier_RF)
# Importance plot
k <- importance(classifier_RF)

# selecting important 10
df <- enframe(k)
df<-df[order(df$value,decreasing = TRUE),]
Names <- c(head(df$name, n = 15L))
varforest  <- d15[ , names(d15) %in% Names]
varforest<-varforest %>% clean_names()

#making final data better
finalData <- left_join(varforest,finalData)
finalData=subset(finalData,select = -c(7))

#Replace missing numerical values to take care of outliers
dt <- data.table(finalData[-1])
indx <- sapply(dt, \(x) !(x %in% boxplot(x, plot=FALSE)$out))
n <-dt[as.logical(rowProds(indx))]
n <- as.data.frame(n)
summary(n)
#Replace missing values of numeric variables
for(i in 1:ncol(n)){
  n[is.na(n[,i]), i] <- mean(n[,i], na.rm = TRUE)
}
n <- n %>% 
  na.omit()
summary(n)

finalData2 <- left_join(n,finalData)
finalData2=subset(finalData2,select = -c(6))
finalData2 <- finalData2 %>% 
  na.omit()

#EDA
options(repr.plot.width = 17, repr.plot.height = 10)
ggplot(finalData2, aes(x=sector,fill=AnalystRating))+ geom_bar()+ theme_bw() 

ggplot(finalData2, aes(x=AnalystRating, y=eps, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=.1, fill="white") + labs(title="EPS") 

ggplot(finalData2, aes(x=AnalystRating, y=return_on_equity, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Return on Equity")

ggplot(finalData2, aes(x=AnalystRating, y=net_profit_margin_2, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Net Profit Margin")

ggplot(finalData2, aes(x=AnalystRating, y=net_income_per_share, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="net income per share")

ggplot(finalData2, aes(x=AnalystRating, y=operating_cash_flow_sales_ratio, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="operating cash flow sales ratio")


finalData2 %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()

#finalData2
write.csv(finalData2,"~/Data/finalData2.csv", row.names = FALSE)
finalData2 <- read.csv('finalData2.csv')

names(finalData2)


#Question 1 : Are there any upper and lower outliers likely to skew results?
## Cook’s Distance Method
mod <- glm(class ~ .-AnalystRating-x, data = finalData2,family=binomial)
par(mfrow = c(2, 2))
library(effects)
plot(allEffects(mod))
summary(mod)
dev.off()
cooksd <- stats:::cooks.distance.glm(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- cooksd[(cooksd > (1 * mean(cooksd, na.rm = TRUE)))]
influential
ci <- ifelse(cooksd>4*mean(cooksd, na.rm=T),1,0)  # influential row numbers
table(ci)

names_of_influential <- names(influential)
outliers <- finalData2[names_of_influential,]
finalData2_without_outliers <- finalData2 %>% anti_join(outliers)
modn <- glm(class ~ .-AnalystRating-x, data = finalData2_without_outliers,family=binomial)
par(mfrow = c(2, 2))
plot(modn) 

# Question 2: What are different sectors' positive and negative price variances?
k <-finalData2 %>%
  group_by(sector) %>%
  summarise(across(where(is.numeric), list( var = var)))
write.csv(k,"~/Data/k.csv", row.names = TRUE)

#Question 3
## random forest algorithm
#Installing and loading nessesarry libraries
install.packages('pacman')
install.packages('tidyverse')
install.packages('tidymodels')
install.packages('leaps')
install.packages('glmnet')
install.packages('BMA')
install.packages('janitor')
install.packages("randomForest")
install.packages('effects')

library(tidyverse)
library(tidymodels)
library(leaps)
library(pacman)
library(glmnet)
library(BMA)
library(janitor)
library(caTools)
library(randomForest)

#loading the cleaned dataset

dt <- read.csv('finalData2.csv')
dt$class<- as.factor(dt$class)
dt <- subset(dt, select = -c(AnalystRating))
dim(dt)

# running random forest for the class variable
# spiting data into test and train

# Splitting data in train and test data
set.seed(321)  # Setting seed
split <- sample.split(dt, SplitRatio = 0.8)
split

train <- subset(dt, split == "TRUE")
test <- subset(dt, split == "FALSE")

# Fitting Random Forest to the train dataset
set.seed(123)  # Setting seed
classifier_RF = randomForest(class~.,
                             data = train,
                             mtry = 11,
                             ntree = 2000)

classifier_RF

# finding mtry
a=c()
i=5
set.seed(123)  # Setting seed
for (i in 3:23) {
  model3 <- randomForest(class ~ ., data = train, ntree = 100, mtry = i, importance = TRUE)
  predValid <- predict(model3, newdata = test[-23])
  a[i-2] = mean(predValid == test$class)
}
a
plot(3:23,a)

# adjusted forest
set.seed(123)  # Setting seed
classifier_RF = randomForest(class~.,
                             data = train,
                             mtry = 11,
                             ntree = 2000)

classifier_RF


# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-23])

# Confusion Matrix
mtx = table(test[,23], y_pred)
#Calculating accuracy

confusionMatrix(mtx) 

# Plotting model
plot(classifier_RF)

varImpPlot(classifier_RF)
# Importance plot
k <- importance(classifier_RF)
k

a=c()
i=5
set.seed(123)  # Setting seed
for (i in 3:23) {
  model3 <- randomForest(class ~ ., data = train, ntree = 2000, mtry = i, importance = TRUE)
  predValid <- predict(model3, newdata = test[-23])
  a[i-2] = mean(predValid == test$class)
}
a
plot(3:23,a)

##Naive Bayes Classifier
#install packages
install.packages('naivebayes')
install.packages('psych')
library(naivebayes)
library(psych)
# loading cleaned data
#loading the cleaned data set

dt <- read.csv('finalData2.csv')
dt$class<- as.factor(dt$class)
dt <- subset(dt, select = -c(AnalystRating,x))
dim(dt)
xtabs(~class+sector, data = dt)
# all the ranks are than 5 which satisfy the method needs
#create train and test data sets for training the model and testing
set.seed(1234)
ind <- sample(2, nrow(dt), replace = T, prob = c(0.8, 0.2))
train <- dt[ind == 1,]
test <- dt[ind == 2,]
#Naive Bayes Classification in R
model <- naive_bayes(class ~., data = train, usekernel = T)
model
plot(model) 
p <- predict(model, train, type = 'prob')
p1 <- predict(model, train)
tab1 <- table(p1, train$class)
confusionMatrix(tab1) 

#testing
p2 <- predict(model, test)
tab2 <- table(p2, test$class)
confusionMatrix(tab2) 

#Support Vector Machine 
##installing packages
install.packages('caret')
library(caret)
#reading the dataset
dt <- read.csv('finalData2.csv')
dt$class<- as.factor(dt$class)
dt <- subset(dt, select = -c(AnalystRating,x))
dim(dt)
# spliting data into training and testing subsets
intrain <- createDataPartition(y = dt$class, p= 0.8, list = FALSE)
training <- dt[intrain,]
testing <- dt[-intrain,]
# train control.this will control all the computational overheads
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# train() method
svm_Linear <- train(class ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)
p <-table(test_pred, testing$class)
confusionMatrix(p)
# tuning of an SVM classifier with different values of C
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(class ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

#k mean clustering
##installing packages
install.packages('factoextra')
library(factoextra)
#reading the dataset
dt <- read.csv('finalData2.csv')
dt$class<- as.factor(dt$class)
dt <- subset(dt, select = -c(AnalystRating,x))
dim(dt)
#compute k-means
df <- dt %>%
  keep(is.numeric)%>%
  scale() 
df
set.seed(123)
km.res <- kmeans(df, 2, nstart = 25)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)
aggregate(dt, by=list(cluster=km.res$cluster), mean)
fviz_cluster(km.res ,data = df)
dd <- cbind(dt, cluster = as.factor(km.res$cluster))
head(dd)
p <-table(dd$cluster,dd$class)
p
#K-NN Classifier
install.packages("class")
library(class)
#loading data
#reading the dataset
dt <- read.csv('finalData2.csv')
dt$class<- as.factor(dt$class)
dt <- subset(dt, select = -c(AnalystRating,x))
dim(dt)
#spliting the data
# Splitting data into train
# and test data
set.seed(1234)
split <- sample.split(dt, SplitRatio = 0.7)
train_cl <- subset(dt, split == "TRUE")
test_cl <- subset(dt, split == "FALSE")
# Feature Scaling
train_scale <- train_cl %>%
  keep(is.numeric)%>%
  scale() 
test_scale <- test_cl %>%
  keep(is.numeric)%>%
  scale() 
# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$class,
                      k = 1)
confusionMatrix(table(test_cl$class, classifier_knn))

#Question 4
finalData2 <- read.csv('finalData2.csv')

#Asses which model can most accurately predict price variance

#Pick the best model
control = trainControl(method="cv", number=10)
metric = "Accuracy"

# Linear Discriminant Analysis (LDA)
set.seed(138)
fit.lda = train(AnalystRating~.- x - class , data=finalData2, method="lda", metric=metric, trControl=control,na.action = na.pass)

# Classfication and Regression Trees (CART)
set.seed(138)
fit.cart = train(AnalystRating~.- x - class, data=finalData2, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (KNN)
set.seed(138)
fit.knn = train(AnalystRating~.- x - class, data=finalData2, method="knn", metric=metric, trControl=control)

# Bayesian Generalized Linear Model 
set.seed(138)
fit.logi = train(AnalystRating~.- x - class, data=finalData2, method="bayesglm", metric=metric, trControl=control)

# Support Vector Machines (SVM) 
set.seed(138)
fit.svm = train(AnalystRating~.- x - class, data=finalData2, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(138)
fit.rf = train(AnalystRating~.- x - class, data=finalData2, method="rf", metric=metric, trControl=control)


# Select Best Model
# summarize accuracy of models
results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, logi=fit.logi, svm=fit.svm, rf=fit.rf))
summary(results)

