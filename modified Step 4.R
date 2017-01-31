data1<- data_rand[,-c(8)]

data_train <- data1[1:570, ]
data_test  <- data1[571:636, ]

# check the data sets  
prop.table(table(data_train$tempCat))
prop.table(table(data_test$tempCat))

# Modeling with CART 
data_model_cart<-rpart(tempCat~agrland+forestarea+electricityconsum+co2emission+
                         poptot+urbanpop+temperature,
                       method="class",data=data_train)

printcp(data_model_cart) # display the results 
plotcp(data_model_cart) # visualize cross-validation results 
summary(data_model_cart) # detailed summary of splits

# plot tree 
plot(data_model_cart, uniform=TRUE, 
     main="Classification Tree for Climate Data")
text(data_model_cart, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(data_model_cart)
#Testing the model using test data set
data_predict_cart <- predict(data_model_cart,data_test)
# Using C5.0 for modelling
#Training the model
x<-data_train[,c(2,3,4,5,6,7,8)]
y<-data_train$tempCat
y<-as.factor(y)
install.packages("C50")
library(C50)
data_model_c50 <- C5.0(x,y)
data_model_c50
summary(data_model_c50)
# From the summary of the summary of the C50 model we can see that temparture doesnot 
#have any contribution variable to how other factors also influence the temperature
data_model_c50_01<- C5.0(data_train[,c(2,3,4,6,7)],as.factor(data_train$tempCat))
summary(data_model_c50_01)

# Testing the model using Test data set
data_predict_c50 <- predict(data_model_c50,data_test)
# Validating the accuracy of the model using chi-square test for propotions
install.packages("gmodels")
library(gmodels)
CrossTable(data_test$tempCat, data_predict_c50,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))

