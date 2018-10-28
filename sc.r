#Data
getwd()
data <- read.csv("rain.csv",header = TRUE)
str(data)
#min max normalization
data$MinTemp <- (data$MinTemp - min(data$MinTemp))/(max(data$MinTemp)-min(data$MinTemp))
data$MaxTemp <- (data$MaxTemp - min(data$MaxTemp))/(max(data$MaxTemp)-min(data$MaxTemp))
data$Rainfall <- (data$Rainfall - min(data$Rainfall))/(max(data$Rainfall)-min(data$Rainfall))
data$WindGustSpeed <- (data$WindGustSpeed - min(data$WindGustSpeed))/(max(data$WindGustSpeed)-min(data$WindGustSpeed))
data$WindSpeed9am <- (data$WindSpeed9am - min(data$WindSpeed9am))/(max(data$WindSpeed9am)-min(data$WindSpeed9am))
data$WindSpeed3pm <- (data$WindSpeed3pm - min(data$WindSpeed3pm))/(max(data$WindSpeed3pm)-min(data$WindSpeed3pm))
data$Humidity9am <- (data$Humidity9am - min(data$Humidity9am))/(max(data$Humidity9am)-min(data$Humidity9am))
data$Humidity3pm <- (data$Humidity3pm - min(data$Humidity3pm))/(max(data$Humidity3pm)-min(data$Humidity3pm))
data$Pressure9am <- (data$Pressure9am - min(data$Pressure9am))/(max(data$Pressure9am)-min(data$Pressure9am))
data$Pressure3pm <- (data$Pressure3pm - min(data$Pressure3pm))/(max(data$Pressure3pm)-min(data$Pressure3pm))
data$Temp9am <- (data$Temp9am - min(data$Temp9am))/(max(data$Temp9am)-min(data$Temp9am))
data$Temp3pm <- (data$Temp3pm - min(data$Temp3pm))/(max(data$Temp3pm)-min(data$Temp3pm))
data$RISK_MM <- (data$RISK_MM - min(data$RISK_MM))/(max(data$RISK_MM)-min(data$RISK_MM))
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]
library(neuralnet)
set.seed(333)
n <- neuralnet(RainTomorrow~MinTemp+MaxTemp+Rainfall+WindGustSpeed+WindSpeed9am+WindSpeed3pm+Humidity9am+Humidity3pm+Pressure9am+Pressure3pm+Temp9am+Temp3pm+RainToday+RISK_MM,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               algorithm="rprop+")
plot(n)

# Prediction
output <- compute(n, training[,-1])
head(output$net.result)
head(training[1,])


# Confusion Matrix & Misclassification Error - training data
output <- compute(n, training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, training$RainTomorrow)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$RainTomorrow)
tab2
1-sum(diag(tab2))/sum(tab2)


