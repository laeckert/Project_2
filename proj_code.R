library(readr)
library(tidyverse)
library(caret)
library(ggplot2)


data.path <- "C:/Users/leckert/Documents/NCSU/ST558/Project_2"
day <- read_csv(paste0(data.path,"/day.csv"))
Monday <- day %>% filter(weekday==1) %>% select(-c(casual,registered, instant, dteday))
MonTib <- as_tibble(Monday)


trainIndex <- createDataPartition(Monday$cnt, p = 0.7, list = FALSE)
Monday.Train <- Monday[trainIndex, ] %>% select(-c(workingday, weekday)) %>% 
  mutate(mnth=as.factor(mnth), season=as.factor(season), weathersit = as.factor(weathersit))
Monday.Test <-  Monday[-trainIndex, ]


dmy <- dummyVars(" ~ .", data = Monday.Train, fullRank = T)
Monday.Train.trf <- data.frame(predict(dmy, newdata = Monday.Train)) %>% mutate(y = scale(cnt)) 
Monday.Train.trf.pred <- Monday.Train.trf %>% select(-c(cnt)) 
boxcoxt <- BoxCoxTrans(Monday.Train.trf$cnt)
Monday.Train.trf.resp <- predict(boxcoxt, Monday.Train.trf$cnt)

#Summarizations

a <- ggplot(Monday, aes(temp, cnt))
a + geom_jitter() +geom_smooth() +labs(title = "Bike Rental Count by Temperature", 
                                       x = "Normalized Temperature in Celcius", 
                                       y = "Count of Bike Rentals")

b <- ggplot(Monday, aes(x = season, y = cnt))
b + geom_bar(stat = "identity", aes(y=cnt, fill="Season"), colour="green") + labs(title = "Bike Rental Count by Season", x = "Season", y = "Count of Bike Rentals") + scale_fill_discrete(name = "Seasons:", 
       labels = c("Winter", "Spring", "Summer", "Fall")) 

c <- ggplot(Monday, aes(x = weathersit, y = cnt))
c + geom_bar(stat = "identity", aes(y=cnt, fill="Weather"), colour="green") + labs(title = "Bike Rental Count by Weather Type", x = "Weather Type", y = "Count of Bike Rentals") 
            + scale_fill_discrete(name = "Seasons:", labels = c("Winter", "Spring", "Summer", "Fall")) 

p <- ggplot(Monday, aes(x=season, y=cnt, color=season)) +
  geom_bar(stat="identity", fill="white") +labs(title = "Bike Rental Count by Weather Type", x = "Weather Type", y = "Count of Bike Rentals") + scale_fill_discrete(name = "Seasons:", labels = c("Winter", "Spring", "Summer", "Fall")) 
p

b <- ggplot(Monday, aes(x=season, y=cnt, color=season)) +
  geom_bar(stat="identity", fill="white") +labs(title = "Bike Rental Count by Weather Type", x = "Weather Type", y = "Count of Bike Rentals") + scale_fill_discrete(name = "Seasons:", labels = c("Winter", "Spring", "Summer", "Fall")) 
b

fitControl <- trainControl(method = "LOOCV")
model <- train(y ~., data = Monday.Train.trf, method = "ctree",
               trControl = fitControl)
print(model)

summary(lm(cnt ~ . , data=Monday.Train))

SSy <- sum(sapply(Monday.Train.trf$cnt,function(x)(x-mean(Monday.Train.trf$cnt))^2))
library(tree)
treefit <- tree(cnt ~ ., data=Monday.Train.trf)
plot(treefit)
text(treefit)
summary(treefit)
1-(deviance(treefit)/SSy)


#### Appendix
Monday.Train.contPred <- Monday.Train %>% select("temp":"windspeed")

trans <- preProcess(Monday.Train.contPred,
                    method = c("center", "scale"))
Monday.Train.contPred.transf <- predict(trans, Monday.Train.contPred)
Monday.Train$seasonF <- as.factor(Monday.Train$season)
Monday.Train$weathersitF <- as.factor(Monday.Train$weathersit)
dummys <- dummyVars(~ seasonF + weathersitF, data=Monday.Train)
Monday.Train.dummy <- predict(dummys, Monday.Train)
View(Monday.Train.dummy)
levels(Monday.Train$season)

