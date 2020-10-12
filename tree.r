## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
library(tidyverse)
library(haven)
library(knitr)
library(rgl)
library(tree) 
options(dplyr.print_min = 5)
options(tibble.print_min = 5)
opts_chunk$set(message = FALSE, cache = TRUE)
knit_hooks$set(webgl = hook_webgl)
voting <- tbl_df(read.csv("../datasets/counties.csv", header = TRUE))
subVoting <- voting %>% select(college, pop.change, income)
FGData <- read_sas("..\\datasets\\FG.sas7bdat")
FGData$outcome <- as.factor(FGData$outcome)

## ------------------------------------------------------------------------
treeFit <- tree(income ~ college, data = voting)
plot(treeFit)
text(treeFit) #first optimal split is 17.55

## ---- echo = FALSE-------------------------------------------------------
meansRSS <- function(df, response, predictor, split){
  index <- df[[predictor]] <= split
  lower <- df[[response]][index]
  higher <- df[[response]][!index]
  lowmean = mean(lower, na.rm = TRUE)
  highmean = mean(higher, na.rm = TRUE)
  RSS = sum((lower - lowmean)^2, na.rm = TRUE) + sum((higher - highmean)^2, na.rm = TRUE)
  return(list(info = data.frame(min = min(df[[predictor]]), max = max(df[[predictor]]), lowmean = lowmean, highmean = highmean, RSS = RSS, split = split, response = response, predictor = predictor), df))
}

#possible value to split on
vals <- c(5, 12, 17.55, 23)

## ---- echo = FALSE-------------------------------------------------------
#find lower and upper means
RSS1 <- meansRSS(df = voting, response = "income", predictor = "college", split = vals[1])

#plot and put on   
g <- ggplot(data = voting, aes(x = college, y = income)) + geom_point(size = 0.75, alpha = 0.75) +
  geom_segment(data = RSS1$info, aes(x = min, xend = split, y = lowmean, yend = lowmean), lwd = 2, color = "Blue") +
  geom_segment(data = RSS1$info, aes(x = split, xend = max, y = highmean, yend = highmean), lwd = 2, color = "Blue") + 
  geom_text(color = "Blue", x = 5, y = 60000, label = paste("Root RSS = ", round(sqrt(RSS1$info$RSS), 1))) + 
  geom_text(data = RSS1$info, color = "Blue", aes(x = split, label = split), y = 10000)
g

## ---- echo = FALSE-------------------------------------------------------
#find lower and upper means
RSS2 <- meansRSS(df = voting, response = "income", predictor = "college", split = vals[2])

#plot and put on   
g <- g + geom_segment(data = RSS2$info, aes(x = min, xend = split, y = lowmean, yend = lowmean), lwd = 2, color = "Red") +
  geom_segment(data = RSS2$info, aes(x = split, xend = max, y = highmean, yend = highmean), lwd = 2, color = "Red") + 
  geom_text(color = "Red", x = 5, y = 55000, label = paste("Root RSS = ", round(sqrt(RSS2$info$RSS), 1))) + 
  geom_text(data = RSS2$info, color = "Red", aes(x = split, label = split), y = 10000)
g

## ---- echo = FALSE-------------------------------------------------------
#find lower and upper means
RSS3 <- meansRSS(df = voting, response = "income", predictor = "college", split = vals[3])

#plot and put on   
g <- g + geom_segment(data = RSS3$info, aes(x = min, xend = split, y = lowmean, yend = lowmean), lwd = 2, color = "Orange") +
  geom_segment(data = RSS3$info, aes(x = split, xend = max, y = highmean, yend = highmean), lwd = 2, color = "Orange") + 
  geom_text(color = "Orange", x = 5, y = 50000, label = paste("Root RSS = ", round(sqrt(RSS3$info$RSS), 1)))+ 
  geom_text(data = RSS3$info, color = "Orange", aes(x = split, label = split), y = 10000)
g

## ---- echo = FALSE-------------------------------------------------------
#find lower and upper means
RSS4 <- meansRSS(df = voting, response = "income", predictor = "college", split = vals[4])

#plot and put on   
g + geom_segment(data = RSS4$info, aes(x = min, xend = split, y = lowmean, yend = lowmean), lwd = 2, color = "Brown") +
  geom_segment(data = RSS4$info, aes(x = split, xend = max, y = highmean, yend = highmean), lwd = 2, color = "Brown") + 
  geom_text(color = "Brown", x = 5, y = 45000, label = paste("Root RSS = ", round(sqrt(RSS4$info$RSS), 1)))+ 
  geom_text(data = RSS4$info, color = "Brown", aes(x = split, label = split), y = 10000)

## ------------------------------------------------------------------------
diamonds

## ------------------------------------------------------------------------
set.seed(90)
train <- sample(1:nrow(diamonds), size = nrow(diamonds)*0.8)
test <- dplyr::setdiff(1:nrow(diamonds), train)

diamondsTrain <- diamonds[train, ]
diamondsTest <- diamonds[test, ]

## ------------------------------------------------------------------------
treeFit <- tree(price ~ ., data = diamondsTrain)
plot(treeFit); text(treeFit) 

## ------------------------------------------------------------------------
cvTree <- cv.tree(treeFit); cvTree

## ------------------------------------------------------------------------
plot(cvTree$size ,cvTree$dev ,type="b")

## ------------------------------------------------------------------------
pred <- predict(treeFit, newdata = dplyr::select(diamondsTest, -price))

## ------------------------------------------------------------------------
sqrt(mean((pred-diamondsTest$price)^2))
