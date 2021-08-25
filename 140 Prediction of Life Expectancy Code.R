life <- read.csv("Life Expectancy Data.csv")
library(caret)
library(GGally)
library(randomForest)
library(dplyr)

#data cleaning
life <- na.omit(life)
developing_country <- life %>% filter(Status == 'Developing') %>% select(-Country,-Year,-Status)
developed_country <- life %>% filter(Status == 'Developed') %>% select(-Country,-Year,-Status)

#correlation analysis
ggcorr(life[, -c(1,2,3,12,17,19)], nbreaks = 6, label = TRUE, label_size = 3,
       hjust = 0.9, size = 3.5, layout.exp = 3, label_alpha = TRUE)

#DEVELOPING COUNTRIES - random forest model building
set.seed(1)
bag.model.ing <- randomForest(Life.expectancy ~ ., data = developing_country, mtry = 18,importance = TRUE)
pred.ing <- predict(bag.model.ing, newdata = developing_country)
mean((pred.ing - developing_country$Life.expectancy)^2)
varImpPlot(bag.model.ing, type = 1, scale = F, pch = 19,color = "gray21",lcolor = "tomato3", main = "Factors for Developing Countries")

#DEVELOPED COUNTRIES - random forest model building
set.seed(1)
bag.model.ed <- randomForest(Life.expectancy ~ ., data = developed_country, mtry = 18,importance = TRUE)
pred.developed <- predict(bag.model.ed, newdata = developed_country)
mean((pred.developed - developed_country$Life.expectancy)^2)
varImpPlot(bag.model.ed, type = 1, scale = F, pch = 19,color = "gray21",lcolor = "cornflowerblue",main = "Factors for Developed Countries")