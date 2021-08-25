library(dplyr)
div<-read.csv("div.oct.17.csv")
diversity<-read.csv("div.oct.17.csv")
mydata<-select(div,divrespectp,impuclaclimate,religion)

mydata<-mydata %>% filter(religion != "Other")

t<-table(mydata$religion)

#cleaning
a<-mydata %>% filter(religion == "Christian")
b<-mydata %>% filter(religion == "Spiritual but not associated with a major religion")
c<-mydata %>% filter(religion == "Not particularly spiritual")
main_reg<-rbind(a,b)
sec_reg<-rbind(a,c)
new1<-rbind(main_reg,c)

par(mfrow=c(1,2))
boxplot(main_reg$divrespectp~main_reg$religion,
        cex.axis=0.5,xlab = "religion", ylab = "divrespectp", main = "Uncleaned", col = "lightblue") #we can see that there are some outliers
boxplot(main_reg$impuclaclimate~main_reg$religion,
        xlab = "religion", cex.axis = 0.5, ylab = "impuclaclimate", col = "lightcoral")

out1<-boxplot(main_reg$divrespectp~main_reg$religion, plot=FALSE)$out
main_reg<-main_reg[-which(main_reg$divrespectp %in% out1),]
boxplot(main_reg$divrespectp~main_reg$religion, main = "Cleaned", xlab ="religion",col = "lightblue")
#remove outliers

par(mfrow=c(1,2))
boxplot(sec_reg$divrespectp~sec_reg$religion,
        cex.axis=0.5,xlab = "religion", ylab = "divrespectp", col = "lightblue",main = "Uncleaned" )#we can see that there are some outliers
boxplot(sec_reg$impuclaclimate~sec_reg$religion,
        xlab ="religion", cex.axis = 0.5, ylab = "impuclaclimate", col = "lightcoral")

out2<-boxplot(sec_reg$divrespectp~sec_reg$religion, plot=FALSE)$out
sec_reg<-sec_reg[-which(sec_reg$divrespectp %in% out2),]
boxplot(sec_reg$divrespectp~sec_reg$religion, main = "Cleaned", xlab ="religion", col = "lightblue")

#unclean data distribution
par(mfrow=c(1,2))
divr<-table(mydata$divrespectp,mydata$religion)
hist(divr,main="Uncleaned freq dist. of divrespectp" ,
     xlab="religion and respect score", col = "lightcoral")
div_imp<-table(mydata$impuclaclimate,mydata$religion)
hist(div_imp,main="Uncleaned freq dist. of impuclaclim",
     xlab="religion and improving ucla climate score", col = "lightblue")

#clean data distribution
par(mfrow=c(1,2))

tt22<-table(new1$divrespectp,new1$religion)
hist(tt22,main ="Cleaned freq dist. of divrespectp",
     xlab="religion and respect score", col = "lightcoral")
tt33<-table(new1$impuclaclimate,new1$religion)
hist(tt33,main="Cleaned freq dist. of impuclaclim",
     xlab="religion and improving ucla climate score", col = "lightblue")

df25 <-cbind(diversity$divrespectp, diversity$impuclaclimate)

cor(df25)
plot(diversity$divrespectp, diversity$impuclaclimate, col = "lightgreen")

#Figure 1: Uncleaned Boxplot for our variable "divrespectp" using Christian and Spiritual 
#Figure 2: Boxplot for our variable "impuclaclimate" using Christian and Spiritual
#Figure 3: Cleaned Boxplot for our variable "divrespectp" using Christian and Spiritual 
#Figure 4: Uncleaned Boxplot for our variable "divrespectp" using Christian and Non-Spiritual
#Figure 5: Boxplot for our variable "impuclaclimate" using Christian and Non-Spiritual
#Figure 6: Uncleaned Distribution for the variable "divrespectp" 
#Figure 7: Uncleaned Distribution for the variable "impuclaclimate" 
#Figure 8: Cleaned Distribution for the variable "divrespectp" 
#Figure 9: Cleaned Distribution for the variable "impuclaclimate" 
#Figure 10: Correlation Matrix of Predictor Variables
#Figure 11: Scatterplot of Predictor Variables



na_id <- which(is.na(diversity$uclaclimate))
#na_id

diversity <- diversity[-na_id, ]
#dim(diversity)

table(diversity$uclaclimate)
# Figure: Table of "uclaclimate" before cleaning the data 

diversity$uclaclimate <- as.factor(diversity$uclaclimate)
levels(diversity$uclaclimate)[5] <- c("Uncomfortable")
table(diversity$uclaclimate)
# Figure: Table of "uclaclimate" after cleaning the data 
prop.table(table(diversity$uclaclimate))

ucla_levels <- c("Uncomfortable", "Somewhat comfortable", 
                 "Comfortable", "Very comfortable")

diversity$uclaclimate <- factor(diversity$uclaclimate, levels = ucla_levels, 
                                ordered = TRUE)
# Figure: Proportion table of "ucla climate" after cleaning the data"

barplot(prop.table(table(diversity$uclaclimate)), col = "lightcoral", cex.names = 0.75, 
        ylim = c(0, 0.5), main = "Proportion of UCLA Climate")
# Figure: Barplot of "ucla climate" after cleaning the data" 

hist(diversity$divrespectp, freq = FALSE, col = "lightblue", ylim = c(0, 0.025), 
     xlab = "Score Range", main = "Histogram of Feeling of UCLA Score")

abline(v = c(median(diversity$divrespectp), mean(diversity$divrespectp)), 
       lty = c(3, 2), col = c("blue", "red"))

legend("topleft", c("Median Score", "Mean Score"), col = c("blue", "red"), 
       lty = c(3, 2), inset = 0.05)

boxplot(diversity$divrespectp ~ diversity$uclaclimate, col = "lightcoral")
# Figure: Boxplot of "divrespectp" vs "diversity$uclaclimate" 

plot(diversity$divrespectp, diversity$ucladiscp, xlab = "Score of experienced discrimination", 
     ylab = "Score of feeling about UCLA", col = "red",
     main = "Score of feeling about UCLA and experienced discrimination")
# Figure: Scatterplot of variables "divrespectp" and "ucladiscp"

plot(diversity$divrespectp, diversity$uclaexclusionaryp, xlab = "Score of observed exclusion", 
     ylab = "Score of feeling about UCLA", col = "green",
     main = "Score of feeling about UCLA and observed exclusion")
# Figure: Scatterplot of variables "divrespectp" and "uclaexclusionaryp"

df3 <- cbind(diversity$ucladiscp, diversity$uclaexclusionaryp)
cor(df3)
# Figure: Correlation matrix of numerical variables "ucladiscp" and "uclaexclusionaryp" 

plot(diversity$ucladiscp, diversity$uclaexclusionaryp, col = "yellow")
# Figure: Scatterplot of numerical variables "ucladiscp" and "uclaexclusionaryp" 
# Figure: Histogram of "divrespectp"



#model:
religion1<- rep(NA,nrow(main_reg)) #new col for religion and change to num in order to fit in glm
main_reg<- cbind(main_reg,religion1)

main_reg$religion1[main_reg$religion == "Christian"]<-0
main_reg$religion1[main_reg$religion == "Spiritual but not associated with a major religion"]<-1
# class(main_reg$religion1)
lr <- glm(main_reg$religion1~main_reg$divrespectp+main_reg$impuclaclimate,data=main_reg)
round(summary(lr)$coef, 4)
#Figure: Table of Coefficients and their corresponding p-values 

christian_and_spirtual <- exp(coef(lr))#odd ratios
christian_and_spirtual
# Figure: Table of log-odds 

library(car)
vif(lr)
# Figure: VIF table for Model 1

infl2 <- influencePlot(lr)
infl2
# Figure: Influence Plot and Leverage Values 



nnn<-nrow(main_reg)
#AIC:
lr.sse0 <- sum(resid(lr) ^2)
b <- nnn + nnn*log(2*pi) + nnn * log(lr.sse0 / nnn) + 2 * (1+1)
a <- AIC(lr,k=2)

v <- nnn + nnn * log(2*pi) + nnn*log(lr.sse0/nnn) + log(nnn)*(1+1)
d <-  AIC(lr,k=log(nnn))

add1(glm(main_reg$religion1~main_reg$divrespectp),main_reg$religion1~main_reg$divrespectp+main_reg$impuclaclimate,test="F")
add1(glm(main_reg$religion1~main_reg$impuclaclimate),main_reg$religion1~main_reg$divrespectp+main_reg$impuclaclimate,test="F")
#Figure: AIC model and their respective p-values 

religion2<- rep(NA,nrow(sec_reg)) #new col for religion and change to num in order to fit in glm
sec_reg<- cbind(sec_reg,religion2)

sec_reg$religion2[sec_reg$religion == "Christian"]<-0
sec_reg$religion2[sec_reg$religion == "Not particularly spiritual"]<-1
lr1 <- glm(sec_reg$religion2~sec_reg$divrespectp+sec_reg$impuclaclimate,data=sec_reg)
summary(lr1)$coef
#table for odd ratios
christian_and_nonspirtual <-exp(coef(lr1))#odd ratios
christian_and_nonspirtual

# VIF 
library(car)
vif(lr1)
# Figure: VIF table for Model 2

infl1 <- influencePlot(lr1)
infl1
# Figure: Influence Plot and Leverage Values 

#AIC:
lr1.sse0 <- sum(resid(lr1) ^2)
b1 <- nnn + nnn*log(2*pi) + nnn * log(lr1.sse0 / nnn) + 2 * (1+1)
c1 <- AIC(lr,k=2)

d1 <- nnn + nnn * log(2*pi) + nnn*log(lr1.sse0/nnn) + log(nnn)*(1+1)
a1 <- AIC(lr1,k=log(nnn))

add1(glm(sec_reg$religion2~sec_reg$divrespectp),sec_reg$religion2~sec_reg$divrespectp+sec_reg$impuclaclimate,test="F")
add1(glm(sec_reg$religion2~sec_reg$impuclaclimate),sec_reg$religion2~sec_reg$divrespectp+sec_reg$impuclaclimate,test="F")#agree with our model

diversity$uclaclimate <- as.character(diversity$uclaclimate)
lm <- lm(divrespectp ~ ucladiscp + uclaexclusionaryp + uclaclimate, data = diversity)
summary(lm)
plot(lm)

library(car)
vif(lm)

infl <- influencePlot(lm)
infl


library(leaps)

best_subset <- regsubsets(divrespectp ~ ucladiscp + uclaexclusionaryp + uclaclimate, 
                          data = diversity, nbest = 1, nvmax = 6,
                          intercept = TRUE, method = "exhaustive",
                          really.big = FALSE)
sumBS <- summary(best_subset)
sumBS

forward_sel <- regsubsets(divrespectp ~ ucladiscp + uclaexclusionaryp + uclaclimate, 
                          data = diversity, nbest = 1, nvmax = 6,
                          intercept = TRUE, method = "forward",
                          really.big = FALSE)
sumF <- summary(forward_sel)
sumF

backward_sel <- regsubsets(divrespectp ~ ucladiscp + uclaexclusionaryp + uclaclimate, 
                           data = diversity, nbest = 1, nvmax = 6,
                           intercept = TRUE, method = "backward",
                           really.big = FALSE)
sumB <- summary(backward_sel)
sumB

par(mfrow = c(1, 2))

plot(sumBS$rsq, xlab = "Number of predictors", ylab = "RSq",
     type = "l", col = "blue", main = 'R^2 of  Best Subset Selection')
lines(sumF$rsq, col = "red")
lines(sumB$rsq, col = "green")

plot(sumBS$rss, xlab = "Number of predictors", ylab = "RSS",
     type = "l", col = "blue", main = 'RSS of Best Subset Selection')
lines(sumF$rss, col = "red")
lines(sumB$rss, col = "green")