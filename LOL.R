#League of Legend Skills

#Research Task: Find the value of the skills Kills, Deaths, Assists, and Time in winning a championship LOL game.

#Data Features: A success/failure response variable with quantitative explanatory variables

#Data
source("http://grimshawville.byu.edu/eSports2017.R")

#Create Train and Test
set.seed(12345)
sam <- sample(1:3881, 3105, replace=FALSE)
train.all <- all.players[sam,]
test.all <- all.players[-sam,]

#EDA

summary(train.all$Kills)
#A typical LOL player has a mean of 2.615 Kills, with a min of 0, max of 17, and a very right skewed distribution 

summary(train.all$Deaths)
hist(train.all$Deaths)
##A typical LOL player has a mean of 2.005 Deaths, with a interquartile range of 1 to 3, a max of 10, the shape of
#the distribution is right skewed

summary(train.all$Assists)
hist(train.all$Assists)
#The typical championship LOL player has a mean of 6.099 assists, with an interquartile range of 3 to 9, and a 
#right skewed distribution 

summary(train.all$Time)
hist(train.all$Time)
##The typical championship LOL game has a time of 35.66 minutes, with a minimum of 20.25 mins and a max of 77.95 
#mins, and the shape of the distribution is right skewed

#plots 
par(mfrow=c(2,2))
boxplot(Kills~Win, data=train.all, main="Kills")
boxplot(Deaths~Win, data=train.all, main="Deaths")
boxplot(Assists~Win, data=train.all, main="Assists")
boxplot(Time~Win, data=train.all, main="Time")
par(mfrow=c(1,1))

#Analysis

#The response variable is whether the player wins or not. The explanatory variabels are Kills, Deaths, Assists,
# and Time. 


#Model:
# ln( (P(Win=1 | Kills,Deaths,Assists,Time)) / P(Win=0 | Kills, Deaths, Assists, Time)) = beta0 + beta1*Kills + 
#  beta2*Deaths + beta3* Assists + beta4*Time

#Fit a model in R glm family binomial (Bc you win or lose)
lol.out <- glm( Win~Kills + Deaths + Assists + Time, data=train.all, family="binomial")
summary(lol.out)
#Our beta1 coefficient of 0.42 means that for each additionall kill, we estimate an increase of 0.44 in log odds of 
# winning, if everything else is held constant. 
exp(0.423284)
# For each additional kill, we estimate the odds of winning increase 1.53 times, holding all else constant
# or even better
# For each additional kill, we estimate a 53% increase in the odds of winning

#interpret change in odds
exp(coef(lol.out))[-1]

#Deaths
#For each additional death, we estimate a 56% (1-0.44) decrease in winning, holding all else constant. 


#Add Confidence Bands
#Probability of Winning, given kills, holding all other factors at median: Deaths = 2, Assists = 6, Time = 35
x.star <- data.frame(Kills=seq(0,10, length=100), Deaths = 2, Assists = 6, Time = 35)
plot(x.star$Kills, predict(lol.out, newdata=x.star, type = "response"), type = "l", xlab="# of Kills", 
     ylab="P(Win), holding all else at median", ylim=c(0,1))

#Probability of Winning, given deaths, holding all else constant
x.d.star <- data.frame(Kills = 2, Deaths=seq(0,10, length=100), Assists = 6, Time= 35)
plot(x.d.star$Deaths, predict(lol.out, newdata=x.d.star, type = "response"), type = "l", xlab="# of Deaths", 
     ylab="P(Win), holding all else at median", ylim=c(0,1))

#Probability of Winning, given assists, holding all else constant
x.a.star <- data.frame(Kills = 2, Deaths=2, Assists = seq(0,15, length=100), Time= 35)
plot(x.a.star$Assists, predict(lol.out, newdata=x.a.star, type = "response"), type = "l", xlab="# of Assists", 
     ylab="P(Win), holding all else at median", ylim=c(0,1))

#Probability of Winning, given time, holding all else constant
x.t.star <- data.frame(Kills = 2, Deaths=2, Assists = 6, Time= seq(20,80, length=500))
plot(x.t.star$Time, predict(lol.out, newdata=x.t.star, type = "response"), type = "l", xlab="Time", 
     ylab="P(Win), holding all else at median", ylim=c(0,1))

#95% CI on exp(beta)
exp(confint(lol.out))[-1,]

#For each additional Kill, we expect a 53% increase in winning, CI(46%, 63%), holding all else constant.

#For each additional Death, we expect a 56% decrease in winning, CI(52%, 60%), holding all else constant.

#For each additional Assist, we expect a 69% increase in winning, CI(62%, 76%), holding all else constant.

#For each additional Minute in the Time of the Game, we expect a 7% decrease in winning, CI(6%, 9%), holding all
# else constant. 


#Ho: AGGRESSIVE STRATEGRY HAS NO EFFECT
#Ho: TIME HAS NO EFFECT
summary(lol.out)

#Because the z test statistic for Time was -9.22, and the pvalue was less than 0.001, we conclude that an 
#aggressive strategy in shortening the game does have an effect on the outcome of the game. 

#LRT R^2
reduced.lol <- glm(Win~Kills+Deaths+Assists, data=train.all, family="binomial")
anova(reduced.lol, lol.out, test="Chisq")



#Prediction for Faker and Ambition
predict(lol.out, newdata=data.frame(Kills=2, Deaths=3, Assists=8, Time=40), type="response")
predict(lol.out, newdata=data.frame(Kills=2, Deaths=2, Assists=14, Time=40), type="response")

#Confidence Interval for Faker
logit.Faker <- predict(lol.out, newdata=data.frame(Kills=2, Deaths=3, Assists=8, Time=40), type="link",
                       se.fit=TRUE)
logit.L <- logit.Faker$fit - 1.96*logit.Faker$se.fit
logit.U <- logit.Faker$fit + 1.96*logit.Faker$se.fit
phat.L.Faker <- 1/(1+exp(-logit.L))
phat.U.Faker <- 1/(1+exp(-logit.U))
#The probability that Faker wins is 69%, with a confidence interval of (66%, 73%)


#Confidence Interval for Ambition
logit.Ambition <- predict(lol.out, newdata=data.frame(Kills=2, Deaths=2, Assists=14, Time=40), type="link",
                       se.fit=TRUE)
logit.LA <- logit.Ambition$fit - 1.96*logit.Ambition$se.fit
logit.UA <- logit.Ambition$fit + 1.96*logit.Ambition$se.fit
phat.L.Ambition <- 1/(1+exp(-logit.LA))
phat.U.Ambition <- 1/(1+exp(-logit.UA))

#The probability that Ambition wins is 99.2%, with a confidence interval of (99.8%, 99.4%)

#We can Missclassify entirely, or we can get it right in two ways. 
#Sensitivity - proportion of true positives. We predicted a win, and the player wins
#Specificity - proportion of true negatives. We predicted a loss, and the player loses. 

#ROC plots sensitivity against (1-specificity)
#each point on the curve represents a different threshold. It's a pain to compute by hand, so we'll do this.
install.packages("ROCR")
library(ROCR)
train.pred <- prediction(predict(lol.out, type="response"), train.all$Win)
train.perf <- performance(train.pred, measure="tpr", x.measure="fpr")
plot(train.perf, xlab = "1-specificity", ylab="sensitivity", main="ROC Curve")
#Add in worst case scenario line to compare.
abline(0,1, col="gray")

#let's overlay the training ROC (out of sample validation) to make sure we didn't overfit the model.
test.pred <- prediction(predict(lol.out, newdata=test.all, type="response"), test.all$Win)
test.perf <- performance(test.pred, measure="tpr", x.measure="fpr")
plot(test.perf, col="red", add=TRUE)
#the argument "add=TRUE" just causes the curve to be added to our current plot


#AUC (Area under the ROC curve) for Train data
#THe best AUC would be 1. THe worst you could get is 0.5
performance(train.pred, measure="auc")
#auc is the number in the output

#AUC for test data
performance(test.pred, measure="auc")

#if there is a big difference between the test and the train, the model might be overfit. If it is a small 
#difference, prob just random chance. We have a slight increase, so it's just random chance.


#Analysis Weakness: Everything is measured in "odds" not probability, which makes it hard to understand. 
