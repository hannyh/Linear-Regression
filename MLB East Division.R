#Incorporating Categorical Variables into our Model

#Data: 
#Accessed at http://www.espn.com/mlb/standings/_/ on March 2, 2018
library(XML)

url1 <-"http://www.espn.com/mlb/standings/_/season/2017"
mlb.webpage <- htmlParse(url1)
mlb <- readHTMLTable(mlb.webpage, header=FALSE, which=1, skip.rows = c(1:21, 27, 33), 
                     colClasses = rep("numeric", 11))
mlb <- mlb[ , c(1,9)]
colnames(mlb) <- c("Wins", "Run Differential")
mlb$Division <- c(rep("East",5), rep("Central",5), rep("West", 5))

head(mlb)

#EDA
#Summary Statistics for Wins
tapply(mlb$Wins, mlb$Division, mean)
tapply(mlb$Wins, mlb$Division, sd)
#Summary Statistics for Run Differential
tapply(mlb$`Run Differential`, mlb$Division, mean)
tapply(mlb$`Run Differential`, mlb$Division, sd)
#Scatterplot of RunDiff~Wins with each division in it's own colors
plot(mlb$`Run Differential`, mlb$Wins, xlab= "Run Differential", ylab= "Wins")
points(mlb$`Run Differential`[mlb$Division=="East"], mlb$Wins[mlb$Division=="East"], col="blue2", pch=19)
points(mlb$`Run Differential`[mlb$Division=="Central"], mlb$Wins[mlb$Division=="Central"], col="red4", pch=19)
points(mlb$`Run Differential`[mlb$Division=="West"], mlb$Wins[mlb$Division=="West"], col="lightgreen", pch=19)
legend("topleft", legend=c("East", "Central", "West"), col=c("blue2", "red4", "lightgreen"), pch=c(rep(19,3)))

#Model:
#R won't handle the categorical type. Change Division to a factor. 
mlb$Division <- as.factor(mlb$Division)

#Fit Model like normal, and see what happens
mlb.out <- lm(Wins~Division +`Run Differential`, data = mlb, x= TRUE, y = TRUE)
summary(mlb.out)
#There isn't a column for the central division because it is represented when the east and west divisions both 
#have a zero in their columns. 

# The estimated regression coefficient for this model means that if you hold the division constant, for every
# run a team earns or every run a team prevents their opponent from getting, the team will win an average of 0.082
# more games. 

#beta0 = y intercept = mean wins for all x's equal to 0.
#We want to specify the "comparison case" by declaring the factor level that is omitted from x matrix

#To specify the comparison case (y-intercept as East)
mlb$Divison <- relevel(mlb$Division, "East")

#New Model
mlb.out2 <- lm(Wins ~Division + `Run Differential`, data= mlb, x=TRUE, y=TRUE)
 
#Really there are 3 levels of divison, and therefore, 3 models based on which factor we're using. That makes it 
# a little more complicated than if we didn't have a categorical variable in our data. 

#If East: Y = beta0 + beta1*0 + beta2*0 + beta3* RunDiff 
#Simplifies to = (Beta0) + beta3 *RunDiff

#If Central: Beta0 + Beta1 * 1 + Beta2*0 + Beta3*RunDiff
#Simplifies: = (Beta0 + Beta1) + Beta3*RunDiff

#If West: Y = Beta0 + Beta1*0 + Beta2*1 + Beta3*RunDiff
#Simplifies: = (Beta0 + Beta2) + Beta3 * RunDiff

#If you were to plot the East and Central models, they would have the same slope, just different y intercepts. The difference 
#would be beta1. Therefore beta1 = the mean diff between central and East for all RunDiff. 

#If you were to plot East and West, they also have same slope, but diff y intercepts. The difference of the 
#yintercepts is Beta2. Therefore Beta2 = The mean diff between west and east for all RunDiff.
summary(mlb.out2)


 
#Model for wins.hat:

 #If East, wins.hat = 81.1 + 0.08*Run.Diff
 #If Central, wins.hat = (81.1 + (-0.16)) + 0.08*Run.Diff
 #IF West, wins.hat = (81.1 + 0.13) + 0.08*Run.Diff
 
#Graphic showing predicted model (3 lines)
 #existing scatterplot
 plot(mlb$`Run Differential`, mlb$Wins, xlab= "Run Differential", ylab= "Wins")
 points(mlb$`Run Differential`[mlb$Division=="East"], mlb$Wins[mlb$Division=="East"], col="blue2", pch=19)
 points(mlb$`Run Differential`[mlb$Division=="Central"], mlb$Wins[mlb$Division=="Central"], col="red4", pch=19)
 points(mlb$`Run Differential`[mlb$Division=="West"], mlb$Wins[mlb$Division=="West"], col="lightgreen", pch=19)
 legend("topleft", legend=c("East", "Central", "West"), col=c("blue2", "red4", "lightgreen"), pch=c(rep(19,3)))
 
 #East model
 abline(81.096918, 0.082029, col="blue2")
 #Central model
 abline(81.096918-0.168041,0.082029, col= "red4")
 #West 
 abline(81.096918+0.306306,0.082029, col= "lightgreen")
 
 #Test this hypothesis
 #H0: no difference between divisions after adjusting for run.diff
 
 #Reduced Model:
 reduce.mlb2 <- lm(Wins ~ `Run Differential`, data=mlb)
 
 #By not including Division in the model, we are telling R that Division doesn't matter
 anova(reduce.mlb2, mlb.out2)
 #The test statistic from the anova test is 0.9919.
 
 #We can conclude that there is no statistically significant difference between the divisions in paths to the 
 #playoffs
 
 
 
 