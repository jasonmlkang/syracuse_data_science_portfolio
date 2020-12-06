# Data is loaded from my local machine. It can be downloaded from
# https://drive.google.com/open?id=1ggH2Jt_2qdeyTWUdRCI4DuHdEwQH6d9X

# Data Munging
nba.team_stats <- read.csv("/Users/kang/Documents/syracuse_datascience/IST_687_applied_data_science/project/Nba_Players_Stats/team-stats.csv")

colnames(nba.team_stats)[which(names(nba.team_stats) == "FG.")] <- "FGPer"
colnames(nba.team_stats)[which(names(nba.team_stats) == "X3P")] <- "3P"
colnames(nba.team_stats)[which(names(nba.team_stats) == "X3PA")] <- "3PA"
colnames(nba.team_stats)[which(names(nba.team_stats) == "X3P.")] <- "3PPer"
colnames(nba.team_stats)[which(names(nba.team_stats) == "X2P")] <- "2P"
colnames(nba.team_stats)[which(names(nba.team_stats) == "X2PA")] <- "2PA"
colnames(nba.team_stats)[which(names(nba.team_stats) == "X2P.")] <- "2PPer"
colnames(nba.team_stats)[which(names(nba.team_stats) == "FT.")] <- "FTPer"
colnames(nba.team_stats)[which(names(nba.team_stats) == "finaly.16")] <- "final16"
colnames(nba.team_stats)[which(names(nba.team_stats) == "final.8")] <- "final8"
colnames(nba.team_stats)[which(names(nba.team_stats) == "final.4")] <- "final4"
colnames(nba.team_stats)[which(names(nba.team_stats) == "final.2")] <- "final2"

str(nba.team_stats)
head(nba.team_stats[,c(1,2:4, 20:25)])
head(nba.team_stats[,c(1,5:13)])
head(nba.team_stats[,c(1,14:16)])
head(nba.team_stats[,c(1,17:19)])
head(nba.team_stats[,c(1,26:31)])

# Describtive Statistics
library(ggplot2)
ggplot(nba.team_stats) + 
  geom_point(aes(x=AST, y=win, color=final16)) + 
  labs(title="Assists vs Playoff Brackets 16 and Wins")
ggplot(nba.team_stats) + 
  geom_point(aes(x=STL, y=win, color=final16)) + 
  labs(title="Steals vs Playoff Brackets 16 and Wins")
ggplot(nba.team_stats) + 
  geom_point(aes(x=BLK, y=win, color=final16)) + 
  labs(title="Blocks vs Playoff Brackets 16 and Wins")
ggplot(nba.team_stats) + 
  geom_point(aes(x=FGPer, y=win, color=final16)) + 
  labs(title="Field Goals Percentages vs Playoff Brackets 16 and Wins")
ggplot(nba.team_stats) + 
  geom_point(aes(x=FTPer, y=win, color=final16)) + 
  labs(title="Free Throw Percentages vs Playoff Brackets 16 and Wins")

# Modeling Technique
# Create Test Dataset
randIndex <- sample(1:dim(nba.team_stats)[1])
cutePoint2_3 <- floor(2 * dim(nba.team_stats)[1]/3)
nba.trainData <- nba.team_stats[randIndex[1:cutePoint2_3],]
nba.testData <- nba.team_stats[randIndex[(cutePoint2_3+1):dim(nba.team_stats)[1]],]

# Predict Wins
wins_lm <- lm(data=nba.trainData, formula=win ~ FGPer)
nba.testData$predicted_win <- predict(wins_lm, nba.testData, type="response")
nba.testData$prediction_error <- nba.testData$win - nba.testData$predicted_win
sd(nba.testData$prediction_error) #=> 9.590283
ggplot(nba.testData) + 
  geom_point(aes(x=FGPer, y=win, color=prediction_error)) + 
  labs(title="Field Goal Percentages vs Wins Prediction Error")

# Predict Playoff Brackets 16
final16_ksvm <- ksvm(data=nba.trainData, formula=~ Ozone, data=aq_trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
aq_testData$ksvm_predict <- predict(model, data.frame(Ozone=aq_testData$Ozone), type="votes")[2,]
aq_testData$ksvm_predict <- factor(aq_testData$ksvm_predict)
ksvm_plot <- ggplot(aq_testData) + geom_point(aes(x=Temp, y=Wind, shape=ksvm_predict, color=goodOzone))
ksvm_plot
