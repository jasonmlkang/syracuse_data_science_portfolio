# Required libraries
library(sqldf)

# Load data from my local computer
# Data can be downloaded from Kaggle.com (search for "NBA Championship Data")
nba.champions <- read.csv("/Users/kang/Documents/syracuse_datascience/IST_687_applied_data_science/project/Nba_Players_Stats/championsdata.csv")

# First inspect NBA Championship Data
str(nba.champions)
## 'data.frame':	220 obs. of  24 variables:
## $ Year: int  1980 1980 1980 1980 1980 1980 1981 1981 1981 1981 ...
## $ Team: Factor w/ 13 levels "'Heat'","Bulls",..: 6 6 6 6 6 6 4 4 4 4 ...
## $ Game: int  1 2 3 4 5 6 1 2 3 4 ...
## $ Win : int  1 0 1 0 1 1 1 0 1 0 ...
## $ Home: int  1 1 0 0 1 0 1 1 0 0 ...
## $ MP  : int  240 240 240 240 240 240 240 240 240 240 ...
## $ FG  : int  48 48 44 44 41 45 41 41 40 35 ...
## $ FGA : int  89 95 92 93 91 92 95 82 89 74 ...
## $ FGP : num  0.539 0.505 0.478 0.473 0.451 0.489 0.432 0.5 0.449 0.473 ...
## $ TP  : int  0 0 0 0 0 0 0 0 2 0 ...
## $ TPA : int  0 1 1 0 0 2 1 3 3 3 ...
## $ TPP : num  NA 0 0 NA NA 0 0 0 0.667 0 ...
## $ FT  : int  13 8 23 14 26 33 16 8 12 16 ...
## $ FTA : int  15 12 30 19 33 35 20 13 19 24 ...
## $ FTP : num  0.867 0.667 0.767 0.737 0.788 0.943 0.8 0.615 0.632 0.667 ...
## $ ORB : int  12 15 22 18 19 17 25 14 16 17 ...
## $ DRB : int  31 37 34 31 37 35 29 34 28 30 ...
## $ TRB : int  43 52 56 49 56 52 54 48 44 47 ...
## $ AST : int  30 32 20 23 28 27 23 17 24 22 ...
## $ STL : int  5 12 5 12 7 14 6 6 12 5 ...
## $ BLK : int  9 7 5 6 6 4 5 7 6 6 ...
## $ TOV : int  17 26 20 19 21 17 19 22 11 22 ...
## $ PF  : int  24 27 25 22 27 22 21 27 25 22 ...
## $ PTS : int  109 104 111 102 108 123 98 90 94 86 ...

# Remove all variables except Year and Team
nba.champions <- nba.champions[, 1:2]

# Inspect NBA championship Data again
str(nba.champions)
## 'data.frame':	220 obs. of  2 variables:
## $ Year: int  1980 1980 1980 1980 1980 1980 1981 1981 1981 1981 ...
## $ Team: Factor w/ 13 levels "'Heat'","Bulls",..: 6 6 6 6 6 6 4 4 4 4 ...

head(nba.champions)
## Year   Team
## 1 1980 Lakers
## 2 1980 Lakers
## 3 1980 Lakers
## 4 1980 Lakers
## 5 1980 Lakers
## 6 1980 Lakers

tail(nba.champions)
## Year      Team
## 215 2017 Warriorrs
## 216 2017  Warriors
## 217 2018  Warriors
## 218 2018  Warriors
## 219 2018  Warriors
## 220 2018  Warriors

# Get unique Year and Team
nba.champions <- sqldf("SELECT Year, Team FROM 'nba.champions' GROUP BY Year, Team")

head(nba.champions)
## Year    Team
## 1 1980  Lakers
## 2 1981 Celtics
## 3 1982  Lakers
## 4 1983  Sixers
## 5 1984 Celtics
## 6 1985  Lakers

tail(nba.champions)
## Year      Team
## 36 2014     Spurs
## 37 2015  Warriors
## 38 2016 Cavaliers
## 39 2017 Warriorrs
## 40 2017  Warriors
## 41 2018  Warriors

length(unique(nba.champions$Year))
## [1] 39
