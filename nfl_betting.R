# loading data cleaning libraries
library(tidyverse)
library(Hmisc)
library(mice)
library(VIM)

# loading model libraries
library(e1071)
library(randomForest)

# setting path
path = "https://raw.githubusercontent.com/peanutshawny/nfl-sports-betting/master/data/"
spreadspoke = paste(path, "spreadspoke_scores.csv", sep = "")
teams = paste(path, "nfl_teams.csv", sep = "")
elo = paste(path, "nfl_elo.csv", sep = "")

# reading in core nfl csv, teams csv, and elo csv
nfl_df <- read.csv(spreadspoke, header = TRUE, sep = ",")
teams_df <- read.csv(teams, header = TRUE, sep = ",")
elo_df <- read.csv(elo, header = TRUE, sep = ",")

# reading in validation csv -- FOR BETTING PURPOSES ONLY
#validation_df <- read.csv("final_2019.csv", header = TRUE, sep = ",")

# dropping unnecessary features from teams_df
teams_df <- teams_df[, c(1, 3)]

# merging with nfl_df to map team ids to team names
nfl_df <- merge(nfl_df, teams_df, by.x = "team_home", by.y = "team_name")
nfl_df <- merge(nfl_df, teams_df, by.x = "team_away", by.y = "team_name")

# renaming column names
colnames(nfl_df)[c(18, 19)] <- c("home_id", "away_id")

#Deleting all rows with blank favourite ID's.
nfl_df <- subset(nfl_df, team_favorite_id != "")

# replacing all remaining blanks into NA's
nfl_df[nfl_df == ""] <- NA

# setting factors to characters so as to avoid errors
nfl_df$home_id <- as.character(nfl_df$home_id)
nfl_df$away_id <- as.character(nfl_df$away_id)

# mapping scores to favorite team instead of home/away to create win/loss variable
nfl_df <- mutate(.data = nfl_df, score_fav = if_else(home_id == team_favorite_id, 
                                                     score_home, score_away))
nfl_df <- mutate(.data = nfl_df, score_notfav = if_else(home_id == team_favorite_id, 
                                                        score_away, score_home))
# mapping new scores to fav/notfav teams
nfl_df <- mutate(.data = nfl_df, win_loss = if_else(score_fav > score_notfav, "fav", "underdog"))

# setting weather detail to character so as to avoid errors
nfl_df$weather_detail <- as.character(nfl_df$weather_detail)

# need to map out all unique values of weather detail
nfl_df <- mutate(.data = nfl_df, indoor_outdoor = if_else(startsWith(weather_detail, "DOME"),                                                           "indoors", "outdoors"))

# deleting original weather detail feature
nfl_df$weather_detail <- NULL

# filling in numbers to represent different playoff weeks
nfl_df$schedule_week <- as.character(nfl_df$schedule_week)

nfl_df$schedule_week[nfl_df$schedule_week == "18"] = "17"
nfl_df$schedule_week[nfl_df$schedule_week == "Wildcard"] <- "18"
nfl_df$schedule_week[nfl_df$schedule_week == "Division"] <- "19"
nfl_df$schedule_week[nfl_df$schedule_week == "Conference"] <- "20"
nfl_df$schedule_week[nfl_df$schedule_week == "Superbowl"] <- "21"

# changing to numeric type for modeling
nfl_df$schedule_week <- as.numeric(nfl_df$schedule_week)

# cutting out all elo data before threshold year
threshold_year <- 1984
elo_df <- subset(elo_df, season > threshold_year)

# cutting elo_df into two and grouping by team and year, taking the mean qelos of each team
elo_df1 <- elo_df[, c(2, 5, 9, 11, 21, 27)]
elo_df2 <- elo_df[, c(2, 6, 10, 12, 22, 28)]

elo_df1 <- subset(elo_df1, is.na(elo1_post) == FALSE)
elo_df2 <- subset(elo_df2, is.na(elo2_post) == FALSE)

# grouping and summarising
elo_df1 <- group_by(elo_df1, season, team1)
elo_df2 <- group_by(elo_df2, season, team2)

elo_df1 <- elo_df1 %>%
  summarise(elo_prob1 = mean(elo_prob1),
            elo1_post = mean(elo1_post),
            qbelo_prob1 = mean(qbelo_prob1),
            qbelo1_post = mean(qbelo1_post))
elo_df2 <- elo_df2 %>%
  summarise(elo_prob2 = mean(elo_prob2),
            elo2_post = mean(elo2_post),
            qbelo_prob2 = mean(qbelo_prob2),
            qbelo2_post = mean(qbelo2_post))

# changing the name of a column for ease of merging
names(elo_df2)[2] <- "team1"

# recombining the dataframes and joining onto nfl_df
elo_df <- merge(elo_df1, elo_df2, by = c("team1", "season"))

# joining onto nfl_df and mapping respective features to fav/underdog teams
elo_df <- elo_df %>%
  rowwise()%>%
  mutate(elo_prob = mean(c(elo_prob1, elo_prob2))) %>%
  mutate(elo = mean(c(elo1_post, elo2_post))) %>%
  mutate(qbelo_prob = mean(c(qbelo_prob1, qbelo_prob2))) %>%
  mutate(qbelo = mean(c(qbelo1_post, qbelo2_post))) 

# deleting unecessary columns
elo_df <- elo_df[, -c(3:10)]

# joining on only elo, all other variables were extremely similar
elo_df <- elo_df[, c(1, 2, 4)]

nfl_df <- merge(nfl_df, elo_df, by.x = c("schedule_season", "home_id"), by.y = c("season", "team1"), all = TRUE)
nfl_df <- merge(nfl_df, elo_df, by.x = c("schedule_season", "away_id"), by.y = c("season", "team1"), all = TRUE)

# changing elo column names, then taking qbelo differential and mapping onto fav/underdog teams
colnames(nfl_df)[c(23:24)] <- c("home_elo", "away_elo")

nfl_df <- nfl_df %>% mutate(fav_elo = if_else(home_id == team_favorite_id, home_elo, away_elo)) %>%
  mutate(underdog_elo = if_else(home_id == team_favorite_id, away_elo, home_elo)) %>%
  mutate(fav_elo_diff = fav_elo - underdog_elo) 

# adding the "underdog/favorite home team" feature
nfl_df <- mutate(.data = nfl_df, home_fav = if_else(home_id == team_favorite_id, "home_fav", "home_underdog"))

# deleting all rows with NA favorite ID's and anything before a certain year
nfl_df <- subset(nfl_df, team_favorite_id != "")
nfl_df <- subset(nfl_df, schedule_season > threshold_year)

# using only 13 features
nfl_df <- nfl_df[, c(1, 7, 8, 12, 13, 16, 17, 18, 21, 22, 27, 28)]

# imputing NA values using various methods from the MICE package
aggr_plot <- aggr(nfl_df, col=c('navyblue','red'), numbers = TRUE, sortVars = TRUE, 
                  labels=names(nfl_df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# because over 70% of indoor_outdoor is missing, I will need to delete that column
nfl_df$indoor_outdoor <- NULL
nfl_df$weather_humidity <- NULL

aggr_plot <- aggr(nfl_df, col=c('navyblue','red'), numbers = TRUE, sortVars = TRUE, 
                  labels=names(nfl_df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# create a mice dataset
imputed_data <- mice(data = nfl_df, m = 5, method = "pmm", maxit = 5, seed = 500)

# complete the imputation
nfl_df <- complete(imputed_data, 1)

# re-arranging and making sure all features are in the correct order in order to normalize
nfl_df <- nfl_df[, c(3, 8, 10, 1, 2, 4, 5, 7, 9)]

# optimal testing size, then splitting into training/testing
test_size <- 0.2
total_years <- max(nfl_df$schedule_season) - min(nfl_df$schedule_season)
test_years <- round(test_size * total_years)

season_mean <- mean(c(min(nfl_df$schedule_season):max(nfl_df$schedule_season)))
season_sd <- sd(c(min(nfl_df$schedule_season):max(nfl_df$schedule_season)))

cut_off <- ((max(nfl_df$schedule_season) - test_years) - season_mean)/season_sd

# characterizing and normalizing
for (i in 1:ncol(nfl_df)){
  if (i <= 3){
    nfl_df[, i] = as.character(nfl_df[, i])
  } else {
    nfl_df[, i] = (nfl_df[, i] - mean(nfl_df[, i]))/sd(nfl_df[, i])
  }
}

# converting into dummies for consistency among models
nfl_df <- nfl_df %>%
  mutate(schedule_playoff = if_else(schedule_playoff == "TRUE", 1, 0)) %>%
  mutate(win_loss = if_else(win_loss == "fav", 1, 0)) %>%
  mutate(home_fav = if_else(home_fav == "home_fav", 1, 0))

# training/testing split with most recent 6 years being in testing
training_set <- nfl_df[nfl_df$schedule_season <= cut_off, ]
testing_set <- nfl_df[nfl_df$schedule_season > cut_off, ]

# building Models, including a KNN, Randomforest, Decision Tree, Logistic Regression, and SVM

svm_model <- svm(win_loss ~., data = training_set, kernel = "radial", cost = 5)
forest_model <- randomForest(win_loss ~., data = training_set, ntree = 150 )
log_model <- glm(win_loss ~., data = training_set, family = binomial(logit))

# making Predictions and rounding

svm_predictions <- predict(svm_model, testing_set, type = "response")
svm_predictions <- round(svm_predictions)

forest_predictions <- predict(forest_model, testing_set, type = "response")
forest_predictions <- round(forest_predictions)

log_predictions <- predict(log_model, testing_set, type = "response")
log_predictions <- round(log_predictions)

# svm error
wrong <- sum(svm_predictions != testing_set$win_loss)
misclassification_rate <- wrong/nrow(testing_set)
print(misclassification_rate)

# random forest errors
wrong <- sum(forest_predictions != testing_set$win_loss)
misclassification_rate <- wrong/nrow(testing_set)
print(misclassification_rate)

# logistic regression error
wrong <- sum(log_predictions != testing_set$win_loss)
misclassification_rate <- wrong/nrow(testing_set)
print(misclassification_rate)

# building the ensemble model with all three models
pred_df <- data.frame(svm_predictions, forest_predictions, log_predictions)

# measuring correlation between model predictions
cor(pred_df)

# seems the models are approaching, but don't reach the threshold for high correlation of 0.75. 
# a max voting ensemble model will be built as it is the simplest and most easily understandable. 
# in the end, our ensemble model outperformed the logistic regression by 0.5%, a very slight improvement. 

# building ensemble predictions that performs a majority vote 
ensemble_predictions <- pred_df %>%
  rowwise() %>%
  transmute(en_pred = if_else(sum(svm_predictions, forest_predictions, log_predictions) >= 2, 
                              1, 0))

# calculating misclassification/FP/FN
wrong <- sum(ensemble_predictions != testing_set$win_loss)
misclassification_rate <- wrong/nrow(testing_set)
print(misclassification_rate)

false_positive <- sum(ensemble_predictions == 1 & testing_set$win_loss == 0)
false_negative <- sum(ensemble_predictions == 0 & testing_set$win_loss == 1)

false_positive_rate <- false_positive/nrow(testing_set)
false_negative_rate <- false_negative/nrow(testing_set)

print(false_positive_rate)
print(false_negative_rate)

# writing validation predictions onto csv
#write.csv(ensemble_predictions, 
#          "C:/Users/LWW-Dell/Documents/School/HBA2/Data Science/project/nov25predictions.csv")
```
