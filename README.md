# nfl-sports-betting
Model created to predict outcomes of nfl games with the goal of making bets to win money!
#### -- Project Status: [Active]

## Project Intro/Objective
The purpose of this project is to be able to accurately predict the outcome of NFL matches using various algorithms and interesting features. Me and my group of friends collectively contributed $200 (with an extra $100 donated by my professor) to bet on the model's predictions. The stakes are high!

### Methods Used
* Exploratory Analysis
* SVM
* Random Forest
* Logistic Regression
* Ensemble Methods

### Technologies/Languages
* R Notebook

## Project Description
Being extremely interested in football, I decided to try and predict the outcome of NFL games using data I found on Kaggle, score spreads from sports betting websites, ELO predictions from fivethirtyeight, and some of my own features, to try and see if I can make accurate predictions this season and (hopefully) make some money in the process. In the NFL the home team wins 57% of the time. This can serve as a perfect benchmark when predicting the results of games.

Sports betting in the NFL comprises of three main betting methods: spread betting, moneyline, and over/under. 

### Spread Betting
Like many high scoring sports, NFL wagering is dictated by the point spread. The spread, or line, is a type of side bet that equalizes the chance of winning a wager. The line offered for any given team will be accompanied with a – or + symbol to indicate whether a team is a favorite or an underdog. Although not the focus for this project, the spread could be a useful feature in determining which team the "house" thinks will most likely win the game.

### Over/Under
The concept of betting on totals, or Over/Under, is quite simplistic. The bettor wagers on whether the TOTAL points scored in a game will fall over or under a pre-determined number set by the sportsbook. Like the spread, the Over/Under prediction can also be a predictor of the outcome of a game, since if a game is low-scoring there should be therefore less chances to score, throwing in an element of luck into the game.

### Moneyline
In the parity driven NFL, the phrase “any given Sunday” is more prevalent than ever, which is why Moneyline bets can be very profitable when betting on the underdog. A Moneyline wager is one that is placed on either the favorite or the underdog to win straight up. Optimizing Moneyline bets will be the focus of this project.

## Description of my Current Process
Here's a breakdown of the steps I took and the conclusion I arrived at:

First, I found three different datasets online that will make up the features that I will be testing. They are: 

### 1) Spreadspoke 
Betting websites like www.spreadspoke.com not only release point spread predictions of all weekly games, but also release datasets online of all the NFL games played since 1968 along with their respective features. This data will be the main dataset that I will be cleaning and modeling on.

### 2) Teams
Since the spreadspoke data does not contain the team ID of all teams, a separate dataset of all teams along with their respective short-hands and ID's were found on www.kaggle.com. This dataset will serve as a useful mapping tool for team ID's to team names in case I would want to manipulate home/away team into favourite/underdog team (spoilers, I will!).

### 3) Fivethirtyeight ELO 
Www.fivethirtyeight.com is an online sports blog that develops their own ELO rating of team for each season on a rolling basis. I wanted to use some of their features to see whether or not they added to my model's predictive power.

## Conclusion & Next Steps

## Contact

Feel free to email me at shawnliu30@gmail.com if you want to talk about this project or data science in general!
