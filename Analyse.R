# setwd("/Users/mikadirkes/Github-Projekt/")

titanic <- read.csv("processed_titanic.csv")

source("Funktionen-R-Skript 1.R")
source("Funktionen-R-Skript 2.R")

# Zu untersuchende Statistiken und deren Typen: 

# Survived - dichotom
# Pclass   - kategorial
# Sex      - dichotom
# Age      - metrisch
# Sibsp    - metrisch
# Parch    - metrisch
# Fare     - metrisch
# Embarked - kategorial
# Title    - kategorial
# Deck     - kategorial
# Side     - dichotom


bivariate_stats_md(titanic$Fare, titanic$Survived)
# Man sieht, dass es einen signifikanten Unterschied im erwarteten Ticketpreis
# zwischen denen, die überlebt haben, und denen, die nicht überlebt haben, gibt.
# Der Ticketpreis von denen, die überlebt haben, war durchschnittlich mehr als
# doppelt so teuer wie von den Verstorbenen. Es besteht allerdings auch eine 
# deutlich größere Varianz in den Ticketpreisen der Überlebenden, es gab also,
# wie man gut dem Boxplot entnehmen kann, auch viele Überlebende mit günstigen
# Boardkarten. 

#...
