# setwd("/Users/mikadirkes/Github-Projekt/")

titanic <- read.csv("processed_titanic.csv")

source("Funktionen-R-Skript 1.R")
source("Funktionen-R-Skript 2.R")

# Zu untersuchende Statistiken und deren Typen: 

# Survived - dichotom
# Pclass   - kategorial
# Sex      - dichotom
# Age      - metrisch
# SibSp    - metrisch
# Parch    - metrisch
# Fare     - metrisch
# Embarked - kategorial
# Title    - kategorial
# Deck     - kategorial
# Side     - dichotom

# Um den Datensatz zu analysieren, werden die von uns erstellten Funktionen auf
# die jeweils passenden Variablen angewendet. Zunächst untersuchen wir, welche
# metrischen Variablen Einfluss auf die Überlebensrate ausgeübt haben könnten:

bivariate_stats_md(titanic$Fare, titanic$Survived)
# Man sieht, dass es einen signifikanten Unterschied im erwarteten Ticketpreis
# zwischen denen, die überlebt haben, und denen, die nicht überlebt haben, gibt.
# Der Ticketpreis von denen, die überlebt haben, war durchschnittlich mehr als
# doppelt so teuer wie von den Verstorbenen. Es besteht allerdings auch eine 
# deutlich größere Varianz in den Ticketpreisen der Überlebenden, es gab also,
# wie man gut dem Boxplot entnehmen kann, auch viele Überlebende mit günstigen
# Boardkarten. Dennoch kann gefolgert werden, dass ein teureres Ticket eine 
# höhere Überlebenschance erkauft hat. 

bivariate_stats_md(titanic$Age, titanic$Survived)
# Vom Alter aus ist allerdings kein signifikanter Einfluss auf die mittleren
# Überlebensraten festzustellen. Zwar ist die Varianz höher bei den Überlebenden,
# ein bahnbrechender Unterschied ist aber nicht festzustellen. 

# Wenn man nun Unterschiede zwischen den Geschlechtern untersucht, fällt auf:
bivariate_stats_md(titanic$Fare, titanic$Sex) 
# dass Frauen durchschnittlich deutlich mehr für ein Ticket gezahlt haben als 
# Männer und 
bivariate_stats_md(titanic$Parch, titanic$Sex) 
# mit mehr Eltern und Kindern gereist sind als Männer. 



# Nun untersuchen wir einige kategoriale Merkmale: 

deskriptive_statistiken_kategorial(titanic$Title)
# So sieht man zum Beispiel, dass die meisten mit "Mr" anzusprechen waren, 
# allerdings auch viele mit "Miss" oder "Mrs". Sämtliche anderen Anreden 
# sind im Vergleich dazu kaum vorkommend, wodurch eine sehr ungleiche 
# Verteilung vorliegt. 

deskriptive_statistiken_kategorial(titanic$Embarked)
# Des Weiteren stiegen die meisten Passagiere in Southampton auf das Schiff, 
# jedoch auch ungefähr ein Viertel in anderen Orten, namentlich Cherbourg 
# und Queenstown. 

deskriptive_statistiken_kategorial(titanic$Deck)
# Während der Schifffahrt war Deck C am meisten gebucht. Doch auch Deck B, D 
# und E erfuhren großen Andrang. Zusammen genommen bieteten diese vier Decks
# rund fünf Sechsteln der Gäste Unterschlupf, während nur ein Sechstel auf 
# einem der anderen vier Decks übernachtete.



#...
