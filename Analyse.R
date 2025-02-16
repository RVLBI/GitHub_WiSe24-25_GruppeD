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
# die jeweils passenden Variablen angewendet. Dazu gehen wir nach der Reihenfolge
# der Funktionen vor und untersuchen daher zuerst die metrischen Variablen:

#...

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
# und E erfuhren großen Andrang. Zusammen genommen boten diese vier Decks
# rund fünf Sechsteln der Gäste Unterschlupf, während nur ein Sechstel auf 
# einem der anderen vier Decks übernachtete.

deskriptive_statistiken_kategorial(titanic$Pclass)
# Außerdem war die am meisten genutzte Klasse scheinbar die dritte - sie
# kommt auf mehr Buchende als die ersten beiden Klassen zusammen. 

-------------------------------------------------------------------------------
# Hier werden noch die dichotomen Merkmale untersucht, welche eine besonders
# Form von kategoriellen Merkmalen darstellen:

deskriptive_statistiken_kategorial(titanic$Survived)
# Die Analyse zeigt, dass Menschen an Bord der Titanic mehrheitlich (ca. 60%) 
# nicht überlebt haben. Dennoch ist die normierte Entropie nahe 1, was zeigt, 
# dass die Verteilung nah an einer Gleichverteilung liegt. 

deskriptive_statistiken_kategorial(titanic$Sex)
#
# 

-------------------------------------------------------------------------------

# Wir gehen nun über zu den Zusammenhängen zwischen den verschiedenen Variablen.
# Hier beginnen wir mit Einflüssen von kategorialen Variablen auf andere
# kategoriale Variablen. Dabei ist zu beachten, dass dichotome Variablen auch 
# ein Spezialfall von kategorialen Variablen sind:

analyse_kategorial(titanic$Embarked, titanic$Sex)
# Zuerst der Zusammenhang zwischen dem Zustiegshafen und dem Geschlecht: Anhand 
# der Kreuztabelle sieht man bereits auf den ersten Blick, dass es Unterschiede
# bei der Geschlechtsverteilung zwischen den Häfen gibt.
# Die verschiedenen Koeffizienten deuten alle auf einen schwachen Zusammenhang 
# hin.
# Der Chi-Quadrat-Test legt nahe, dass es einen signifikanten Zusammenhang
# zwischen Zustiegshafen und Geschlecht gibt (P-Wert weit unter 0.05).
# Insgesamt ist festzuhalten, dass es einen gewissen zusammenhang zwischen den 
# beiden Variablen gibt.

analyse_kategorial(titanic$Survived, titanic$Sex)
# Mit der Kreuztabelle sieht man direkt, dass der Anteil der Frauen, die 
# überlebt haben, bei über 50% liegt, während der Anteil der Männer darunter 
# liegt.
# Der Phi-Koeffizient und Yules Q-Koeffizient deuten beude auf einen (negativen) 
# Zusammenhang hin. Dies wird durch den Chi-Quadrat-Test bestätigt: Der Test ist
# signifikant (weit unter 0.05). 
# Das Geschlecht hat demnach einen signifikanten Zusammenhang damit, ob eine 
# Person überlebt hat oder nicht.

analyse_kategorial(titanic$Survived, titanic$Side)
# Bei der Analyse des Zusammenhangs zwischen Boardseite und dem Überleben gibt
# es laut Chi-Quadrat-Test keinen signifikanten Zusammenhang.
# Der Lambda-Koeffizient zeigt jedoch einen mittelstarken Zusammenhang.
# Phi-Koeffizient und Yules Q-Koeffizient deuten auf einen schwachen Zusammenhang 
# hin.
# Die Ergebnisse deuten darauf hin, dass es zwar einen Zusammenhang gibt, aber
# keinen signifikanten.

analyse_kategorial(titanic$Survived, titanic$Deck)
# Im Allgemienen zeigen der Chi-Quadrat-Test, der Fisher-Test, und fast alle
# Koeffizienten keinen oder nur einen schwachen Zusammenhang.
# Als einziges fällt der Lambdakoeffizient für die Zeilen aus der Reihe:
# Dieser deutet auf einen sehr starken Einfluss des Decks auf die Überlebens-
# chancen hin.

# Aus den analysierten Zusammenhängen für das Überleben kann man ableiten, dass
# das Geschlecht den klarsten Zusammenhang hat. Für das Deck und die Boardseite
# gibt es zwar Zusammenhänge, aber nur schwache.

# Nutzung der Funktion aus v
#visualize_kat(titanic$Pclass, titanic$Embarked, titanic$Title, titanic$Deck)

# -----------------------------------------------------------------------------

# Als nächstes gehen wir dem Zusammenspiel von metrischen und dichotomen
# Variablen auf den Grund:

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
# ein bahnbrechender Unterschied liegt aber nicht vor. 

# Wenn man nun Unterschiede zwischen den Geschlechtern untersucht, fällt auf:
bivariate_stats_md(titanic$Fare, titanic$Sex) 
# dass Frauen durchschnittlich deutlich mehr für ein Ticket gezahlt haben als 
# Männer und 
bivariate_stats_md(titanic$Parch, titanic$Sex) 
# mit mehr Eltern und Kindern gereist sind als Männer. 

# Zuletzt analysieren wir den Einfluss, den drei oder vier kategoriale Variablen 
# aufeinander haben:

#...

# Zusammenfassend kann also gesagt werden, dass...
