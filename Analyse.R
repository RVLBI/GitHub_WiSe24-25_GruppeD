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

deskript_stat_metr(titanic$Age)
# Zunächst wird das Alter untersucht, wobei aufällt, dass die Maße zum messen des
# Mittel mit 28, 28 und 29.23308 liegen trotz einer Spannweite von fast 80 nah 
# beinander. Die Standartabweichung beträgt ungefähr 13 und die Quartilsdiffernz 
# mit den Quantilen 35 und 21 bei 14. Wegen der Schiefe nahe 0 lässt sich sagen, 
# dass die Verteilung des Alters nah an einer Normalverteilung liegt, jedoch 
# leicht rechtsschief ist. Die Verteilung ist leicht leptokurtisch. 

deskript_stat_metr(titanic$SibSp)
# Beim Untersuchen der Ehefrauen- und Geschwisteranzahl fällt auf, dass trotz 
# des Durchschnittes, der bei 0.523 liegt, und der Spannweite, die von 0 bis 8
# reicht, der Modus und der Median bei 0 liegen. Dies weist zusammen mit dem 
# Schiefe-Koeffizienten auf eine Rechtsschiefe der Verteilung hin. Des Weiteren 
# ist die Verteilung steilgipflig. Es lässt sich also insgesamt schließen, dass 
# die allermeisten ohne viele Geschwister oder Ehefrauen angereist sind und 
# mindestens drei Viertel der Passagiere höchstens eine Person von beiden 
# Gruppen zusammen mitnahmen. Es gab aber auch ein paar Ausreißer nach oben. 

deskript_stat_metr(titanic$Parch)
# Die Anzahl an Eltern und Kindern zeichnet ein sehr ähnliches Bild zu dem 
# der Geschwister und Ehefrauen, nur mit noch etwas extremerer Tendenz zu 
# wenigen Mitreisenden. So liegt hier zum Beispiel selbst das 0.75-Quantil
# bei 0 und die maximale Anzahl an Eltern und Kindern an Bord liegt bei 6.

deskript_stat_metr(titanic$Fare)
# Der Ticketpreis als letzte zu untersuchende metrische Variable lag zwischen 
# 0 und 512.33 (wahrscheinlich Pfund), allerdings lagen die meisten Preise 
# im unteren Bereich dieser Spanne, was zum Beispiel der Durchschnitt von 32.20 
# und der Median von 14.45 zeigen. Ebenfalls interessant ist, dass sogar das 
# 75 Prozent Quantil mit 31 knapp unter dem arithmetischen Mittel liegt, was
# auf große Ausreißer nach oben deutet. Auch diese Verteilung ist wieder 
# rechtsschief und etwas leptokurtisch.

# ------------------------------------------------------------------------------

# Nun untersuchen wir die kategorialen Merkmale: 

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

# Hier werden noch die dichotomen Merkmale untersucht, welche eine besondere
# Form von kategoriellen Merkmalen darstellen:

deskriptive_statistiken_kategorial(titanic$Survived)
# Die Analyse zeigt, dass Menschen an Bord der Titanic mehrheitlich (ca. 60%) 
# nicht überlebt haben. Dennoch ist die normierte Entropie nahe 1, was zeigt, 
# dass die Verteilung nah an einer Gleichverteilung liegt. 

deskriptive_statistiken_kategorial(titanic$Sex)
# Es fällt auf, dass ca. 2/3 der Passagiere männlich waren und ca. 1/3 weiblich.
# Zudem stellt sich auch hier herraus, die Entropie mit 0.9 schon sehr hoch ist
# und mit einem Gini-Index nahe 0.5 sind die Geschlechter weder besonders 
# gleichmäßig noch besonders ungelichmäßig verteilt. 

# -------------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------

# Zuletzt analysieren wir den Einfluss, den drei oder vier kategoriale Variablen 
# aufeinander haben:

visualize_kat(titanic, "Sex", "Survived", "Age")
#...

visualize_kat(titanic, "Pclass", "Sex", "Survived", "Embarked")
# Die Grafik zeigt, dass die Passagiere, die in Queenstown zugestiegen sind, 
# nahezu alle in PClass 3, also der niedrigsten Klasse untergekommen sind. Zudem
# wird deutslich, dass proportial die meisten Passagiere in Cherbourg der besten
# Klasse (PClass = 1) angehören. 
# Dies könnte der Fall sein, da sich die Passagiere aus Cherbourg mehr leisten
# können, also mehr für ihr Ticket ausgeben können. 
# Diese These lässt sich dadurch belegen, wenn die Mittelwerte von Fare an den 
# einzelnen Einstiegsorte bestimmt werden. 
mittel_bestimmter_auspraegung(titanic$Embarked, titanic$Fare, "Queenstown") # 13.27603
mittel_bestimmter_auspraegung(titanic$Embarked, titanic$Fare, "Southampton") # 27.07981
mittel_bestimmter_auspraegung(titanic$Embarked, titanic$Fare, "Cherbourg") # 59.95414
# Folglich gaben die in Cherbourg zugestiegenen tatsächlich viel mehr für ihre 
# Tickets aus, als die anderen, um in einer bessere Klasse zu "wohnen". 
# Der Preis wird insbesondere durch die höhere Klasse höher. 
mittel_bestimmter_auspraegung(titanic$Pclass, titanic$Fare, "3") # 13.67555
mittel_bestimmter_auspraegung(titanic$Pclass, titanic$Fare, "2") # 20.662218
mittel_bestimmter_auspraegung(titanic$Pclass, titanic$Fare, "1") # 84.15469

# ------------------------------------------------------------------------------------

# Zusammenfassend kann also gesagt werden, dass...
