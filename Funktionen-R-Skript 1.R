# Aufgabe 2a)

# i:



# ii:
# Funktion zur Erstellung von geeigneteten deskriptiven Statistiken für 
# kategorielle (nominale/ordinale) Variablen und deren Ausgabe
# Input: kath_var als die zu bearbeitende Variable
# Output: entsprechende Statistiken
des_stats_kath <- function(kath_var) {
  modus <- mod(kath_var)            # berechnet den Modalwert der Variablen
  modus
  # Die `Höhe` muss noch in einen Vektor umgewandelt werden`
  # barplot(kath_var)                 # erstellt einen Balkendiagramm der Variable
  table(kath_var)                   # erstellt ein Häufigkeitentabelle der Variable 
}


# iii:



# iv: 
# bivariate_stats_md  - Funktion zur Berechnung und Ausgabe von geeigneten  
#                       deskriptiven bivariaten Statistiken für den Zusammenhang  
#                       zwischen einer metrischen und einer dichotomen Variable
#
# Input: metric_var   - Beobachtungsvektor der metrischen Variable
#        dichotom_var - Beobachtungsvektor der dichotomen Variable
# 
# Output: ...

bivariate_stats_md <- function(metric_var, dichotom_var) {
  # Mittelwerte der metrischen Variable in den beiden Ausprägungen der dichotomen 
  # Variable:
  mittelwerte <- tapply(metric_var, dichotom_var, mean)

  # P-Wert des t-Tests: Wahrscheinlichkeit, dass die beiden Mittelwerte nur aus 
  # Zufall voneinander abweichen (wenn sie überhaupt voneinander abweichen)
  p_Wert_t_test <- vollstaendiger_t_test(metric_var, dichotom_var)
}



# v:


