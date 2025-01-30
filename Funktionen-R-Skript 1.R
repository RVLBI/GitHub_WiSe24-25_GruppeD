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
# Output: entsprechende Statistiken für den Zusammenhang der Variablen (eine 
#         benannte Liste und ein Plot)

bivariate_stats_md <- function(metric_var, dichotom_var) {
  # Mittelwerte der metrischen Variable in den beiden Ausprägungen der dichotomen 
  # Variable:
  mittelwerte <- tapply(metric_var, dichotom_var, mean)

  # Das gleiche für die Mediane:
  mediane <- tapply(metric_var, dichotom_var, median)

  # Das gleiche für die Varianzen:
  varianzen <- tapply(metric_var, dichotom_var, var)

  # Boxplots für die metrische Variable bei den beiden Ausprägungen der 
  # dichotomen Variable: 
  boxplot(metric_var ~ dichotom_var, 
          main = "Boxplots je nach Ausprägung der dichotomen Variable")

  # Weichen die beiden Varianzen signifikant voneinander ab?
  p_wert_varianzen <- bartlett.test(metric_var ~ dichotom_var)$p.value
  # p_wert_varianzen gibt die Wahrscheinlichkeit an, dass die gegebenenfalls 
  # vorhandene Abweichung der Varianzen nur durch Zufall besteht
  # (Bei > 0.05 keine signifikante Abweichung)

  # Weichen die beiden Erwartungswerte signifikant voneinander ab?
  if(p_wert_varianzen <= 0.05){ 
    # Es gibt signifikante Anzeichen für unterschiedliche Varianzen:
    p_wert_erwartungswerte <- t.test(metric_var ~ dichotom_var, var.equal = FALSE)
  } else {
    # Es gibt keine signifikanten Anzeichen für unterschiedliche Varianzen:
    p_wert_erwartungswerte <- t.test(metric_var ~ dichotom_var, var.equal = TRUE)
  }
  # p_wert_erwartungswerte gibt die Wahrscheinlichkeit an, dass die gegebenenfalls 
  # vorhandene Abweichung der Mittelwerte nur durch Zufall besteht
  # (Bei > 0.05 keine signifikante Abweichung)

  # Punktbiseriale Korrelation:
  korrelation <- cor(metric_var, as.numeric(dichotom_var), method = "pearson")
  # (Gibt Stärke und Richtung des Zusammenhangs an; Liegt innerhalb des Intervalls 
  # [-1, 1])

  return(list(Mittelwerte = mittelwerte, Varianzen = varianzen, Mediane = mediane,
             P_Wert_Varianzen = p_wert_varianzen, 
             P_Wert_Erwartungswerte = p_wert_erwartungswerte, 
             Korrelation = korrelation))  
}


# v:


