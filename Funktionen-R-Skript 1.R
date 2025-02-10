# Aufgabe 2a)

# i:



# ii:
# Funktion zur Erstellung von geeigneteten deskriptiven Statistiken für 
# kategorielle (nominale/ordinale) Variablen und deren Ausgabe
# Input: variable als die zu bearbeitende Variable
# Output: entsprechende Statistiken

deskriptive_statistiken_kategorial <- function(variable) {
  # Überprüfung, ob eine kategorielle Variable vorliegt, falls nicht Fehler ausgeben
  if (!is.factor(variable) && !is.character(variable)) {
    stop("Die Variable muss ein Faktor oder ein Character-Vektor sein.")
  }
  
  # absolute Häufigkeiten berechnen
  haeufigkeiten <- table(variable) 
  # relative Häufigkeiten berechnen
  relhaeufigkeiten <- prop.table(haeufigkeiten)
  
  # Modus bestimmen (Hilffunktion in Funktionen-R-Skript 2)
  modus <- mod(variable)
  
  # Dominanz der häufigsten Kategorie
  dominanz <- max(relhaeufigkeiten)
  
  # Anzahl der einzigartigen Werte
  unique_values <- length(unique(variable))
  
  # Shannon-Entropie berechnen
  Entropie <- -sum(relhaeufigkeiten * log2(relhaeufigkeiten), na.rm = TRUE)
  
  # Gini-Index berechen (Ungleichverteilungsmaß (0 = gleichmäßig, 1 = maximal ungleich))
  gini_index <- 1 - sum(relhaeufigkeiten^2)
  
  # Balkendiagramm zu den Häufigkeiten erstellen
  Balkendiagramm_kathegorial(variable)
  
  # Ergebnisse ausgeben
  list(
    "Absolute Häufigkeiten" = haeufigkeiten,
    "Relative Häufigkeiten" = relhaeufigkeiten,
    "Modus" = modus,
    "Dominanzindex"= dominanz,
    "Anzahl einzigartiger Kategorien" = unique_values,
    "Shannon-Entropie" = Entropie,
    "Gini-Index" = gini_index
    # Das Balkendiagramm wird in Plots erzeugt und muss deshalb hier nicht etxra
    # angegben werden
  )
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
    p_wert_erwartungswerte <- t.test(metric_var ~ dichotom_var, var.equal = FALSE)$p.value
  } else {
    # Es gibt keine signifikanten Anzeichen für unterschiedliche Varianzen:
    p_wert_erwartungswerte <- t.test(metric_var ~ dichotom_var, var.equal = TRUE)$p.value
  }
  # p_wert_erwartungswerte gibt die Wahrscheinlichkeit an, dass die gegebenenfalls 
  # vorhandene Abweichung der Mittelwerte nur durch Zufall besteht
  # (Bei > 0.05 keine signifikante Abweichung)

  return(list(Mittelwerte = mittelwerte, 
              Varianzen = varianzen, 
              Mediane = mediane,
              P_Wert_Varianzen = p_wert_varianzen, 
              P_Wert_Erwartungswerte = p_wert_erwartungswerte))  
}


# v:


