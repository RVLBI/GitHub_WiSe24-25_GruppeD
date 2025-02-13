# Aufgabe 2a)
library(vcd) #fuer die Teilaufgabe iii)
# i:



# ii:
# Funktion zur Erstellung von geeigneteten deskriptiven Statistiken für 
# kategorielle (nominale/ordinale) Variablen und deren Ausgabe
# Input: variable als die zu bearbeitende Variable
# Output: entsprechende Statistiken

deskriptive_statistiken_kategorial <- function(variable) {
  # Überprüfung, ob eine kategorielle Variable vorliegt, falls nicht Fehler ausgeben
  # Rausgenommen, um Pclass auch analysieren zu können
  # if (!is.factor(variable) && !is.character(variable)) {
  #   stop("Die Variable muss ein Faktor oder ein Character-Vektor sein.")
  # }
  
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
  
  # Shannon-Entropie berechnen und normieren
  Entropie <- -sum(relhaeufigkeiten * log2(relhaeufigkeiten), na.rm = TRUE)
  Entropie_normiert <- Entropie/log2(unique_values)
  
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
    "normierte Shannon-Entropie" = Entropie_normiert,
    "Gini-Index" = gini_index
    # Das Balkendiagramm wird in Plots erzeugt und muss deshalb hier nicht etxra
    # angegben werden
  )
}


# iii: Analyse der Beziehung zwischen zwei kategorialen Variablen

# analyse_kategorial - fuehrt eine detaillierte Analyse der 
# Beziehung zwischen zwei kategorialen Variablen durch
# und berechnet geeignete statistische Koeffizienten, 
# abhaengig von den Eigenschaften der Variablen.

# Input:
# var1: Name der ersten kategorialen Variablen als Zeichenkette.
# var2: Name der zweiten kategorialen Variablen als Zeichenkette.
# daten: Ein DataFrame, der die Daten enthält, die analysiert werden sollen.

# Output:
# Es wird eine Reihe von statistischen Tests
# und Koeffizienten angezeigt, wie der Chi-Quadrat-Test,
# Cramers V, Phi-Koeffizient, Yules Q, sowie Pearson Kontingenzkoeffizient,
# wenn dies zutrifft.
# Abhaengig von der Art der Variablen wird der geeignete Koeffizient ausgewaehlt.
analyse_kategorial <- function(var1, var2, daten) {
  print(paste("Analyse der Beziehung zwischen", var1, "und", var2))
  
  tabelle <- table(daten[[var1]], daten[[var2]])
  print("Kreuztabelle:")
  print(tabelle)  # Kreuztabelle erstellen
  
  zeilen <- nrow(tabelle)
  spalten <- ncol(tabelle)  # Dimensionen der Tabelle pruefen
  
 
  print("Chi-Quadrat-Test:")
  chi_test <- chisq.test(tabelle)
  print(chi_test) # Chi-Quadrat-Test fuer alle kategorialen Variablen
  
 
  if (zeilen > 2 | spalten > 2) {
    print("Cramers V:")
    
    print(assocstats(tabelle)$cramer) # Cramers V und Kontingenzkoeffizient 
                                       # (falls Tabelle groesser als 2x2 ist)
    
    print("Pearsons Kontingenzkoeffizient:")
    chi_sq <- chi_test$statistic
    n <- sum(tabelle)
    C <- sqrt(chi_sq / (chi_sq + n))
    print(C)
  }
  
  
  if (zeilen == 2 & spalten == 2) {
    print("Phi-Koeffizient:")
    phi <- sqrt(chi_test$statistic / sum(tabelle))
    print(phi)
    
    print("Yules Q-Koeffizient:")
    Q <- (tabelle[1,1] * tabelle[2,2] - tabelle[1,2] * tabelle[2,1]) /
      (tabelle[1,1] * tabelle[2,2] + tabelle[1,2] * tabelle[2,1])
    print(Q)
  } # Falls die Tabelle 2×2 ist, Phi-Koeffizient und Yules Q berechnen
  
 
  if (is.ordered(daten[[var1]]) & is.ordered(daten[[var2]])) {
    print("Kendalls Tau-Koeffizient:")
    var1_numeric <- as.numeric(daten[[var1]])
    var2_numeric <- as.numeric(daten[[var2]])
    print(cor(var1_numeric, var2_numeric, method = "kendall"))
    
    print("Kruskals Gamma-Koeffizient:")
    print(GKgamma(tabelle)) # Pruefen, ob die Variablen ordinal sind
  }
  
  
  if (any(tabelle < 5)) {
    print("Fishers Exakter Test:")
    fisher_test <- fisher.test(tabelle)
    print(fisher_test)# Falls es Zellen mit weniger als 5 
                      # Beobachtungen gibt, Fishers Exakter Test
  }
}


analyse_kategorial("Sex", "Survived", titanic)
analyse_kategorial("Pclass", "Survived", titanic)


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


