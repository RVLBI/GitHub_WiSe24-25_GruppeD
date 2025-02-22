# Aufgabe 2a)
library(vcd) #fuer die Teilaufgabe iii)
library(ggplot2) #fuer die Teilaufgabe v)

source("Funktionen-R-Skript 2.R")

# i:
# deskript_stat_metr: 
# Funktion zur Erstellung von geeigneteten deskriptiven Statistiken für 
# metrische Variablen und deren Ausgabe

# Input: 
# variable - eine Variable des Datensatzes als die zu bearbeitende Variable

# Output: 
# entsprechende Statistiken für die Beschreibung einer metrischen Variable
# Genauer: Arithmetisches Mittel, Modus, Median und andere Quantile, Maximum und
# Minimum, Quantilsdifferenz, Spannweite, Standardabweichung, Varianz,
# Varianzkoeffizient, Schiefe und Woelbung.
# Gibt einen Boxplot am Ende aus.

deskript_stat_metr <- function(variable){
  
  # überprüfen, ob Variable metrisch ist
  if (!is.numeric(variable)) {stop("Die Variable muss metrisch sein.")}
  
  # Arithmetisches Mittel bestimmen:
  arith_mittel <- mean(variable)
  
  # Modus bestimmen: (Hilffunktion in Funktionen-R-Skript 2)
  modus <- mod(variable)
  
  # Quartile bestimmen:
  median <- median(variable, na.rm = T)
  minimum <- min(variable, na.rm = T)
  maximum <- max(variable, na.rm = T)
  quartil_25 <- quantile(variable, probs = 0.25, names = F, na.rm = T)
  quartil_75 <- quantile(variable, probs = 0.75, names = F, na.rm = T)
  
  # Quantilsdifferenz bestimmen:
  qd <- IQR(variable, na.rm = T)
  
  # Spannweite bestimmen:
  spannweite <- maximum - minimum
  
  # Standardabweichung bestimmen:
  stand_abw <- sd(variable, na.rm = T)
  
  # Varianz bestimmen:
  varianz <- var(variable, na.rm = T)
  
  # Variationskoeffizient bestimmen:
  vark <- stand_abw/arith_mittel
  
  # Schiefe bestimmen (mit Fkt aus den Hilfsfktnen):
  schiefe <- schiefefkt(arith_mittel, median, stand_abw)
  
  #Wölbung bestimmen:
  woelbung <- woelbungfkt(variable, qd)
  
  # Boxplot ausgeben:
  name <- deparse(substitute(variable)) # bestimmen Namen der Variable
  name <- tail(strsplit(name, "\\$")[[1]], 1) # entferne titanic$
  boxplot(variable, xlab = name)
  
  return(list(Arith_Mittel = arith_mittel,
              Modus = modus,
              Median = median,
              Minimum = minimum,
              Maximum = maximum,
              Quartil_25 = quartil_25,
              Quartil_75 = quartil_75,
              QD = qd,
              Spannweite = spannweite,
              Stand_Abw = stand_abw,
              Varianz = varianz,
              VarKoeffizient = vark,
              Schiefe = schiefe,
              Woelbung = woelbung))
}



# ii:
# deskriptive_statistiken_kategorial: 
# Funktion zur Erstellung von geeigneteten deskriptiven Statistiken für 
# kategorielle (nominale/ordinale) Variablen und deren Ausgabe

# Input: 
# variable - eine Variable des Datensatzes als die zu bearbeitende Variable

# Output: 
# entsprechende Statistiken für die Beschreibung einer kateogriellen Variable
# genauer, absolute und relative Häufigkeiten, den Modus, den Dominanzindex, 
# die Anzahl der unterschiedlichen Werte der Variablen, die Entropie, den Gini-
# Index, sowie ein Balkendiagramm des zu analysierenden Merkmals.

deskriptive_statistiken_kategorial <- function(variable) {
  # Überprüfung, ob eine kategorielle Variable vorliegt, falls nicht Fehler ausgeben
  if (is.numeric(variable)) {
     stop("Die Variable muss kategoriell sein.")
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
    "normierte Shannon-Entropie" = Entropie_normiert,
    "Gini-Index" = gini_index
    # Das Balkendiagramm wird in Plots erzeugt und muss deshalb hier nicht extra
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

analyse_kategorial <- function(var1, var2) {
  
  tabelle <- table(var1, var2)
  print("Kreuztabelle:") # Kreuztabelle erstellen
  print(tabelle)
  
  print("Chi-Quadrat-Test:")
  chi_test <- chisq.test(tabelle, simulate.p.value = TRUE) # Chi-Quadrat-Test fuer alle kategorialen Variablen
  print(chi_test)
  
  max_row <- apply(tabelle, 1, max)
  max_col <- apply(tabelle, 2, max)
  total <- sum(tabelle) # Lambda-Koeffizient berechnen
  
  lambda_row <- (total - sum(max_row)) / total
  lambda_col <- (total - sum(max_col)) / total
  print("Lambda-Koeffizient:")
  print(list(lambda_row = lambda_row, lambda_col = lambda_col))
  
  if (ist_2x2(tabelle)) {
    print("Phi-Koeffizient:")
    phi <- sqrt(chi_test$statistic / total)
    print(phi)
    
    print("Yules Q-Koeffizient:") # Falls die Tabelle 2×2 ist, Phi-Koeffizient und Yules Q berechnen
    Q <- (tabelle[1,1] * tabelle[2,2] - tabelle[1,2] * tabelle[2,1]) /
         (tabelle[1,1] * tabelle[2,2] + tabelle[1,2] * tabelle[2,1])
    print(Q)
  } else {
    print("Cramers V:")
    cramers_v <- sqrt(chi_test$statistic / (total * (min(nrow(tabelle), ncol(tabelle)) - 1)))
    print(cramers_v) # Cramers V und Kontingenzkoeffizient 
                                       # (falls Tabelle groesser als 2x2 ist)
    
    print("Pearsons Kontingenzkoeffizient:")
    chi_sq <- chi_test$statistic
    C <- sqrt(chi_sq / (chi_sq + total))
    print(C)
  }
  
  if (is.ordered(var1) & is.ordered(var2)) {
    print("Kendalls Tau-Koeffizient:") # Pruefen, ob die Variablen ordinal sind
    var1_numeric <- as.numeric(var1)
    var2_numeric <- as.numeric(var2)

    print(cor(var1_numeric, var2_numeric, method = "kendall"))
    
    print("Kruskals Gamma-Koeffizient:")
    print(GKgamma(tabelle))
  }
  
  if (any(tabelle < 5)) {
    print("Fishers Exakter Test:")
    tabelle[tabelle > 5] <- 0 # setze alle Einträge, die >5 sind, auf 0
    fisher_test <- fisher.test(tabelle)
    print(fisher_test) # Falls es Zellen mit weniger als 5 
                      # Beobachtungen gibt, Fishers Exakter Test
  }

  # Ausgabe als gestapeltes Balkendiagramm mit Legende:
  opar <- par(mai = c(1,1,1,1), lwd = 2, cex = 1.4, las = 1)
  plot.new()
  x <- table(var1, var2)
  barplot(x, legend =TRUE, args.legend = list(x = "topright", horiz = FALSE, cex = 0.65))
  par(opar)
}

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
# visualize_kat - Funktion zur Visualisierung von drei oder vier  
#                         kategorialen Variablen in einem Balkendiagramm
#
# Input: data - Datensatz (Dataframe)
#        var1 - Name der ersten kategorialen Variable 
#        var2 - Name der zweiten kategorialen Variable 
#        var3 - Name der dritten kategorialen Variable
#        var4 - (Optional) Name der vierten kategorialen Variable 
#
# Output: Ein Balkendiagramm mit Gruppierung und Facettierung

visualize_kat <- function(data, var1, var2, var3, var4 = NULL){
  # Überprüfen, ob eine vierte kategoriale Variable angegeben wurde
  if (!is.null(var4)) {
  # Vier kategoriale Variablen: Mehrere Facetten
  # Falls eine vierte Variable existiert, wird für jede ihrer Ausprägungen ein separates Diagramm erstellt
    par(mfrow = c(1, length(unique(data[[var4]]))))  # Unterteilung des Plots
      # Iteration über jede Ausprägung der vierten Variable
    for (level in unique(data[[var4]])) {
      subset_data <- data[data[[var4]] == level, ]
      # Erstellen einer Häufigkeitstabelle für die drei verbleibenden kategorialen Variablen
      counts <- table(subset_data[[var1]], subset_data[[var2]], subset_data[[var3]])
      counts_matrix <- apply(counts, c(1, 2), sum)  # summiere die 3. Dimension
      barplot(counts_matrix, beside = TRUE, legend = TRUE, main = paste(var4, "=", level))
    }
    par(mfrow = c(1,1))  # Standardansicht wiederherstellen
  } 
  else {
    # Drei kategoriale Variablen: Gruppenbalkendiagramm
    counts <- table(data[[var1]], data[[var2]], data[[var3]])
    counts_matrix <- apply(counts, c(1, 2) , sum)
    barplot(counts_matrix , beside = TRUE, legend = TRUE, 
            main = paste("Balkendiagramm von", var1, "nach", var2, "und", var3))
  }
}

# visualize_kat_alternative - Funktion zur Visualisierung von drei oder vier  
#                 kategorialen Variablen in einem Balloon-Plot
#
# Input: data - Datensatz (Dataframe)
#        var1 - Name der ersten kategorialen Variable 
#        var2 - Name der zweiten kategorialen Variable 
#        var3 - Name der dritten kategorialen Variable
#        var4 - (Optional) Name der vierten kategorialen Variable 
#
# Output: Ein Balloon-Plot mit Gruppierung und Facettierung

visualize_kat_alternative <- function(data, var1, var2, var3, var4 = NULL) {
  if (!is.null(var4)) {
    # Erstelle Häufigkeitstabelle für vier Variablen:
    tbl <- as.data.frame(table(data[[var1]], data[[var2]], data[[var3]], data[[var4]]))
    colnames(tbl) <- c(var1, var2, var3, var4, "Freq")
    
    # Balloon-Plot mithilfe der Häufigkeitstabelle erstellen:
    ggplot(tbl, aes_string(x = var1, y = var2, size = "Freq", fill = "Freq")) +
      geom_point(shape = 21, colour = "black") +
      facet_grid(as.formula(paste(var3, "~", var4))) + # In Teilplots nach dritter und vierter Variable unterteilen 
      scale_size_continuous(range = c(2, 10)) + # Spannweite für Größe der Punkte
      ggtitle("Balloon Plot für vier kategoriale Variablen")
  } else {
    # Erstelle Häufigkeitstabelle für drei Variablen:
    tbl <- as.data.frame(table(data[[var1]], data[[var2]], data[[var3]]))     
    colnames(tbl) <- c(var1, var2, var3, "Freq")          
    
    # Balloon-Plot mithilfe der Häufigkeitstabelle erstellen:
    ggplot(tbl, aes_string(x = var1, y = var2, size = "Freq", fill = "Freq")) +       
      geom_point(shape = 21, colour = "black") +  
      facet_wrap(as.formula(paste("~", var3))) +  # In Teilplots nach dritter Variable unterteilen  
      scale_size_continuous(range = c(2, 10)) +   # Spannweite für Größe der Punkte    
      ggtitle("Balloon Plot für drei kategoriale Variablen")   
  }
}


# iv): 
# mittel_bestimmter_auspraegung - Funktion zur Bestimmung des Mittelwertes der 
#                                 metrischen Variable2, bei den Einträgen, die 
#                                 mit auspraegung1 von variable1 übereinstimmen

# Input: 
# variable1: eine Variable des Datensatzes
# variable2: eine metische Variable des Datensatzes
# auspraegung1: die zu untersuchende Ausprägung von variable1

# Output: 
# Der Mittelwert von Variable 2 von allen Einträgen, die die gewünscht 
# Eigenschaft (variable1 = auspraegung) haben.

mittel_bestimmter_auspraegung <- function(variable1, variable2, auspraegung1) {
  var1 <- variable1 == auspraegung1
  return(mean(variable2[var1], na.rm = TRUE))
}
