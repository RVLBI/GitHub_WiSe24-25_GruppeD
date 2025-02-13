install.packages("ggplot2")

# Hilfsfunktion zum Berechen des Modus: 

mod <- function(x) {
  if (length(x) == 0) {
    return(NA)
  } # wenn keine Werte in x vorhanden sind, so wird NA ausgegeben
  freq_table <- table(x) # erstellt eine Häufigkeitstabelle
  modus <- names(freq_table)[which.max(freq_table)]
  # speichert das Maximum aus der Häufigkeitstabelle in modus
  return(modus) # gibt den Modus aus
}

# Hilfsfunktion für ein Bakendiagramm zur 2.FUnktion:

Balkendiagramm_kathegorial <- function(x) {
  haeufigkeiten <- table(x)
  barplot(haeufigkeiten, 
          main = "Häufigkeitsverteilung der Kategorien",
          xlab = "Kategorie",
          ylab = "Häufigkeit")
}


# Berechnung der Schiefe:

schiefefkt <- function(arith_m, med, sd) {
  # benutze Formel sk3 aus deskriptive Statistik: (xquer - med)/sd
  ergebnis <- (arith_m - med)/sd
  return(ergebnis)
}


# Berechnung der Wölbung:

woelbungfkt <- function(variable, qd) {
  # benutze Formel aus deskr. Stat. für explorativen Wölbungskoeffizient
  Q9 <- quantile(variable, probs = 0.9, names = F, na.rm = T)
  Q1 <- quantile(variable, probs = 0.1, names = F, na.rm = T)
  ergebnis <- 0.5 * (qd/(Q9-Q1))
  return(ergebnis)
}
  