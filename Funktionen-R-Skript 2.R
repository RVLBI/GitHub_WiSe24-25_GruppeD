
# Hilfsfunktion zum Berechen des Modus: 

mod <- function(x) {
  if (length(x) == 0) {
    return(NA)
  } # wenn keine Werte in x vorhanden sind, so wird NA ausgegeben
  freq_table <- table(x) # erstellt eine Häufigkeitstabelle
  modus <- as.numeric(names(freq_table[freq_table == max(freq_table)])) 
  # speichert das Maximum aus der Häufigkeitstabelle in modus
  return(modus) # gibt den Modus aus
}
