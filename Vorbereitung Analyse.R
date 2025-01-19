# setwd("C:/Users/tikva/Desktop")

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Trennen der Anreden von den Namen: 
titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name)  

# Umbenennung der Anreden zur Übersichtlichkeit: 
titanic$Title <- gsub("Mlle|Ms|Miss", "Miss", titanic$Title)
titanic$Title <- gsub("Mme", "Mrs", titanic$Title)
titanic$Title <- gsub("Master", "Mr", titanic$Title)


# Kodierung von "Survived" als factor:
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1),
                           labels = c("No", "Yes"))

# Kodierung von "Sex" als factor:
titanic$Sex <- factor(titanic$Sex, levels = c("male", "female"))

# Kodierung von "Embarked" als factor:
titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"), 
                           labels = c("Cherbourg", "Queenstown", "Southampton"))

# Kodierung von "PClass" als ordered-factor:
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE, levels = c(3, 2, 1))

# Altersangeben: nicht angegebene durch den Median, der Gruppe mit derselben Anrede ersetzen 
impute_age <- function(title, age) {                                         # neue Funktion mit Parameter Titel (Anrede) und Alter erstellen
  if (is.na(age)) {                                                          # Abfrage, ob das Alter nicht angegeben ist
    median(titanic$Age[titanic$Title == title & !is.na(titanic$Age)],
           na.rm = TRUE)                                                     # wenn ja, den median, derer mir demselben Titel bestimmt, ohne NA zu berücksichtigen
  } else {
    age                                                                      # sonst das Alter ´zurückgeben´
  }
}
titanic$Age <- mapply(impute_age, titanic$Title, titanic$Age)                # die Funktion auf den Datensatz anwenden


# Deck und Boardseite aus Cabin extrahieren
# Deck: Falls leer auf NA setzen, sonst Buchstaben behalten
titanic$Deck <- ifelse(titanic$Cabin == "", NA, substr(titanic$Cabin, 1, 1))

# Boardseite: Anhand von Kabinennummer Side angeben, sonst auf NA setzen
titanic$Side <- ifelse(grepl("[0-9]", titanic$Cabin),                        # Zahl enthalten?
                       ifelse(as.numeric(gsub("[^0-9]", "", titanic$Cabin))  # entferne Buchstaben
                       %% 2 == 0, "Backbord", "Steuerbord"), NA)             # Kodierung

# Cabin: Leere "Kabinen" auf NA setzen
titanic$Cabin <- ifelse(titanic$Cabin == "", NA, titanic$Cabin)


titanic <- titanic[, !(names(titanic) %in% 
                         c("PassengerID", "Name", "Ticket", "Cabin"))]

# Update für die csv-Datei
write.csv(titanic, "processed_titanic.csv", row.names = FALSE)

