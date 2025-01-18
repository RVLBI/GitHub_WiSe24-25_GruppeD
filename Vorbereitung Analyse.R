
setwd("C:/Users/tikva/Desktop")

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name)  
#Muss man Master auch ersetzen?
titanic$Title <- gsub("Mlle|Ms|Miss", "Miss", titanic$Title)
titanic$Title <- gsub("Mme", "Mrs", titanic$Title)
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1),
                           labels = c("No", "Yes"))
titanic$Sex <- factor(titanic$Sex, levels = c("male", "female"))
titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"), 
                           labels = c("Cherbourg", "Queenstown", "Southampton"))
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE, levels = c(3, 2, 1))

impute_age <- function(title, age) {
  if (is.na(age)) {
    median(titanic$Age[titanic$Title == title & !is.na(titanic$Age)],
           na.rm = TRUE)
  } else {
    age
  }
}
titanic$Age <- mapply(impute_age, titanic$Title, titanic$Age)
titanic$Deck <- ifelse(titanic$Cabin == "", NA, substr(titanic$Cabin, 1, 1))
titanic$Side <- ifelse(grepl("[0-9]", titanic$Cabin), 
                       ifelse(as.numeric(gsub("[^0-9]", "", titanic$Cabin)) 
                              %% 2 == 0, "Backbord", "Steuerbord"), 
                       NA)


titanic <- titanic[, !(names(titanic) %in% 
                         c("PassengerID", "Name", "Ticket", "Cabin"))]
write.csv(titanic, "processed_titanic.csv", row.names = FALSE)

#Fehlt noch Git-Teil