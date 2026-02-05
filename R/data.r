### Aufgabe 1

## 0. Daten laden (Anas)
titanic <- read.csv("data/titanic.csv")


## 1. Anrede extrahieren (Anas)
# Wir suchen nach dem Wort, das zwischen dem Komma und dem Punkt im Namen steht
titanic$Anrede <- sub(".*, (.*?)\\..*", "\\1", titanic$Name)

#Leerzeichen entfernen 
titanic$Anrede <- trimws(titanic$Anrede)

# Test 
# print(head(titanic$Anrede))


## 2. „Survived“, „Sex“, „Embarked“ als factor umcodieren (Gregor)
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), 
                           labels = c("No", "Yes"))
titanic$Sex <- factor(titanic$Sex, levels = c("male", "female"),
                      labels = c("male", "female"))
titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"),
                           labels = c("Cherbourg", "Queenstown", "Southampton"))


## 3. Pclass umwandeln: 1 = höchste, 2 = mittlere, 3 = untere Klasse (Anas)
titanic$Pclass <- factor(titanic$Pclass, 
                         levels = c(3, 2, 1), 
                         ordered = TRUE)

# Tests:
# print("Check Pclass:")
# print(head(titanic$Pclass))


## 4. Fehlende Alterswerte auffüllen (Anas)
# Wir berechnen den Median des Alters für jede Anrede-Gruppe
# und füllen damit die Lücken (NA) im Alter auf
for (t in unique(titanic$Anrede)) {
  median_age <- median(titanic$Age[titanic$Anrede == t], na.rm = TRUE)
  titanic$Age[is.na(titanic$Age) & titanic$Anrede == t] <- median_age
}
# Falls noch NAs übrig sind (bei sehr seltenen Anreden), nehmen wir den Gesamt-Median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# Test 
# print(sum(is.na(titanic$Age)))


## 5. Deck und Side aus Cabin extrahieren (Anas)
titanic$Cabin[titanic$Cabin == ""] <- NA
titanic_numbers <- as.numeric(gsub("[^0-9]", "", titanic$Cabin))
titanic$Side <- ifelse(is.na(titanic_numbers), NA,
           ifelse(titanic_numbers %% 2==0,"Backbord", "Steuerbord"))
titanic$Deck <- substr(titanic$Cabin, 1, 1)
# Einträge n´mit unbekannter Kabine als NA setzen (Gregor)
titanic$Deck[titanic$Deck == ""] <- NA

# Test
# print(head(titanic[, c("Cabin", "Deck", "Side"),10]))


## 6. Spalten entfernen (Anas)
col_to_remove <- c("Name", "Ticket", "Cabin", "PassengerId")
titanic_final <- titanic[, !(names(titanic) %in% col_to_remove)]

# Test 
#print(colnames(titanic_final))


# 7. Bereinigte Daten speichern (Anas)
write.csv(titanic_final, "data/titanic_cleaned.csv", row.names = FALSE)

