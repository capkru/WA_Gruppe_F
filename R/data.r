#
# Gregorii 
#
#
#
#
#
#
#
#
#
#
#
#
#
#

# fehlende Werte imputieren und neue Variablen erstellen
# 1. Daten laden (Pfad anpassen, falls nötig)
titanic <- read.csv("data/titanic.csv")
# 2. Anrede extrahieren
# Wir suchen nach dem Wort, das zwischen dem Komma und dem Punkt im Namen steht
titanic$Anrede <- sub(".*, (.*?)\\..*", "\\1", titanic$Name)

# 3. Leerzeichen entfernen 
titanic$Anrede <- trimws(titanic$Anrede)

# Test 
# print(head(titanic$Anrede))

# Pclass umwandeln: 1 = höchste, 2 = mittlere, 3 = untere Klasse
titanic$Pclass <- factor(titanic$Pclass, 
                         levels = c(3, 2, 1), 
                         ordered = TRUE)

# ----Test---- print("Check Pclass:")
# ----Test---- print(head(titanic$Pclass))

# Wir berechnen den Median des Alters für jede Anrede-Gruppe
# und füllen damit die Lücken (NA) im Alter auf
for (t in unique(titanic$Anrede)) {
  median_age <- median(titanic$Age[titanic$Anrede == t], na.rm = TRUE)
  titanic$Age[is.na(titanic$Age) & titanic$Anrede == t] <- median_age
}
# Falls noch NAs übrig sind (bei sehr seltenen Anreden), nehmen wir den Gesamt-Median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# ----Test---- print(sum(is.na(titanic$Age)))

# Deck und Side aus Cabin extrahieren 
titanic$Cabin[titanic$Cabin == ""] <- NA
titanic_numbers <- as.numeric(gsub("[^0-9]", "", titanic$Cabin))
titanic$Side <- ifelse(is.na(titanic_numbers), NA,
           ifelse(titanic_numbers %% 2==0,"Backbord", "Steuerbord"))
titanic$Deck <- substr(titanic$Cabin, 1, 1)

# ----Test----  print(head(titanic[, c("Cabin", "Deck", "Side"),10]))

# Spalten entfernen , die nicht mehr benötigt werden
col_to_remove <- c("Name", "Ticket", "Cabin", "PassengerId")
# titanic_final <- titanic[, !(names(titanic) %in% col_to_remove)]

# ---test--- print(colnames(titanic_finale))

# write.csv(titanic_final, "data/titanic_cleaned.csv", row.names = FALSE)