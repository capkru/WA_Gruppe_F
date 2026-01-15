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

# Schritt 1.4 
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

# Testausgabe für Pclass
# print("Check Pclass:")
# print(head(titanic$Pclass))

# Wir berechnen den Median des Alters für jede Anrede-Gruppe
# und füllen damit die Lücken (NA) im Alter auf
for (t in unique(titanic$Anrede)) {
  median_age <- median(titanic$Age[titanic$Anrede == t], na.rm = TRUE)
  titanic$Age[is.na(titanic$Age) & titanic$Anrede == t] <- median_age
}

# Falls noch NAs übrig sind (bei sehr seltenen Anreden), nehmen wir den Gesamt-Median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

print("Check Age (keine NAs mehr):")
print(sum(is.na(titanic$Age)))