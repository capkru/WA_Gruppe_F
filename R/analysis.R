# Hilfsfuktionen laden
source("R/descriptive_stats.r")
source("R/helper_functions.r")

# aufgeraeumten Datensatz laden
titanic <- read.csv("data/titanic_cleaned.csv")

# CSVs speichern keine Faktoren. Wir müssen R sagen, was kategorial ist.
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass   <- as.factor(titanic$Pclass)
titanic$Sex      <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# (i) Metric variables
stats_metric(titanic$Age)
stats_metric(titanic$Fare)

# (ii) Categorical variables
stats_categorical(titanic$Sex)
stats_categorical(titanic$Survived)

# (iii) Two categorical variables
bivariat_kategorial(titanic, "Survived", "Sex")
bivariat_kategorial(titanic, "Survived", "Pclass")

# (iv) Metric × dichotomous
bivariat_metrisch_dichotom(titanic, "Age", "Survived")
bivariat_metrisch_dichotom(titanic, "Fare", "Survived")

# (v) Visualisierung von Daten
plot_categorical_variables(
  titanic,
  "Survived",
  "Sex",
  "Pclass"
)

# Piechart für die Ticket-Klassen
piechart_categorical(titanic, "Pclass")

# Piechart für den Hafen
piechart_categorical(titanic, "Embarked")


