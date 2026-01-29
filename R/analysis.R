# Hilfsfuktionen laden
source("functions/descriptive_stats.R")
source("functions/helper_functions.R")

# aufgeraeumten Datensatz laden
titanic <- read.csv("data/titanic_cleaned.csv")

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
