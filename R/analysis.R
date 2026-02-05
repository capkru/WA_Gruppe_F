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

##Density curve
plot(density(titanic$Age, na.rm = TRUE),
     main = "Density Curve of Age",
     xlab = "Age",
     lwd = 2)

##Boxplot
boxplot(titanic$Age,
        horizontal = TRUE,
        col = "blue",
        main = "Boxplot of Age")

stats_metric(titanic$Fare)
##Density curve
plot(density(titanic$Fare, na.rm = TRUE),
     main = "Density Curve of Fare",
     xlab = "Age",
     lwd = 2)

##boxplot
boxplot(titanic$Fare,
        horizontal = TRUE,
        col = "orange",
        main = "Boxplot of Fare")

## Verteilung der Ticketpreise
# 1. 95%-Quantil berechnen 
limit_95 <- quantile(titanic$Fare, 0.95, na.rm = TRUE)

# 2. Daten filtern: Nur Preise unterhalb dieser Grenze nutzen
fare_sub <- titanic$Fare[titanic$Fare <= limit_95]

# 3. Grafik erstellen
hist(fare_sub, 
     freq = FALSE,                
     breaks = 20,                 
     main = "Verteilung der Ticketpreise (95%-Interval)",
     xlab = "Preis (Fare) in £",
     col = "lightblue", 
     border = "white")

# 4. Dichtekurve darüberlegen
lines(density(fare_sub), 
      col = "darkred", 
      lwd = 2)                    

# 5. Median berechnen  
med_val <- median(titanic$Fare, na.rm = TRUE) 

# 6. Vertikale Linie zeichnen 
abline(v = med_val, 
       col = "blue", 
       lwd = 2, 
       lty = 2) # lty=2 macht die Linie gestrichelt

# 7. Textbeschriftung an die Linie kleben
text(x = med_val, 
     y = 0.04,  # y-Position muss man evtl. anpassen je nach Höhe der Dichte
     labels = paste("Median:", round(med_val, 2),"£"), 
     pos = 4,   # 4 = rechts vom Punkt
     col = "blue", 
     cex = 0.8) # Schriftgröße etwas kleiner


# 8. Rug hinzufügen
rug(fare_sub, col = rgb(0, 0, 0, 0.1)) 


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


