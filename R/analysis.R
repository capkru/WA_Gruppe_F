Sys.setlocale("LC_ALL", "de_DE.UTF-8")
#Pakete laden
library(ggplot2)

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
##titanic$Age
stats_metric(titanic$Age)
##Boxplot 
boxplot(titanic$Age,
        horizontal = TRUE,
        col = "lightblue",
        main = "die Verteilung des Alters",
        xlab = "Alter")

##titanic$Fare
stats_metric(titanic$Fare)
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
     ylab = "Dichte",
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
##titanic$Sex
stats_categorical(titanic$Sex)
##barplot
counts <- table(titanic$Sex)
props  <- prop.table(counts)

bp <- barplot(props,
              col = c("pink", "lightblue"),
              main = "Relative Verteilung des Geschlechts",
              ylab = "Anteil",
              ylim = c(0, max(props) * 1.25),
              names.arg = c("weiblich", "männlich"))

labels <- paste0(round(props * 100, 1), "%\n(n=", counts, ")")

text(x = bp,
     y = props,
     labels = labels,
     pos = 3)

##titanic$Survived
stats_categorical(titanic$Survived)
##barplot
counts <- table(titanic$Survived)
props  <- prop.table(counts)

bp <- barplot(props,
              col = c("pink", "lightblue"),
              main = "Relative Verteilung der Überlebenden",
              ylab = "Anteil",
              ylim = c(0, max(props) * 1.25),
              names.arg = c("Nein", "Ja"))

labels <- paste0(round(props * 100, 1), "%\n(n=", counts, ")")

text(x = bp,
     y = props,
     labels = labels,
     pos = 3)

# (iii) Two categorical variables
#Überlebensrate und Geschlecht
bivariat_kategorial(titanic, "Survived", "Sex")

##Stackbarplot
tab_sex_prop <- prop.table(tab_sex, margin = 2)
##stacked barplot
barplot(tab_sex_prop,
        col = c("red", "green"),
        main = "Überlebensrate nach Geschlecht",
        ylab = "Anteil",
        legend.text = c("Nicht überlebt", "Überlebt"),
        names.arg = c("weiblich", "männlich"))

#Überlebensrate und Pclass
bivariat_kategorial(titanic, "Survived", "Pclass")
##Stacked Barplot
tab_class_prop <- prop.table(tab_class, margin = 2)

barplot(tab_class_prop,
        col = c("red", "green"),
        main = "Überlebensrate nach Pclass",
        ylab = "Anteil",
        legend.text = c("Nicht überlebt", "Überlebt"))

# (iv) Metric × dichotomous
#Age nach Survive
bivariat_metrisch_dichotom(titanic, "Age", "Survived")
#Fare nach Survive
bivariat_metrisch_dichotom(titanic, "Fare", "Survived")

# (v) Visualisierung von Daten
plot_categorical_variables(
  titanic_final,
  "Survived",
  "Sex",
  "Pclass"
)


