## Aufgabe 2(a)

source("helper_functions.r") # Hilfsfunktionen laden

## (i) - Gregor
stats_metric <- function(x) {
    # Vor der Berechnung prüfen, ob die Variable numerisch ist
    if (is.numeric(x)) { 
        cat("Lageparameter:\n") 
        print(berechne_lage(x))

        cat("\nStreuungsmaße:\n")
        print(berechne_streuung(x))
        
        cat("\nVerteilungsmaße:\n")
        print(berechne_verteilung(x))

        cat("\nMin und Max:\n")
        print(berechne_extrema(x))
    }
}


## (ii) - Daniel
stats_categorical <- function(x) {
  # Checkt ob die Daten in der richtigen Form sind
  if(!is.factor(x) && !is.character(x)) stop("Kein Kategorialer Parameter")
  
  # Gibt die absolute Häufigkeit der Variable aus
  cat("Absolute Häufigkeit:\n")
  print(table(x))
  
  # Gibt die relative Häufigkeit aus
  cat("\nRelative Häufigkeit:\n")
  print(prop.table(table(x)))
  
  # Gibt Ergebnisse als Lsite zurück
  freq  <- table(x)
  prop  <- prop.table(freq)
  return(list(freq = freq, prop = prop))
}

## (iii) - Mirah


## (iv) - Yi Wei
bivariat_metrisch_dichotom <- function(data, metrisch, dichotom) {
  x <- data[[metrisch]]
  g <- as.factor(data[[dichotom]])
  
  boxplot(x ~ g,
          main = paste(metrisch, "nach", dichotom),
          xlab = dichotom,
          ylab = metrisch)
  
  tapply(x, g, mean, na.rm = TRUE)
}


## (v) - Anas
# Funktion um einen gruppierten Balkendiagramm für drei kategoriale Variablen zu erstellen
plot_categorical_variables <- function(data, var1, var2, var3) {
  library(ggplot2)
  
  # einen Balkendiagramm erstellen
  ggplot(data, aes(x = factor(var1), fill = interaction(var2, var3))) +
    geom_bar(position = "dodge") +
    labs(title = "kategorialen Variablene Visualization", 
         x = var1, 
         y = "Count", 
         fill = "Interaktion zwischen Variabeln") +
    theme_minimal()
}
