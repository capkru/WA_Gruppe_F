## Aufgabe 2(a)

source("helper_functions.r") # Hilfsfunktionen laden

## (i) - Gregor
stats_metric <- function(x) {
  # Vor der Berechnung prüfen, ob die Variable numerisch ist
  if (!is.numeric(x)) {
    stop("Fehler in stats_metric: Input muss numerisch sein.")
  }
  
  cat("Lageparameter:\n") 
  print(berechne_lage(x))

  cat("\nStreuungsmaße:\n")
  print(berechne_streuung(x))
        
  cat("\nVerteilungsmaße:\n")
  print(berechne_verteilung(x))

  cat("\nMin und Max:\n")
  print(berechne_extrema(x))
}


## (ii) - Daniel
stats_categorical <- function(x) {
  # Checkt ob die Daten in der richtigen Form sind
  if(!is.factor(x) && !is.character(x)) stop("Fehler in stats_categorical: 
                                             Input muss ein Faktor oder 
                                             Character sein.")
  
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
bivariat_kategorial <- function(data, var1, var2){
  # Checkt ob data ein Dataframe ist
  if (!is.data.frame(data)) stop("Fehler in bivariat_kategorial: 
                                 data muss ein Dataframe sein.")
  
  # Checkt ob var1 und var2 Strings sind
  if (!is.character(var1) || !is.character(var2)) stop("Fehler in 
                                                       bivariat_kategorial: 
                                                       var1 und var2 müssen
                                                       Strings sein.")
  
  # Checkt ob die Spalten im Datensatz existieren
  if (!all(c(var1, var2) %in% names(data))) stop("Fehler in bivariat_kategorial: 
                                                 Eine der Variablen existiert 
                                                 nicht im Datensatz.")
  # Checkt ob die Spalten an sich kategorial sind
  if (!is.factor(data[[var1]]) && !is.character(data[[var1]])) stop(paste(
    "Fehler in bivariat_kategorial:", var1, "ist nicht kategorial."))
  if (!is.factor(data[[var2]]) && !is.character(data[[var2]])) stop(paste(
    "Fehler in bivariat_kategorial:", var2, "ist nicht kategorial."))
  
  tab <- table(data[[var1]], data[[var2]])
  
  list(
    Kontingenztabelle = tab,
    Relative_Haefigkeiten = prop.table(tab)
  )
}

## (iv) - Yi Wei
## Metrisch x dichotom
bivariat_metrisch_dichotom <- function(data, metrisch, dichotom) {
  # Checkt ob data ein Dataframe ist
  if (!is.data.frame(data)) stop("Fehler in bivariat_metrisch_dichotom: 
                                 'data' muss ein Dataframe sein.")
  # Checkt ob die Variable im Datensatz ist
  if (!all(c(metrisch, dichotom) %in% names(data))) stop("Fehler in bivariat_metrisch_dichotom: 
                                                         Variablen nicht im Datensatz gefunden.")
  
  # Checkt den Datentyp der Spalte
  if (!is.numeric(data[[metrisch]])) stop(paste("Fehler bivariat_metrisch_dichotom:", 
                                                metrisch, "muss numerisch sein."))
  
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
  
  # Checkt ob data ein Dataframe ist
  if (!is.data.frame(data)) stop("Fehler in plot_categorical_variables: 
                                 'data' muss ein Dataframe sein.")
  
  # Checkt ob die Variable im Datensatz existiert
  if (!all(c(var1, var2, var3) %in% names(data))) stop("Fehler in plot_categorical_variables: 
                                                       Variablen existieren nicht im Datensatz.")
  
  # einen Balkendiagramm erstellen
  ggplot(data, aes(x = factor(var1), fill = interaction(var2, var3))) +
    geom_bar(position = "dodge") +
    labs(title = "kategorialen Variablene Visualization", 
         x = var1, 
         y = "Count", 
         fill = "Interaktion zwischen Variabeln") +
    theme_minimal()
}
