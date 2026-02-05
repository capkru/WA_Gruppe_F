## Aufgabe 2(a)

source("R/helper_functions.r") # Hilfsfunktionen laden

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
  
  # Checkt dichotom
  if (length(unique(na.omit(data[[dichotom]]))) != 2)  stop("Fehler: dichotome Variable muss genau zwei Ausprägungen haben.")
  
  x <- data[[metrisch]]
  g <- as.factor(data[[dichotom]])
  
  boxplot(x ~ g,
          main = paste("Boxplot von", metrisch, "nach", dichotom),
          xlab = dichotom,
          ylab = metrisch,
          col = c("steelblue", "lightgreen"))
  
  tapply(x, g, mean, na.rm = TRUE)
}


## (v) - Anas
# Funktion um einen gruppierten Balkendiagramm für drei kategoriale Variablen zu erstellen
plot_categorical_variables <- function(data, var1, var2, var3) {
  library(ggplot2)
  
  # Checks
  if (!is.data.frame(data))
    stop("Fehler in plot_categorical_variables: 'data' muss ein Dataframe sein.")
  
  if (!all(c(var1, var2, var3) %in% names(data)))
    stop("Fehler in plot_categorical_variables: Variablen existieren nicht im Datensatz.")
  
  ggplot(
    data,
    aes(
      x = factor(.data[[var1]]),
      fill = interaction(.data[[var2]], .data[[var3]])
    )
  ) +
    geom_bar(position = "dodge") +
    labs(
      title = "Visualisierung von drei kategorialen Variablen",
      x = "Überlebensstatus",
      y = "Anzahl",
      fill = "Interaktion\n(Geschlecht · Klasse)"
    ) +
    scale_x_discrete(
      labels = c(
        "No"  = "Nicht überlebt",
        "Yes" = "Überlebt"
      )
    ) +
    scale_fill_manual(
      values = c(
        "male.3"   = "#E74C3C",
        "female.3" = "#F1C40F",
        "male.2"   = "#2ECC71",
        "female.2" = "#1ABC9C",
        "male.1"   = "#3498DB",
        "female.1" = "#9B59B6"
      ),
      labels = c(
        "male.3"   = "Männer, 3. Klasse",
        "female.3" = "Frauen, 3. Klasse",
        "male.2"   = "Männer, 2. Klasse",
        "female.2" = "Frauen, 2. Klasse",
        "male.1"   = "Männer, 1. Klasse",
        "female.1" = "Frauen, 1. Klasse"
      )
    ) +
    theme_minimal()
}


