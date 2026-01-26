## Aufgabe 2(b)

## Hilfsfunktionen zu (i)

# Lageparamaße berechnen
berechneLage <- function(x) {
  x <- na.omit(x) # NAs entfernen
  list(
    Mittelwert = mean(x),
    Median = median(x),
    Modus = berechneModus(x),
    Quartile = quantile(x, probs = c(0.25, 0.5, 0.75))
  )
}

# Streuungsmaße berechnen
berechne_streuung <- function(x) {
  x <- na.omit(x)
  list(
    Standardabweichung = sd(x),
    Varianz = var(x),
    Spannweite = diff(range(x)),
    IQR = IQR(x), # Interquartilsabstand
    Variationskoeffizient = sd(x) / mean(x)
  )
}

# Modus berechnen
berechne_modus <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Verteilungsmaße berechnen
berechne_verteilung <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  list(
    Schiefe = sum((x - m)^3) / (n * s^3),
    Kurtosis = sum((x - m)^4) / (n * s^4) - 3
  )
}

# Min und Max berechnen
berechne_extrema <- function(x) {
  x <- na.omit(x)
  list(
    Minimum = min(x),
    Maximum = max(x)
  )
}
