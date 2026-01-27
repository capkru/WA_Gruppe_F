## Aufgabe 2(b)

## Hilfsfunktionen zu (i)

# Lageparamaße berechnen
berechne_lage <- function(x) {
  x <- na.omit(x) # NAs entfernen
  antwort <- list(
    Mittelwert = mean(x),
    Median = median(x),
    Modus = berechne_modus(x),
    Quartile = quantile(x, probs = c(0.25, 0.5, 0.75))
  )
  return(antwort)
}

# Streuungsmaße berechnen
berechne_streuung <- function(x) {
  x <- na.omit(x)
  antwort <- list(
    Standardabweichung = sd(x),
    Varianz = var(x),
    Spannweite = diff(range(x)),
    IQR = IQR(x), # Interquartilsabstand
    Variationskoeffizient = sd(x) / mean(x)
  )
  return(antwort)
}

# Modus berechnen
berechne_modus <- function(x) {
  ux <- unique(x)
  antwort <- ux[which.max(tabulate(match(x, ux)))]
  return(antwort)
}

# Verteilungsmaße berechnen
berechne_verteilung <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  antwort <- list(
    Schiefe = sum((x - m)^3) / (n * s^3),
    Kurtosis = sum((x - m)^4) / (n * s^4) - 3
  )
  return(antwort)
}

# Min und Max berechnen
berechne_extrema <- function(x) {
  x <- na.omit(x)
  antwort <- list(
    Minimum = min(x),
    Maximum = max(x)
  )
  return(antwort)
}
