## Aufgabe 2(a)

source("helper_functions.r") # Hilfsfunktionen laden

## (i) - Gregor
stats_metric <- function(x) {
    if (is.numeric(x)) {
        cat("Lageparameter:\n") # nolint
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
  if(!is.factor(x) && !is.character(x)) stop("Kein Kategorialer Parameter")
  # Checkt ob die Daten in der richtigen Form sind
  
  
  
}

## (iii) - Mirah


## (iv) - Yi Wei


## (v) - Anas

