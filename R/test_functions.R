# test_functions.r
# Skript zum Testen der Funktionen aus descriptive_stats.r

# 1. Umgebung vorbereiten
# ---------------------
cat("=== STARTE TESTS ===\n\n")

# Dummy-Daten erstellen (damit wir unabhängig von titanic.csv testen können)
dummy_data <- data.frame(
  Alter = c(22, 38, 26, 35, 35, NA, 54, 2, 27, 14),
  Klasse = factor(c(3, 1, 3, 1, 3, 3, 1, 3, 2, 2)),
  Geschlecht = c("male", "female", "female", "female", "male", "male", "male", "male", "female", "female"),
  Survived = factor(c(0, 1, 1, 1, 0, 0, 0, 0, 1, 1))
)

# Dateien laden (Pfade ggf. anpassen!)
# Wir gehen davon aus, dass helper_functions.r im gleichen Ordner liegt
if(file.exists("helper_functions.r") && file.exists("descriptive_stats.r")) {
  source("helper_functions.r")
  source("descriptive_stats.r")
  cat("[OK] Dateien geladen.\n")
} else {
  stop("Kritischer Fehler: helper_functions.r oder descriptive_stats.r nicht gefunden!")
}

# Hilfsfunktion für Fehlertests
expect_error <- function(expr) {
  tryCatch({
    expr
    cat("  [FAIL] Kein Fehler geworfen (Erwartet war ein Stop).\n")
  }, error = function(e) {
    cat("  [PASS] Fehler erfolgreich abgefangen: ", e$message, "\n")
  })
}

# 2. Tests durchführen
# --------------------

cat("\n--- Teste (i) stats_metric (Gregor) ---\n")
cat("Test 1: Normaler Durchlauf (Alter)...\n")
try(stats_metric(dummy_data$Alter))
cat("Test 2: Falscher Input (String)...\n")
expect_error(stats_metric("Hallo"))


cat("\n--- Teste (ii) stats_categorical (Daniel) ---\n")
cat("Test 1: Normaler Durchlauf (Klasse)...\n")
try(stats_categorical(dummy_data$Klasse))
cat("Test 2: Falscher Input (Zahl)...\n")
expect_error(stats_categorical(123))


cat("\n--- Teste (iii) bivariat_kategorial (Mirah) ---\n")
cat("Test 1: Normaler Durchlauf (Klasse vs. Survived)...\n")
try(print(bivariat_kategorial(dummy_data, "Klasse", "Survived")))
cat("Test 2: Spalte existiert nicht...\n")
expect_error(bivariat_kategorial(dummy_data, "Klasse", "GibtsNicht"))
cat("Test 3: Variable ist nicht kategorial (Alter)...\n")
expect_error(bivariat_kategorial(dummy_data, "Alter", "Survived"))


cat("\n--- Teste (iv) bivariat_metrisch_dichotom (Yi Wei) ---\n")
cat("Test 1: Normaler Durchlauf (Alter nach Survived)...\n")
# Plotfenster ggf. öffnen
try({
  res <- bivariat_metrisch_dichotom(dummy_data, "Alter", "Survived")
  print(res)
})
cat("Test 2: Metrische Variable ist eigentlich Text...\n")
expect_error(bivariat_metrisch_dichotom(dummy_data, "Geschlecht", "Survived"))


cat("\n--- Teste (v) plot_categorical_variables (Anas) ---\n")
cat("Test 1: Plot erstellen...\n")
try({
  p <- plot_categorical_variables(dummy_data, "Klasse", "Geschlecht", "Survived")
  print(p) # Muss explizit geprintet werden in Skripten
  cat("  [PASS] Plot-Objekt erstellt.\n")
})
cat("Test 2: Spaltenname falsch...\n")
expect_error(plot_categorical_variables(dummy_data, "Klasse", "Geschlecht", "Falsch"))

cat("\n=== TESTS BEENDET ===\n")

