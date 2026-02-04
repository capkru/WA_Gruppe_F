# Analyse des Titanic-Datensatzes (WA_Gruppe_F)

Gruppenarbeit im Modul "Wissenchaftliches Arbeiten" (Wintersemester 25/26).
Ziel dieses Projekts ist die kollaborative Analyse des Titanic-Passagier-Datensatzes unter Verwendung von R, Git und GitHub.

## Autoren
* **Nur Amirah Khan Binti Rozlan**
* **Yi Wei Er**
* **Grigorii Iakovlev**
* **Daniel Martel**
* **Anas Salaheldin**

## Dateistruktur

### `data/`
Hier liegen die Datensätze für das Projekt.
* `titanic.csv`: Der originale Rohdatensatz (Quelle: Moodle).
* `titanic_cleaned.csv`: Der bereinigte Datensatz ohne fehlende Werte (Ergebnis der Vorverarbeitung).

### `R/`
Dieser Ordner enthält den gesamten R-Code, unterteilt nach Funktionalität:
* `data.r`: **Vorverarbeitung (Teilaufgabe 1)**. Liest die Rohdaten ein, extrahiert Titel aus Namen, imputiert fehlende Alterswerte basierend auf der Anrede und erstellt neue Variablen (z.B. Deck, Seite).
* `descriptive_stats.r`: **Statistik-Funktionen (Teilaufgabe 2a)**. Enthält selbstgeschriebene Funktionen für univariates und bivariates Analysieren (metrisch & kategorial) sowie eine Visualisierungsfunktion für 3-4 kategoriale Variablen.
* `helper_functions.r`: **Helfer-Funktionen (Teilaufgabe 2b)**. Interne Funktionen, die `descriptive_stats.r` unterstützen, aber nicht direkt für die Analyse aufgerufen werden.
* `analysis.r`: **Hauptanalyse (Teilaufgabe 4)**. Lädt die bereinigten Daten und wendet die Funktionen aus `descriptive_stats.r` an, um die Kennzahlen und Grafiken für den Bericht zu erzeugen.

### `report/`
Hier befindet sich die schriftliche Ausarbeitung der Ergebnisse.
* `Bericht.tex`: Der LaTeX-Quellcode unseres Berichts.
* `Bericht.pdf`: Das fertig kompilierte Dokument mit Interpretation der Analysen.
