##Test
##Code from Anas(corrected)
plot_categorical_variables <- function(data, var1, var2, var3) {
  library(ggplot2)
  
  ggplot(
    data,
    aes(
      x = .data[[var1]],
      fill = interaction(.data[[var2]], .data[[var3]])
    )
  ) +
    geom_bar(position = "dodge") +
    labs(
      title = "Visualisierung von drei kategorialen Variablen",
      x = var1,
      y = "Häufigkeit",
      fill = paste(var2, "×", var3)
    ) +
    theme_minimal()
}

plot_categorical_variables(
  titanic,
  "Survived",
  "Pclass",
  "SibSp"
)



##Alternative code
visualize_categorical <- function(data, vars) {
  if (length(vars) < 3 || length(vars) > 4)
    stop("Es werden 3 oder 4 Variablen benötigt")
  
  tab <- table(data[, vars])
  mosaicplot(tab, main = "Mosaikplot kategorialer Variablen")
}
visualize_categorical(
  data = titanic,
  vars = c("Survived", "SibSp", "Pclass")
)