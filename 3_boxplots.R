source("0_aux.R")

plot_combined_boxplots <- function(df) {
  # Vérifier si le dataframe est vide
  if (ncol(df) == 0) {
    stop("Le dataframe est vide.")
  }
  
  # Vérifier si les colonnes sont numériques
  numeric_cols <- sapply(df, is.numeric)
  if (!any(numeric_cols)) {
    stop("Le dataframe ne contient aucune variable numérique.")
  }
  
  # Filtrer pour conserver uniquement les colonnes numériques
  df <- df[, numeric_cols, drop = FALSE]
  
  # Déterminer la grille pour afficher tous les graphiques
  n_vars <- ncol(df)
  n_rows <- ceiling(n_vars / 2)  # Deux colonnes de boxplots côte à côte
  
  # Configurer l'espace pour plusieurs graphiques
  op <- par(mfrow = c(n_rows, 2), mar = c(4, 4, 2, 1))
  
  # Boucle sur chaque colonne pour tracer les deux boxplots
  for (var_name in names(df)) {
    data <- df[[var_name]]
    # Créer une nouvelle fenêtre graphique pour chaque variable (à commenter pour afficher tous les boxplots dans un seul graphique)
    # par(mfrow = c(1, 2))
    
    # Boxplot original
    boxplot(data, main = paste(var_name, "- Original"), col = "lightblue")
    # Boxplot ajusté
    adjbox(data, main = paste(var_name, "- Adjusted"), col = "lightgreen")
  }
  
  # Restaurer les paramètres graphiques par défaut
  par(op)
}
# Generate all the original and adjusted boxplots
plot_combined_boxplots(score %>% select(totpop))
plot_combined_boxplots(score %>% select(alt))
plot_combined_boxplots(score %>% select(gpdistroad))
plot_combined_boxplots(score %>% select(lit))
plot_combined_boxplots(score %>% select(frsc))
plot_combined_boxplots(score %>% select(frobc))
plot_combined_boxplots(score %>% select(density))
plot_combined_boxplots(score %>% select(tepupr))
plot_combined_boxplots(score %>% select(lon))
plot_combined_boxplots(score %>% select(lat))
plot_combined_boxplots(score %>% select(rain))
plot_combined_boxplots(score %>% select(studentattendance))
plot_combined_boxplots(score %>% select(meanscore))
