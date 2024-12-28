source("0_aux.R")

manhalobis_plots_combined <- function(data, dependent_vars, main_var, control_vars_list, titles) {
  # Vérifier les dimensions des arguments
  if (length(dependent_vars) != length(control_vars_list) || 
      length(dependent_vars) != length(titles)) {
    stop("Les longueurs de 'dependent_vars', 'control_vars_list' et 'titles' doivent correspondre.")
  }
  
  # Liste pour stocker les plots
  plots <- list()
  
  # Définir les couleurs et l'ordre des catégories (cohérence)
  outlier_levels <- c("Normal", "Bad Leverage", "Vertical Outlier", "Good Leverage")
  outlier_colors <- c("Normal" = "black", "Bad Leverage" = "red", 
                      "Vertical Outlier" = "blue", "Good Leverage" = "green")
  
  # Boucle sur chaque combinaison de variables dépendantes, contrôles et titres
  for (i in seq_along(dependent_vars)) {
    dependent_var <- dependent_vars[i]
    control_vars <- control_vars_list[[i]]
    title <- titles[i]
    
    # Formule et préparation des données
    formula <- generate_formula(dependent_var, main_var, control_vars)
    data_subset <- data %>% select(c(dependent_var, main_var, all_of(control_vars)))
    cleaned <- detect_and_remove_collinear(data_subset, formula = formula, remove = FALSE)
    cleaned_data <- cleaned$data
    cleaned_formula <- cleaned$formula
    
    # Calcul des distances de Mahalanobis
    x_vars <- setdiff(names(cleaned_data), dependent_var)
    X <- as.matrix(cleaned_data[, x_vars])
    mahalanobis_dist <- mahalanobis(X, colMeans(X, na.rm = TRUE), cov(X, use = "complete.obs"))
    cleaned_data$Mahalanobis <- mahalanobis_dist
    
    # Résidus standardisés
    complete_cases <- complete.cases(cleaned_data)
    cleaned_data_complete <- cleaned_data[complete_cases, ]
    model <- lm(cleaned_formula, data = cleaned_data_complete)
    cleaned_data_complete$Standardized_Residuals <- rstandard(model)
    cleaned_data$Standardized_Residuals <- NA
    cleaned_data$Standardized_Residuals[complete_cases] <- cleaned_data_complete$Standardized_Residuals
    
    # Identification des outliers
    cleaned_data$Outlier_Type <- factor("Normal", levels = outlier_levels)
    cleaned_data$Outlier_Type[cleaned_data$Mahalanobis > qchisq(0.975, df = length(x_vars))] <- "Bad Leverage"
    cleaned_data$Outlier_Type[cleaned_data$Mahalanobis > qchisq(0.975, df = length(x_vars)) & abs(cleaned_data$Standardized_Residuals) > 2] <- "Vertical Outlier"
    cleaned_data$Outlier_Type[cleaned_data$Mahalanobis > qchisq(0.975, df = length(x_vars)) & abs(cleaned_data$Standardized_Residuals) <= 2] <- "Good Leverage"
        # Création du scatter plot
    p <- ggplot(cleaned_data, aes(x = Mahalanobis, y = Standardized_Residuals, color = Outlier_Type)) +
      geom_point() +
      labs(title = title,
           x = "Mahalanobis Distances",
           y = "Standardized Residuals") +
      theme_minimal() +
      scale_color_manual(values = outlier_colors, drop = FALSE)  # Fixer les couleurs et conserver les niveaux
    
    # Ajouter le plot à la liste
    plots[[i]] <- p
  }
  
  # Combiner les graphiques et forcer une seule légende
  combined_plot <- wrap_plots(plots, ncol = 3) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  # Afficher le graphique combiné
  print(combined_plot)
}

# Exemple d'utilisation
dependent_vars <- c("meanscore", "meanscore", "meanscore", "studentattendance", "studentattendance", "studentattendance")
control_vars_list <- list(
  basic_controls,
  c(basic_controls, additional_controls_2),
  c(basic_controls, additional_controls_2, additional_controls_5, additional_controls_6),
  basic_controls,
  c(basic_controls, additional_controls_2),
  c(basic_controls, additional_controls_2, additional_controls_5, additional_controls_6)
)
titles <- c(
  "Meanscore - Baseline",
  "Meanscore - GP population",
  "Meanscore - All controls",
  "Attendance - Baseline",
  "Attendance - GP population",
  "Attendance - All controls"
)

manhalobis_plots_combined(score, dependent_vars, "op", control_vars_list, titles)

ggsave("OUTPUT/MEDIA/manhalobis.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
