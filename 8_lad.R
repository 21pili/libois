source("O_aux.R")

# Fonction pour effectuer la régression quantile et extraire les coefficients
rq_coef <- function(data, indices) {
  d <- data[indices, ]
  model <- rq(formula, data = d, tau = 0.5) # Régression médiane
  return(coef(model))
}

reg_lad_clustered <- function(formula, data) {
  # Créer un objet boot
  boot_result <- boot(data = data, statistic = rq_coef, R = 1000, strata = data[, districtid]) # R = nombre de réplications bootstrap

  # Calculer les erreurs standards
  boot_se <- apply(boot_result$t, 2, sd)

  # Obtenir le résumé du modèle original (pour les coefficients)
  original_model <- rq(formula, data = data, tau = 0.5)
  original_summary <- summary(original_model)

  # Créer un tableau avec les résultats
    results_table <- data.frame(
    Estimate = coef(original_model),
    Std.Error = boot_se,
    t.value = coef(original_model) / boot_se,
    p.value = 2 * pt(abs(coef(original_model) / boot_se), df=nrow(data)-length(coef(original_model)), lower.tail = FALSE) #Calcul p-value
  )
    rownames(results_table) <- names(coef(original_model))

  return(results_table)
}

#Avec gestion de la colinéarité
reg_lad_clustered_collinear <- function(formula, data){
    result <- detect_and_remove_collinear(data, formula, TRUE) #Verbose activé
    cleaned_table <- result$data
    cleaned_formula <- result$formula
    return(reg_lad_clustered(cleaned_formula, cleaned_table))
}

reg_lad_clustered_collinear(fgsmeet1, table4)