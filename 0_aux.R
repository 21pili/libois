#### Load packages ###
{library(haven)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(stargazer)
#Robust stuff
library(moments)
library(MASS)       # Pour LAD
library(quantreg)   # Pour LAD
library(robustbase) # Pour M-, S-, et MM-estimations
library(RobStatTM)
#VIF stuff
library(car) # Pour la fonction vif()
#Bootstrap stuff
library(boot)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(xtable)
library(reshape2)}

#### Code ####

# Functions
generate_formula <- function(dependent_var, main_var, control_vars) {
  as.formula(
    paste(
      dependent_var,
      "~", main_var,
      "+", paste(control_vars, collapse = " + ")
    )
  )
}

# Fonction pour détecter et retirer les colonnes parfaitement collinéaires
detect_and_remove_collinear <- function(data, formula, remove = FALSE) {

  if (remove) {
    data <- data %>% na.omit()
    message("Les lignes contenant des NA ont été supprimées.")
  }
  # Générer la matrice de conception à partir de la formule
  design_matrix <- model.matrix(formula, data = data)

  # Vérifier le rang de la matrice
  qr_decomp <- qr(design_matrix)
  rank <- qr_decomp$rank

  # Identifier les colonnes linéairement dépendantes
  if (ncol(design_matrix) > rank) {
    # Colonnes à exclure (colonnages QR, pivotement)
    collinear_cols <- setdiff(seq_len(ncol(design_matrix)), qr_decomp$pivot[1:rank])

    # Obtenir les noms des colonnes problématiques
    collinear_names <- colnames(design_matrix)[collinear_cols]

    # Retirer les colonnes problématiques du dataset
    cleaned_data <- data %>% dplyr::select(-all_of(collinear_names))

    # Mettre à jour la formule pour exclure les colonnes problématiques
    # Utilisation de reformulate pour une gestion plus robuste des formules
    vars <- all.vars(formula)
    new_vars <- setdiff(vars, collinear_names)
    cleaned_formula <- reformulate(termlabels = new_vars[-1], response = new_vars[1])

    # Afficher un message des colonnes retirées
    message("Colonnes retirées pour collinéarité parfaite : ", paste(collinear_names, collapse = ", "))

    return(list(data = cleaned_data, formula = cleaned_formula))
  } else {
    message("Aucune collinéarité parfaite détectée.")
    return(list(data = data, formula = formula))  # Pas de changement
  }
}


basic_controls <- c(
  "dms01", "dms02", "dms03", "dms04",
  "dms05", "dms06", "dms07", "dms08",
  "dms09", "dms10", "dms11", "dms12",
  "dms13", "dms14", "dms15", "dms16",
  "dms17", "dms18", "dms19", "dms20",
  "dms21", "dmaq3", "dmaq2", "dmaq1",
  "dmph4", "dmph5", "dmph6", "dmph7",
  "dmph8", "dmts1", "dmts2", "dmts3",
  "dmts4", "lon", "lat", "alt", "rain"
)

additional_controls_2 <- c("frsc", "frobc", "totpop", "density")
additional_controls_3 <- c(
  "elec", "phone", "gpdistroad", "lit",
  "tcaste", "tcaste2", "tgen", "edu", "toenr", "dissch",
  "lit", "index", "elec", "phone", "gpdistroad"
)
additional_controls_4 <- c("lit", "elec", "phone", "gpdistro")
additional_controls_5 <- c("lit", "elec", "phone", "gpdistroad")
additional_controls_6 <- c(
  "scaste", "scaste2", "sgen",
  "me2", "me3", "fe2", "fe3", "tepupr", "index"
)

### Score and attendance
# soil and climate as controls
fmeanscore1 <- generate_formula("meanscore", "op", basic_controls)
fmeanscore2 <- generate_formula(
  "meanscore",
  "op",
  c(basic_controls, additional_controls_2)
)
fmeanscore3 <- generate_formula(
  "meanscore",
  "op",
  c(
    basic_controls, additional_controls_2,
    additional_controls_5, additional_controls_6)
)


fattendance1 <- generate_formula("studentattendance", "op", basic_controls)
fattendance2 <- generate_formula(
  "studentattendance",
  "op",
  c(basic_controls, additional_controls_2)
)
fattendance3 <- generate_formula(
  "studentattendance",
  "op",
  c(
    basic_controls, additional_controls_2,
    additional_controls_5, additional_controls_6)
)

# Chargement et transformation des données
score <- read_dta("Datas/table-5-student-score-attendance.dta") %>%
  as.data.frame() %>%  # Convertir en data frame (optionnel si déjà fait par read_dta)
  mutate(across(where(is.character), ~ as.numeric(.))) %>%
  select(-studentno)