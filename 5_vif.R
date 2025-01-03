source("0_aux.R")

vif_threshold <- 13
data <- score
formula <- fmeanscore3

# RUN SPECIFICATION ANALYSIS
{
    cleaned <- detect_and_remove_collinear(data, formula, remove = FALSE)
    data <- cleaned$data
    formula <- cleaned$formula
    # Étape 1 : Ajuster un modèle initial
    model <- lm(formula, data = data)
    robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
    print("Original Specification")
    print(robust_se)
    # Étape 2 : Calculer les VIF
    vif_values <- vif(model)
    print("VIF Values:")
    print(vif_values)

    # Identifier les variables avec un VIF supérieur au seuil
    high_vif_vars <- names(vif_values[vif_values > vif_threshold])

    if (length(high_vif_vars) > 0) {
        print(paste("Variables with VIF >", vif_threshold, ":"))
        print(high_vif_vars)

        # Modifier la formule pour exclure les variables à VIF élevé
        new_formula <- as.formula(
            paste(
                as.character(formula[[2]]), "~",
                paste(setdiff(all.vars(formula[[3]]), high_vif_vars), collapse = " + ")
            )
        )

        print("New formula without high VIF variables:")
        print(new_formula)

        # Ajuster un modèle avec la nouvelle spécification
        new_model <- lm(new_formula, data = data)

        # Étape 3 : Régression avec erreurs robustes (clusterisées)
        robust_se <- coeftest(new_model, vcov = vcovCL, cluster = ~districtid)
        print("Regression results with reduced specification:")
        print(robust_se)
    } else {
        print("No variables exceeded the VIF threshold.")
        robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
        print("Regression results with original specification:")
        print(robust_se)
    }
}


source("0_aux.R")

vif_threshold <- 13
data <- score
formula <- fmeanscore3

# Get VIF values

{
    cleaned <- detect_and_remove_collinear(data, formula, remove = FALSE)
    data <- cleaned$data
    formula <- cleaned$formula

    # Étape 1 : Ajuster un modèle initial
    model <- lm(formula, data = data)
    robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
    print("Original Specification")
    print(robust_se)

    # Étape 2 : Calculer les VIF
    vif_values <- vif(model)
    print("VIF Values:")
    print(vif_values)
    # Diviser les VIF en 3 colonnes pour un affichage compact
    vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)
    vif_split <- split(vif_df, ceiling(seq_along(vif_df$Variable) / ceiling(nrow(vif_df) / 3)))

    # Trouver la longueur maximale des groupes
    max_rows <- max(sapply(vif_split, nrow))

    # Équilibrer les groupes en ajoutant des lignes NA si nécessaire
    vif_split <- lapply(vif_split, function(df) {
        if (nrow(df) < max_rows) {
            df[(nrow(df) + 1):max_rows, ] <- NA
        }
        df
    })

    # Fusionner les groupes en une seule data frame
    vif_compact <- do.call(cbind, lapply(vif_split, function(df) {
        data.frame(Variable = df$Variable, VIF = round(df$VIF, 2))
    }))

    # Transformer en tableau LaTeX
    vif_table_compact <- xtable(vif_compact,
        caption = "VIF Values for Regression Variables (Compact Format)",
        label = "tab:vif_values_compact"
    )
    print(vif_table_compact, type = "latex", file = "OUTPUT/TABLES/vif_values.tex", include.rownames = FALSE)
}

# Tracer et enregistrer la matrice de corrélation
{
    correlation_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")
    corr_melt <- melt(correlation_matrix)

    corr_plot <- ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        theme_minimal() +
        labs(title = "Correlation Matrix", x = "", y = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave("OUTPUT/MEDIA/correlation_matrix.png", corr_plot, width = 8, height = 7)

    # Identifier les variables avec un VIF supérieur au seuil
    high_vif_vars <- names(vif_values[vif_values > vif_threshold])

    if (length(high_vif_vars) > 0) {
        print(paste("Variables with VIF >", vif_threshold, ":"))
        print(high_vif_vars)

        # Modifier la formule pour exclure les variables à VIF élevé
        new_formula <- as.formula(
            paste(
                as.character(formula[[2]]), "~",
                paste(setdiff(all.vars(formula[[3]]), high_vif_vars), collapse = " + ")
            )
        )

        print("New formula without high VIF variables:")
        print(new_formula)

        # Ajuster un modèle avec la nouvelle spécification
        new_model <- lm(new_formula, data = data)

        # Étape 3 : Régression avec erreurs robustes (clusterisées)
        robust_se <- coeftest(new_model, vcov = vcovCL, cluster = ~districtid)
        print("Regression results with reduced specification:")
        print(robust_se)
    } else {
        print("No variables exceeded the VIF threshold.")
        robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
        print("Regression results with original specification:")
        print(robust_se)
    }
}
