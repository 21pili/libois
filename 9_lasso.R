
score <- as.data.frame(read_dta("Datas/table-5-student-score-attendance.dta")) %>% na.omit()
controls <- c("op", basic_controls)
y <- score$meanscore
X <- as.matrix(score[, controls])
# Création d'un modèle LASSO
lasso_model <- glmnet(x = X, y = y, alpha = 1)  # alpha = 1 pour LASSO

# Affichage du chemin de régularisation
plot(lasso_model)

# Sélection du meilleur modèle (par exemple, en utilisant la validation croisée)
cv_lasso <- cv.glmnet(x = X, y = y, alpha = 1)
best_lambda <- cv_lasso$lambda.min

# Modèle final avec le meilleur lambda
final_model <- glmnet(x = X, y = y, alpha = 1, lambda = best_lambda)
coef(final_model)
