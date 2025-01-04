source("0_aux.R")

### Step 1 : Fit OLS


ols_1 <- lm(fmeanscore1, data = score)
ols_1_test <- coeftest(ols_1, vcov = vcovCL, cluster = ~districtid)

ols_2 <- lm(fmeanscore2, data = score)
ols_2_test <- coeftest(ols_2, vcov = vcovCL, cluster = ~districtid)

ols_3 <- lm(fmeanscore3, data = score)
ols_3_test <- coeftest(ols_3, vcov = vcovCL, cluster = ~districtid)



### Step 2 : Fit MM

cleaned <- detect_and_remove_collinear(score, fmeanscore1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
mm_1 <- lmrob(f, data = d, method = "MM", control = lmrob.control(k.max = 500))
print(summary(mm_1))
mm_1_test <- coef_test(mm_1, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fmeanscore2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
mm_2 <- lmrob(f, data = d, method = "MM", control = lmrob.control(k.max = 500))
print(summary(mm_2))
mm_2_test <- coef_test(mm_2, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fmeanscore3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "MM", control = lmrob.control(k.max = 500))
print(summary(model))
mm_3 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

### Step 3 : Fit S-estimation

cleaned <- detect_and_remove_collinear(score, fmeanscore1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, control = lmrob.control(k.max = 500))
print(summary(model))
s_1 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fmeanscore2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, control = lmrob.control(k.max = 500))
print(summary(model))
s_2 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fmeanscore3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, control = lmrob.control(k.max = 500))
print(summary(model))
s_3 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

### Step 3 : Fit M-estimation


cleaned <- detect_and_remove_collinear(score, fmeanscore1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "M", init = "S")
summary(model)
m_1 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fmeanscore2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "M", init = "S")
summary(model)
m_2 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fmeanscore3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "M", init = "S", control = lmrob.control(k.max = 1000))
summary(model)
m_3 <- coef_test(model, vcov = "CR2", cluster = d$districtid)



### Step 4 : Fit Redescending M-estimation
cleaned <- detect_and_remove_collinear(score, fmeanscore1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrobM(f, data = d)
print(summary(model))

cleaned <- detect_and_remove_collinear(score, fmeanscore2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrobM(f, data = d)
print(summary(model))


cleaned <- detect_and_remove_collinear(score, fmeanscore3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrobM(f, data = d)
print(summary(model))

### Step 5 : Fit LAD

cleaned <- detect_and_remove_collinear(score, fmeanscore1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- rq(f, data = d, tau = 0.5)
lad_1 <- coef(model)

cleaned <- detect_and_remove_collinear(score, fmeanscore2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- rq(f, data = d, tau = 0.5)
lad_2 <- coef(model)

cleaned <- detect_and_remove_collinear(score, fmeanscore3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- rq(f, data = d, tau = 0.5)
lad_3 <- coef(model)

### Tracer les scatter plots

# Extraire les coefficients pour chaque modèle
op_coefficients <- list(
  ols_op = ols_3_test["op", 1],
  m_op = m_3["op", 2],
  rm_op = rm_3["op"],
  lad_op = lad_3["op"]
)

intercepts <- list(
  ols_i = ols_3_test["(Intercept)", 1],
  m_i = m_3["(Intercept)", 2],
  rm_i = rm_3["(Intercept)"],
  lad_i = lad_3["(Intercept)"]
)


# Sélectionner uniquement les modèles de type 3

# Générer le tableau avec stargazer
stargazer(
  models_type3, 
  type = "latex",
  title = "Robust Meanscore Regressions",
  label = "meanscore", 
  omit = c("dms", "dmph", "dmts", "lat", "lon", "alt", "rain", "frsc", "frobc", "totpop",
           "density", "dmaq3", "dmaq2", "dmaq1", "elec", "phone", "gpdistro", 
           "gpdistroad", "lit", "Constant", "tcaste", "tcaste2", "tgen", "edu", "toenr", "dissch", "index",
           "scaste", "sgen", "me2", "me3", "fe2", "fe3", "tepupr", "(Intercept)"), 
  covariate.labels = c("op"),  # Afficher uniquement le coefficient de "op"
  column.sep.width = "5pt",    # Réduction des espaces horizontaux entre colonnes
  font.size = "scriptsize",
  omit.stat = c("f", "ser", "adj.rsq", "rsq"),
  notes = "Robust errors at the district level.",
  digits = 2,
  out = "OUTPUT/TABLES/meanscore_regression.tex"
)
