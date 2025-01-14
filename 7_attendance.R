source("0_aux.R")

### Step 1 : Fit OLS


ols_1 <- lm(fattendance1, data = score)
ols_1_test <- coeftest(ols_1, vcov = vcovCL, cluster = ~districtid)

ols_2 <- lm(fattendance2, data = score)
ols_2_test <- coeftest(ols_2, vcov = vcovCL, cluster = ~districtid)

ols_3 <- lm(fattendance3, data = score)
ols_3_test <- coeftest(ols_3, vcov = vcovCL, cluster = ~districtid)



### Step 2 : Fit MM

cleaned <- detect_and_remove_collinear(score, fattendance1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
mm_1 <- lmrob(f, data = d, method = "MM", control = lmrob.control(k.max = 500))
summary(mm_1)
mm_1_test <- coef_test(mm_1, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fattendance2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
mm_2 <- lmrob(f, data = d, method = "MM", control = lmrob.control(k.max = 500))
summary(mm_2)
mm_2_test <- coef_test(mm_2, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fattendance3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "MM", control = lmrob.control(k.max = 500))
summary(model)
mm_3 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

### Step 3 : Fit S-estimation

cleaned <- detect_and_remove_collinear(score, fattendance1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, control = lmrob.control(k.max = 500))
summary(model)
s_1 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fattendance2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, control = lmrob.control(k.max = 500))
summary(model)
s_2 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fattendance3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, control = lmrob.control(k.max = 500))
summary(model)
s_3 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

### Step 3 : Fit M-estimation


cleaned <- detect_and_remove_collinear(score, fattendance1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "M", init = "S")
m_1 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fattendance2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "M", init = "S")
m_2 <- coef_test(model, vcov = "CR2", cluster = d$districtid)

cleaned <- detect_and_remove_collinear(score, fattendance3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrob(f, data = d, method = "M", init = "S", control = lmrob.control(k.max = 1000))
m_3 <- coef_test(model, vcov = "CR2", cluster = d$districtid)



### Step 4 : Fit Redescending M-estimation
cleaned <- detect_and_remove_collinear(score, fattendance1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrobM(f, data = d)
summary(model)

### Step 4 : Fit Redescending M-estimation
cleaned <- detect_and_remove_collinear(score, fattendance2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrobM(f, data = d)
summary(model)

### Step 4 : Fit Redescending M-estimation
cleaned <- detect_and_remove_collinear(score, fattendance3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- lmrobM(f, data = d)
summary(model)


### Step 5 : Fit LAD

cleaned <- detect_and_remove_collinear(score, fattendance1, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- rq(f, data = d, tau = 0.5)
lad_1 <- coef(model)

cleaned <- detect_and_remove_collinear(score, fattendance2, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- rq(f, data = d, tau = 0.5)
lad_2 <- coef(model)

cleaned <- detect_and_remove_collinear(score, fattendance3, remove = FALSE)
f <- cleaned$formula
d <- cleaned$data
model <- rq(f, data = d, tau = 0.5)
lad_3 <- coef(model)
