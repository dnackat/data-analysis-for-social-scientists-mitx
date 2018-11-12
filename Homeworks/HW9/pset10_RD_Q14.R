#Parametric Regression
matrix_coef <- matrix(NA, nrow = 2, ncol = 11)

model <- lm(myoutcomenext ~ above, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 1] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 1] <- pvalue$coefficients[2, 4]

model <- lm(myoutcomenext ~ above + X1, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 2] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 2] <- pvalue$coefficients[2, 4]

model <- lm(myoutcomenext ~ above + X1 + X4, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 3] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 3] <- pvalue$coefficients[2, 4]

model <- lm(myoutcomenext ~ above + X1 + X2, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 4] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 4] <- pvalue$coefficients[2, 4]

model <- lm(myoutcomenext ~ above + X1 + X2 + X4 + X5, data = indiv, 
            subset = abs(difshare) <= 0.5)
matrix_coef[1, 5] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 5] <- pvalue$coefficients[2, 4]

model <- lm(myoutcomenext ~ above + X1 + X2 + X3, data = indiv, 
            subset = abs(difshare) <= 0.5)
matrix_coef[1, 6] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 6] <- pvalue$coefficients[2, 4]

model <- lm(myoutcomenext ~ above + X1 + X2 + X3 + X4 + X5 + X6, data = indiv, 
            subset = abs(difshare) <= 0.5)
matrix_coef[1, 7] <- model$coefficients[2]
pvalue <- summary(model)
matrix_coef[2, 7] <- pvalue$coefficients[2, 4]

matrix_coef