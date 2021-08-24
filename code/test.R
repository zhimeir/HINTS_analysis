res <- interaction_mdl(data = data, type = "cts", feature = "GeneralHealth")
feature <- "GeneralHealth"

idxna <- which(data[[feature]] < 0)
sub_data <- data[-idxna, ]
mdl <- lm(GeneralHealth ~ BirthGender + Pandemic 
          + GenderXPandemic + AgeGrpB + BMIGrp 
          + as.factor(RaceEthn5) + as.factor(MaritalStatus) 
          + Education + IncomeRanges, sub_data, x=TRUE)
beta <- summary(mdl)$coef[c(2,4),1]
sigma <- summary(mdl)$sigma^2
X <- mdl$x
H <- solve(t(X)%*%X)
c <- rep(0,22)
c[c(2,4)] <- 1
t_stat <- (beta[1] + beta[2]) / sqrt(sigma * t(c) %*% H %*% c)
2*(1-pnorm(t_stat))
