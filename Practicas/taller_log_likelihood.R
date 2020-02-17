library(nlme)
head(Oxboys)

fit <- lme(height ~ age, random= ~ 1 | Subject, data=Oxboys, method="REML")
fit2 <- gls(height ~ age, data=Oxboys, method="REML")

anova(fit, fit2)

est <- -2*(logLik(fit2) - logLik(fit))

pchisq(est, 1, lower.tail = T)


library(RLRsim)
exactRLRT(fit, nsim=1000)


pseudo_gen_dat <- function(beta0, beta1, sigmay) {
  group <- Oxboys$Subject
  x <- Oxboys$age  # Aqui la diferencia ;)
  mu <- beta0 + beta1 * x
  y <- rnorm(n=length(x), mean=mu, sd=sigmay)
  data.frame(group=group, x=x, y=y)
}

k <- 500
lrts <- numeric(k)
for (i in 1:k) {
  pseudo_datos <- pseudo_gen_dat(beta0=149.37180, beta1=6.52102, 
                                 sigma=8.080536)
  m1 <- gls(y ~ x, data=pseudo_datos, method="REML")
  m2 <- lme(y ~ x, random = ~ 1| group, data=pseudo_datos, method="REML")
  lrts[i] <- -2 * (logLik(m1) - logLik(m2))
}

plot(density(lrts), main='Densidad empírica de los lrts')
acumulada <- ecdf(x=lrts) # F(x) para los valores LRT
1 - acumulada(3.04712)    # Valor-P
