library(wooldridge)
library(stargazer)
library(sandwich)
library(car)
library(doBy)
library(ggplot2)
library(ivpack)

cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}
ivse = function(reg) {
  rob = robust.se(reg)[,2]
  return(rob)
}

data(fish)
str(fish)


fit0 <- lm(ltotqty~lavgprc+mon+tues+wed+thurs, data=fish)
stargazer(fit0, se=list(cse(fit0)), 
          title="Effect on Log Total Qty", type="text",
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, omit.stat=c("adj.rsq"))

fit1 = lm(ltotqty~wave2+wave3+mon+tues+wed+thurs, data=fish)
stargazer(fit0, fit1, se=list(cse(fit0), cse(fit1)), 
          title="Effect on Log Total Qty", type="text",
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, omit.stat=c("adj.rsq"))

fitIV = ivreg(ltotqty~lavgprc+mon+tues+wed+thurs | wave2+wave3+mon+tues+wed+thurs, data=fish)
stargazer(fit0, fit1, fitIV, se=list(cse(fit0), cse(fit1), ivse(fitIV)), 
          title="Effect on Log Total Qty", type="text",
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, omit.stat=c("adj.rsq"))

summary(fitIV, vcov = sandwich, diagnostics = TRUE)
