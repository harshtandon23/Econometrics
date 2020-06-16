library(wooldridge)
library(stargazer)
library(sandwich)
library(car)

data(hprice2)
str(hprice2)

cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}
fit0 <- lm(price~nox+stratio+rooms+dist, data=hprice2)
fit1 <- lm(price~nox+stratio+rooms+I(rooms**2)+dist, data=hprice2)
fit2 <- lm(log(price)~nox+stratio+rooms+dist, data=hprice2)
fit3 <- lm(log(price)~log(nox)+stratio+rooms+log(dist), data=hprice2)
fit4 <- lm(log(price)~log(nox)+stratio+rooms+log(dist)+(log(dist)*log(nox)), data=hprice2)



stargazer(fit1, fit2, fit3, fit4, se=list(cse(fit1), cse(fit2), cse(fit3), cse(fit4)), 
          title="Effect of pollution on House Prices", type="text",
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, omit.stat=c("adj.rsq"))


stargazer(fit0, se=list(cse(fit0)), 
          title="Effect of pollution on House Prices", type="text",
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, omit.stat=c("adj.rsq"))
 