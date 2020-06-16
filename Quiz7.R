library(AER)
library(stargazer)
library(ggplot2)
library(erer)
library(dplyr)
library(foreign)
library(nnet)

cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

data(HMDA)
str(HMDA)

HMDA$Deny=as.numeric(HMDA$deny)-1


p1=glm(Deny~afam+pirat+hirat+lvrat+phist+selfemp, family=binomial(link="probit"), x=TRUE, data=HMDA)
l1=glm(Deny~afam+pirat+hirat+lvrat+phist+selfemp, family=binomial, x=TRUE, data=HMDA)

stargazer(p1,l1, se=list(NULL, NULL), 
          title="Probit/Logit- Mortage Rejection and Race", type="text", 
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, keep.stat = c("n","ll", "lr"))

fm3a=maBina(p1, x.mean=TRUE, rev.dum=TRUE, digits=3)
fm4a=maBina(l1, x.mean=TRUE, rev.dum=TRUE, digits=3)

stargazer(fm3a,fm4a, se=list(NULL, NULL), 
          title="Probit/Logit - Average Marginal Effects", type="text", 
          star.cutoffs = c(0.05, 0.01, 0.001), df=FALSE, digits=3, keep.stat = c("n","ll"))
