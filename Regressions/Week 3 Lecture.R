require(datasets)
data(swiss)
require(GGally)
require(ggplot2)
g = ggpairs(swiss, lower=list(continuous = "smooth"))

g

## calling 'lm'
## all of the estimated values show the impact on fertility if the other coefficients are constant

summary(lm(Fertility ~. , data=swiss))
summary(lm(Fertility ~.,data=swiss))$coefficients

## let's now just look at fertility and agriculture. Agriculture co-efficient has changed. 
## Other variables have acted as confounders 

summary(lm(Fertility ~ Agriculture, data=swiss))

## let's do an example to see why
n <- 100
x2 <- 1:n
x1 <- .01*x2 + runif(n,-.1,.1)
y= -x1 + x2 + rnorm(n, sd = .01)
summary (lm (y ~ x1))
summary (lm (y ~ x1 + x2))
dat= data.frame(y = y, x1 = x1, x2=x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))

g=ggplot(dat, aes(y=y, x=x1, colour=x2))
g= g + geom_point(colour = "grey50", size=5) + geom_smooth(method = lm, se = FALSE, colour= "black") + geom_point(size=4)
g

library(ggplot2)
g2 = ggplot(dat, aes(y = ey, ex1, colour =x2 ))
g2 = g2 + geom_point(colour="grey50", size=5) + geom_smooth(method=lm, se=FALSE, colour="black") + geom_point (size=4)
g2

library(datasets)
data(swiss)
head(swiss)
hist(swiss$Catholic)
library(dplyr)
swiss = mutate(swiss, CatholicBin=1 *(Catholic > 50))
head(swiss)
g3 = ggplot(swiss, aes(x=Agriculture, y= Fertility, colour=factor(CatholicBin)))
g3 = g3 + geom_point(size=6, colour="black") + geom_point(size=5)
g3 =g3 + xlab("% in Agriculture") + ylab("Fertility")
g3

## 2 scenarios 

#1 : B0 + B1X1 + B2X2 --> if x=0, then B0+B1X1, if x=1 B0+B2 + B1X1

#2 B0 + B1X1 + B2X2 + B3X1X2, if x=0, then B0+B1X1, if x=1, B0+B2 + B1X1 + B3X1, or (B0+B2) + (B1+B3)X1

## First fit model that doesn't include religion
head(swiss)
fit=lm(Fertility ~ Agriculture,data=swiss)
gnr=g3
gnr=gnr + geom_abline(intercept = coef(fit)[1],slope= coef(fit)[2],size=2)
gnr
       
## Now let's do it with two parallel lines 

fit=lm(Fertility ~ Agriculture + factor(CatholicBin), data=swiss)
summary(fit)$coef
gr=g3
gr=gr + geom_abline(intercept= coef(fit)[1],slope= coef(fit)[2],size=2)
gr=gr + geom_abline(intercept= coef(fit)[1]+coef(fit)[3],slope= coef(fit)[2],size=2)
gr

## Now let's do with with the different slopes 

fit=lm(Fertility ~ Agriculture * factor(CatholicBin),data=swiss)
summary(fit)$coef
gr1 =g3
gr1 = gr1 + geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],size=2)
gr1 = gr1 + geom_abline(intercept=coef(fit)[1] + coef(fit)[3],
                        slope=coef(fit)[2]+coef(fit)[4],size=2)
gr1