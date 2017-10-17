### Design and Sample Size Demonstration
## Simulating the difference between effect size and sample size in a power analysis


### The power of Sunny Delight
## Kids who drink sunny d at a young age are more likely to be happier adults
## In our example, if a child drinks 1 bottle of sunny d a week at the age of 5, 
# then the amount of dopamine in the brain increases in adulthood. 
## The average adult has 45+/-5 ng/ml of dopamine, but kids who drink sunny d 
# increase that level to 55 ng/ml 
## The first study tests 1000 kids

library(arm)
library(ggplot2)
library(gridExtra)

## What is the effect size in this example?
ES = (55-45)/5 ## ES = 2
SE = 5/(sqrt(1000)) ## SE = 0.16
## What about when we double the effect size?
ES = (90-45)/5 ## ES = 9
SE = 5/(sqrt(1000)) ## SE stays the same
## How about when we double the sample size?
ES = (55-45)/5 ## ES is 2 like before but... 
SE = 5/(sqrt(2000)) ## SE decreases slightly - SE = 0.11

### Andrew states that " it is generally better to double the effect size [theta] than to double the sample size [n]"
## here we can see that doubling the sample size decreases the standard error at a slower rate than doubling the effect size
# The SE: effect size influences the numerator, sample size influences the denominator     
    
nsunny = 2
rep = 500
ntot = nsunny*rep
sunny = gl(nsunny, rep, length=ntot)

sunnydiff = 10
sunnydiff.sd = 0
suns<-rnorm(ntot, 55, 5)

base <- 45
child <- base + suns-mean(suns)
mm <- model.matrix(~(sunny)^2, data.frame=(sunny))

fake <- vector()
for (i in 1:length(ntot)){
  coeff <- c(child[i], 
             rnorm(1, sunnydiff, sunnydiff.sd)
  )
  
  dp <- rnorm(n = length(sunny), mean = mm %*% coeff, sd = 5)
  
  fake <- data.frame(dp=dp, sunny=sunny)  
}

hist(fake$dp[sunny==1])
hist(fake$dp[sunny==2])        
mean(fake$dp) # 50.5
sd(fake$dp) # 7.03
display(lm(dp~sunny, data=fake))
#lm(formula = dp ~ sunny, data = fake)
#coef.est coef.se
#(Intercept) 45.52     0.22  
#sunny2      10.02     0.31  
#---
#  n = 1000, k = 2
#residual sd = 4.94, R-Squared = 0.51


### Alright, now let's double the effect size...
### Sunny D increases the level of dopamine to 90 ng/ml - we'll keep the sample size the same
nsunny.e = 2
rep.e = 500
ntot.e = nsunny.e*rep.e
sunny.e = gl(nsunny.e, rep.e, length=ntot.e)

sunnydiff.e = 45
sunnydiff.sd.e = 0
suns.e<-rnorm(ntot.e, 90, 5)

base.e <- 45
child.e <- base.e + suns.e-mean(suns.e)
mm.e <- model.matrix(~(sunny.e)^2, data.frame=(sunny.e))

fake.e <- vector()
for (i in 1:length(ntot.e)){
  coeff.e <- c(child.e[i], 
             rnorm(1, sunnydiff.e, sunnydiff.sd.e)
  )
  
  dp.e <- rnorm(n = length(sunny.e), mean = mm.e %*% coeff.e, sd = 5)
  
  fake.e <- data.frame(dp.e=dp.e, sunny.e=sunny.e)  
}
hist(fake.e$dp.e[sunny.e==1])
hist(fake.e$dp.e[sunny.e==2])
mean(fake.e$dp.e) # 62.9
sd(fake.e$dp.e) # 23.0
display(lm(dp.e~sunny.e, data=fake.e))
#lm(formula = dp.e ~ sunny.e, data = fake.e)
#coef.est coef.se
#(Intercept) 40.48     0.22  
#sunny.e2    44.76     0.31  
#---
#  n = 1000, k = 2
#residual sd = 4.97, R-Squared = 0.95


## And now, we double the sample size...
nsunny.s = 2
rep.s = 1000
ntot.s = nsunny.s*rep.s
sunny.s = gl(nsunny.s, rep.s, length=ntot.s)

sunnydiff.s = 10
sunnydiff.sd.s = 0
suns.s<-rnorm(ntot.s, 55, 5)

base.s <- 45
child.s <- base.s + suns.s-mean(suns.s)
mm.s <- model.matrix(~(sunny.s)^2, data.frame=(sunny.s))

fake.s <- vector()
for (i in 1:length(ntot.s)){
  coeff.s <- c(child.s[i], 
             rnorm(1, sunnydiff.s, sunnydiff.sd.s)
  )
  
  dp.s <- rnorm(n = length(sunny.s), mean = mm.s %*% coeff.s, sd = 5)
  
  fake.s <- data.frame(dp.s=dp.s, sunny.s=sunny.s)  
}
hist(fake.s$dp.s)        
mean(fake.s$dp.s) # 48.2
sd(fake.s$dp.s) # 6.88
display(lm(dp.s~sunny.s, data=fake.s))
#lm(formula = dp.s ~ sunny.s, data = fake.s)
#coef.est coef.se
#(Intercept) 43.29     0.15  
#sunny.s2     9.75     0.22  
#---
#  n = 2000, k = 2
#residual sd = 4.85, R-Squared = 0.50


#### Let's plot the effects!
fake$sunny<-ifelse(fake$sunny==1, "control", "sunnyD")
base<- qplot(sunny, dp, data = fake, geom="boxplot", color=sunny) +
  xlab("Sunny D consumption") + ylab("Dopamine levels (ng/ml)") + ylim(35,90)

fake.e$sunny.e<-ifelse(fake.e$sunny.e==1, "control", "sunnyD")
effect<- qplot(sunny.e, dp.e, data = fake.e, geom="boxplot", color=sunny.e) +
  xlab("Sunny D consumption") + ylab("Dopamine levels (ng/ml)") + ylim(35,90)

fake.s$sunny.s<-ifelse(fake.s$sunny.s==1, "control", "sunnyD")
sample<- qplot(sunny.s, dp.s, data = fake.s, geom="boxplot", color=sunny.s) +
  xlab("Sunny D consumption") + ylab("Dopamine levels (ng/ml)") + ylim(35,90)

grid.arrange(base, effect, sample, ncol=3, nrow=1)


fake$child<- as.numeric(sample(1000))
fake.e$child.e<-as.numeric(sample(1000))
fake.s$child.s<-as.numeric(sample(2000))

bg<-ggplot(fake, aes(x=child, y=dp)) + geom_point(aes(color=sunny)) + geom_smooth(method="lm")  
#bh<-hist(fake$dp)
#grid.arrange(bg,bh, ncol=1, nrow=2)
eg<-ggplot(fake.e, aes(x=child.e, y=dp.e)) + geom_point(aes(color=sunny.e)) + geom_smooth(method="lm") 
#eh<-hist(fake.e$dp.e)
#grid.arrange(eg,eh, ncol=1, nrow=2)
sg<-ggplot(fake.s, aes(x=child.s, y=dp.s)) + geom_point(aes(color=sunny.s)) + geom_smooth(method="lm")      
#sh<-hist(fake.s$dp.s)
#grid.arrange(sg,sh, ncol=1, nrow=2)
grid.arrange(bg,eg,sg, ncol=3, nrow=1)




