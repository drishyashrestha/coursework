library(ISLR2)

df <- read.table("babyweights.txt")
summary(df)
str(df)
head(df,100)

lm.fit <- lm(bwt~smoke, data = df)
summary(lm.fit)

#5.29
#Heights VS Weights Scatterplot

# (a)	Positive, linear relationship: taller people tend to weigh more.
# (b)	Equation: weight = -105.01 + 1.02 × height. Slope: +1.02 kg per cm. Intercept: not meaningful physically.
# (c)	H0: no association. H1 association. p-value ≈ 0. Strong evidence for association of height and weight .
# (d)	R² ≈ 0.52. About 52% of weight variation is explained by height.

#6.1
#eqn is weight = 123.05 - 8.4 smoke
# slope of (-8.94) shows how mother who smoke has their baby's weight by 8.4 ounces
# less than botn to non smoker mothers.
#Predict birth weights of non smoker so y = 123.05 -8.5*0 = 123.05 ounces
# for smoker y = 123.05 - 8.4 *1 = 114.11.

# looking at p-value which is really small we reject the null so there is statistically significant
#relationship.

#6.2

lm.fit <- lm(bwt~parity, data = df)
summary(lm.fit)

# baby's weight = 120.06 + -1.92 parity 
#this means the weight of baby is changed by 1.92 ounces less if it is first born
#weight of baby if its not first born (parity =1)  =  120.06 - 1.92 = 118.14 ounces
#weight of baby if its first born (parity =0) = 120.06 ounces
# it's not statistically significant since its p-value is 0.1 which is more than 0.05


#6.3
lm.fit <- lm(bwt~ . , data = df)
summary(lm.fit)

#a. baby weight (y) = -80.41 + 0.44gestation -3.32 parity - 0.0089 age + 1.15 height +
# 0.05 weight - 8.40 smoke

#b. here we can say since slope of gestation is 0.44 meaning the weight is increases by 0.44 ounces 
#for each additional day of pregnancy

#for age we have slope of -0.01 meaning the weight decreases by 0.01 ounces foe each additional year 
#of mother's age which is very small and not significant and its p value is 0.9


#c. since for the previous model parity was only the predictor but for this we are taking in account
# with many variables  the coefficient of parity changes. this is called controlling for confounding 
# the effect of parity is adjusted for the effects of the other predictors.

#d. residuals is 
head(df)
pred_bwt = -80.41 + 0.44*284 - 3.33*0 - 0.01*27 + 1.15*62 + 0.05*100 - 8.40*0
print(120 - pred_bwt)

#e variance of residuals = 249.28 , variance of birth weights of all babies = 332.57
summary(lm.fit)
summary(lm.fit)$r.squared
summary(lm.fit)$adj.r.squared






