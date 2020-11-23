# AvodatGmar
my theasis work code
## Avodat Gmar Code ##

## Avodat Gmar Code ##

# turning ses to binary variable sesr 1= low ses 2=high ses #

danielhalabi$sesr[danielhalabi$ses < 4] <- 1
 danielhalabi$sesr[danielhalabi$ses > 3] <- 2
danielhalabi$sesr[danielhalabi$ses > 6] <- NA

# turning q50 (Subjective social status) into three levels variable 1=low SSS 2=mid SSS 3= high SSS #
danielhalabi$SSS <- danielhalabi$q50
 danielhalabi$SSS[danielhalabi$SSS < 3] <- 1
 danielhalabi$SSS[danielhalabi$SSS == 3] <- 2
 danielhalabi$SSS[danielhalabi$SSS> 3] <- 3

 # turning all the factorial variables into factors (SSS, sesr, sex, discriminated, jworno, q1) #
> danielhalabi$SSS = factor(danielhalabi$SSS)
> danielhalabi$sesr = factor(danielhalabi$sesr)
> danielhalabi$discriminated = factor(danielhalabi$discriminated)
> danielhalabi$sex = factor(danielhalabi$sex)
> danielhalabi$q1 = factor(danielhalabi$q1)
danielhalabi$jworno = factor(danielhalabi$jworno)

 # create demographic variables boxplot usuing ggplot2 without variables names #
 ggplot(danielhalabi, aes(x = q1, y = age, fill = q1,)) + geom_boxplot() + 
+     facet_grid(jworno ~ sex, margins = FALSE) + geom_jitter()

 # creating oridinal logit regression model named "model1" #
model1 <- polr(q1 ~ age + sex + jworno + discriminated + sesr + SSS, data = danielhalabi, Hess = TRUE) 

# calculating p values by comparing t values against normal distribution #
 summary_table <- coef(summary(model1))
 pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
 summary_table <- cbind(summary_table, "p value" = round(pval,3))
 summary_table

# Create nice table using Sjplot package #
tab_model(model1)

 # create SSS effects plot using effects package #
plot(Effect(focal.predictors = c("SSS"), model1))
