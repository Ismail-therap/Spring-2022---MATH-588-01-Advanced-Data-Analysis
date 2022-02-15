library(Sleuth3)
head(ex0429)

# Median Percent Lost by Action
tapply(ex0429$PercentLost,ex0429$Action,median,na.rm=T)

# a
# Rank sum test
wilcox.test(PercentLost~Action,conf.int = TRUE,data = ex0429)

#b
t.test(PercentLost~Action,var.equal=TRUE,data = ex0429)








# a
sample_var = var(case0502$Percent)

# b
total_ss = sample_var*45
total_ss
# c

sp = ((9-1)*5.03^2+(5-1)*11.94^2+(6-1)*6.58^2+(9-1)*4.59^2+(2-1)*3.818^2+(6-1)*9.010^2+(9-1)*5.969^2)/((9-1)+(5-1)+(6-1)+(9-1)+(2-1)+(6-1)+(9-1))
residual_ss = sp*(46-7)
residual_ss

ms_resid = residual_ss/(46-7)
ms_resid

# SS Between groups
ss_btwn = total_ss-residual_ss
ss_btwn

ms_btwn = ss_btwn/6
ms_btwn


# F-statistic
f_statistic = ms_btwn/ms_resid
f_statistic


pf(f_statistic,6,39, lower.tail = FALSE)


# So, we are getting exactly similar results like in Disply 5.10



### 3

#install.packages("car")
library(car)
# Levene's test with one independent variable
leveneTest(Percent ~ Judge, data = case0502)

# H0: All population variance are equal
# H1: Populaiton variance are unequal




### 4

head(ex0623)
hist(WtLoss24~Group,data=ex0623)


# install.packages("ggplot2")
library(ggplot2)

ggplot(ex0623, aes(x = WtLoss24)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Group ~ .)

# The histogram suggest that data does not suffer seviarly from non normality. So, we can use
# one way anova to do the comarison among groups. 

# Compute the analysis of variance
anova_oneway <- aov(WtLoss24 ~ Group, data = ex0623)
# Summary of the analysis
summary(anova_oneway)


# Multiple test:

TukeyHSD(anova_oneway)


# Checking ANOVA Assumptions

# Equal variance
leveneTest(WtLoss24 ~ Group, data = ex0623)

# This test of homogeneity suggest that, at 5\% level of significane we can conclude that the 
# groups do not have equal variance. Equality of variance is one of the assumption setted before
# cary out the anova. The alternative is to use non parametric approach to test the hypothesis. 

# Normality
plot(anova_oneway, 2)
