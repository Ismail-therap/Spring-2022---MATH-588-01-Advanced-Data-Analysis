library(readr)
fullBumpus <- read_table2("HW1 files/fullBumpus.txt")

head(fullBumpus)

## 1 (a)

ttst1 = t.test(Weight~Survive,var.equal=TRUE,data = fullBumpus)
ttst2 = t.test(Weight~Survive,var.equal=FALSE,data = fullBumpus)

## 1 (b)

res = resid(lm(Weight~Survive, data = fullBumpus))

qqline(res)
hist(res)
shapiro.test(res)

# After observing the qqplot we can interpret that the data does not follow normal distribution.
# Because the lower tail and upper part in the plot not goes close to the stright line. We should
# take some sort of transformation before carry out the t-test. The data distribution seems
# right skweed, so log-transformation could leads the data to normal shape and we know normality 
# is the key assumption for t-test.


## 1 (c)



