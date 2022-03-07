library(Sleuth3)
bb = case1102
sapply(bb, class) # Simplify future typeing by changing names to lower case:
names(bb) = casefold(names(bb)) 
names(bb)

# Make new variables 
bb$logBLratio = log(bb$brain/bb$liver)
bb$logTime = log(bb$time)

# a(1)
library(dplyr)
with(bb, table(sex)) %>% prop.table()


# a(2)
with(bb, table(treatment,days)) %>% prop.table()

# b

# Plot key variables
with(bb, plot(logBLratio ~ logTime, pch=as.numeric(bb$treat), xlab="Log Time"))
with(bb, table(bb$treat,as.numeric(bb$treat)))
legend("topleft", legend=c("Barrier Disruption","Saline Control"), pch=1:2)


m0 = lm(logBLratio ~ logTime + treatment, bb)
summary(m0)
plot(m0)
plot(m0, which=c(1,1))
