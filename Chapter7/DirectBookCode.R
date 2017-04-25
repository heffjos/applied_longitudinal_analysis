library(foreign)

ds <- read.dta("../datasets/exercise.dta")
exlong <- reshape(ds, idvar="id", 
   varying=c("y0","y2","y4","y6","y8","y10","y12"), v.names="y", 
   timevar="time", time=0:6, direction="long")
exlong <- subset(exlong, time!=1 & time!=5)
attach(exlong)
day <- time*2
day.f <- factor(day, c(0,4,6,8,12))
group.f <- factor(group, c(1,2))

newtime <- time
newtime[time==0] <- 1
newtime[time==2] <- 2
newtime[time==3] <- 3
newtime[time==4] <- 4
newtime[time==6] <- 5 

library(nlme)
model1 <- gls(y ~ group.f*day.f, na.action=na.omit, 
   corr=corSymm(, form= ~ newtime | id), 
   weights = varIdent(form = ~ 1 | newtime))
summary(model1)
