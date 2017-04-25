library(foreign)
ds <- read.dta("../datasets/tlc.dta")
tlclong <- reshape(
  ds, 
  idvar = "id",
  varying = c("y0", "y1", "y4", "y6"),
  v.names = "y",
  timevar = "time",
  time = 1:4,
  direction = "long"
)

attach(tlclong)
week <- time
week[time == 1] <- 0
week[time == 2] <- 1
week[time == 3] <- 4
week[time == 4] <- 6
week.f <- factor(week, c(0, 1, 4, 6))
