library(tidyverse)
library(foreign)
library(nlme)

# id
# group
#   ranges from 1 to 4
#   1-3 are dual therapy, 4 is triple therapy
# age
# sex
# week
# logcd4
cd4 <- read.dta("../datasets/cd4.dta") %>%
  as_tibble()

p1 <- ggplot(cd4, aes(x = week, y = logcd4, color = trt_label)) +
  geom_smooth(method = "loess")

# 8.2.1
# On a single graph, construct a smoothed time plot that displays the mean log
# CD4 counts versus time (in weeks) for the four treament groups. Describe the
# general characteristics of the time ternds for the four groups.
p1 <- ggplot(cd4, aes(x = week, y = logcd4, color = factor(group))) +
  geom_smooth(method = "loess") +
  labs(color = "group")

# groups 2, 3, and 4 have similar curves where there is an initial increase followed by
# a decrease. The onset of decrease is different for each group. Alos group 3 appears to
# increase again at week 33. Group 1 decreases without any increase period.

# 8.2.2
# Fit a model where each patient's response trajectory is represented by a randomly
# varying piecewise linear spline with a knot at week 16. That is fit a model
# with random intercepts and two randomly varying slopes, one slope for
# the changes in log CD4 counts before week 16, another slope for the changes
# in response after week 16. Allow the average slopes for chanes in response before and
# after week 16 to vary by group, but assume the mean response at
# baseline is the same in the four groups.
cd4 <- cd4 %>%
  mutate(
    week16 = (week - 16) * (week >= 16),
    group.factor = case_when(
      group == 1 ~ "group1",
      group == 2 ~ "group2",
      group == 3 ~ "group3",
      group == 4 ~ "group4"
    )
  )

model1 <- lme(
  logcd4 ~ week + week16 + week:group.factor + week16:group.factor,
  data = cd4,
  random = ~ week + week16 + week:group.factor + week16:group.factor | id
)

