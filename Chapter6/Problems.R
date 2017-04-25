library(foreign)
library(modelr)
library(tidyverse)
library(nlme)

# 6.1 In a study of weight gain (Box, 1950) investigators randomly assigne3d 30 rats to three treatment groups: treament 1 was a control (no additive); trements 2 and 3 consisted of two different additives (thiouracil and thyroxin respectively) to the rats drinking water. Weight, in grams was measured at baseline (week 0) ad at weeks 1, 2, 3, and 4. Due to an accident at the beginning of the study, data on 3 rats from the thyroxin group are unavaliable.
#
# The variable Group is coded 1 = control, 2 = thiouracil, and 3 = thyroxin.

# 6.1.1 On a single graph, contruct a time plot that displays the mean weight verus time (in weeks) for the three groups. Describe the general characteristics fo the time trend for the three groups.

data <- read.dta("../datasets/rat.dta")

data_new <- data %>%
  as_tibble() %>%
  gather(timepoint, weight, y1, y2, y3, y4, y5) %>%
  mutate(
    week = case_when(
      .$timepoint == "y1" ~ 0,
      .$timepoint == "y2" ~ 1,
      .$timepoint == "y3" ~ 2,
      .$timepoint == "y4" ~ 3,
      .$timepoint == "y5" ~ 4
    ),
    time = week + 1,
    group = case_when(
      .$group == 1 ~ "control",
      .$group == 2 ~ "thiouracil",
      .$group == 3 ~ "throxin"
    ),
    group = factor(group, levels = c("control", "thiouracil", "throxin"))
  )

data_new_summary <- data_new %>%
  group_by(group, week) %>%
  summarize(mean_weight = mean(weight))

p1 <- ggplot(data_new_summary, aes(x = week, y = mean_weight, color = group)) +
  geom_line() +
  geom_point() +
  labs(color = "Treatment", x = "Week", y = "mean weight (g)")

# control and throxin appear to have same slope
# thiouracil has lower slope and decreases further at after week 2

# 6.1.2 Read the data from the external file and put the data in "univariate" or "long" format, with five "records per subject.

# 6.1.3 Assume that the rate of increase in each group is approximately constant throught the duration of the study. Assuming an unstructed covariance matrix, construct a test of whether the rate of increase differs in the groups.
#
# B0 + x*B1 + group*B2
model1 <- gls(
  weight ~ week + group:week,
  corr = corSymm(, form = ~ time | id),
  data = data_new,
  weights = varIdent(form = ~ 1 | time)
)

data_new <- data_new %>%
  mutate(
    thiouracil.week = week * (group == "thiouracil"),
    throxin.week = week * (group == "throxin")
  )
  
model2 <- gls(
  weight ~ week + thiouracil.week + throxin.week,
  corr = corSymm(, form = ~ time | id),
  data = data_new,
  weights = varIdent(form = ~ 1 | time)
)

# model2 is the preferred method, because anova will test all coefficents against 0 giving better interpretation
# the rate of increase differs between the groups and thiouracil has a decreased rate

# 6.1.4 On a single graph, construct a time plot that displays the estimated mean weight verus time (in weeks) for the three treatment groups from the results generated from Problem 6.1.3

grid <- data_new %>%
  data_grid(group, week) %>%
  mutate(
    thiouracil.week = week * (group == "thiouracil"),
    throxin.week = week * (group == "throxin")
  ) %>%
  add_predictions(model2)

p2 <- ggplot(data_new_summary, aes(x = week, y = mean_weight, color = group)) + 
  geom_point(size = 2) +
  geom_line(data = grid, aes(y = pred, color = group)) +
  labs(color = "treatment", y = "weight (g)")

# 6.1.5 Base on the results from Problem 6.1.3, what is the estimated rate of increase in mean weight in the control group (group 1)? What is the estimated rate of increase n mean weight int he thiouracil group (group 2)? What is the estimated rate of increase in mean weight in the thyroxin group (group 3)?
#
# control = 26.21509
# thiouracil = 26.21509 - 7.09640
# throxin = 26.21509 - 2.09436

# 6.1.6 The study investigators conjectured that there would be an increase in weight, but that the rate of increase would level-off toward the end of the study. They also conjectured that this pattern of change may differ in the three treatment groups. Assuming unstructed covariance matrix, construct a test of this hypothesis.

data_new <- data_new %>%
  mutate(
    week2 = (week - 2) * (week >= 2),
    thiouracil.week2 = week2 * (group == "thiouracil"),
    throxin.week2 = week2 * (group == "throxin")
  )

model3 <- gls(
  weight ~ week + thiouracil.week + throxin.week + week2 + thiouracil.week2 + throxin.week2,
  corr = corSymm(, form = ~ time | id),
  weights = varIdent(form = ~ 1 | time),
  data = data_new
)

grid2 <- grid %>%
  select(-pred) %>%
  mutate(
    week2 = (week - 2) * (week >= 2),
    thiouracil.week2 = week2 * (group == "thiouracil"),
    throxin.week2 = week2 * (group == "throxin")
  ) %>%
  add_predictions(model3)

p3 <- ggplot(data_new_summary, aes(x = week, y = mean_weight, color = group)) + 
  geom_point(size = 2) +
  geom_line(data = grid2, aes(y = pred, color = group)) +
  labs(color = "treatment", y = "weight (g)")

# using a spline at week 2, we do see weight level off. This is seen by the week2 coefficient
# is found equal to -1.03022. There is no difference in rate between controls and throxin.
# Thiouracil has a decreased rate of weight gain contradicting the original conjecture.

# 6.1.7 Compare and contrast the result form Problems 6.1.3 and 6.1.6. Does a model with only a linear trend in tmie adequately account from the pattern of change in the three treatments groups? Provide results that support yoru conclusion.
  
model.spline <- gls(
  weight ~ week + thiouracil.week + throxin.week + week2 + thiouracil.week2 + throxin.week2,
  corr = corSymm(, form = ~ time | id),
  weights = varIdent(form = ~ 1 | time),
  data = data_new,
  method = "ML"
)

model.trend <- gls(
  weight ~ week + thiouracil.week + throxin.week,
  corr = corSymm(, form = ~ time | id),
  data = data_new,
  weights = varIdent(form = ~ 1 | time),
  method = "ML"

# model.spline is better according to the maximum liklihood test.

# 6.1.8 Given thre results of all the previous analyses, what conclusions can be drawn about the effect of the additives on the patterns of cahnge in weight?

# Thiouracil has no effect over time. Throxin decreases weight abnormally and then increases the rate of decrease after 2 weeks of administering the drug.
)
