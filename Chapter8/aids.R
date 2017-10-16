library(foreign)
library(tidyverse)
library(nlme)
library(modelr)

# id
# group
#   ranges from 1 to 4
#   1-3 are dual therapy, 4 is triple therapy
# age
# sex
# week
# logcd4
cd4 <- read.dta("../datasets/cd4.dta")

cd4 <- cd4 %>%
  mutate(
    trt = ifelse(group == 4, 1, 0),
    trt_label = ifelse(group == 4, "triple", "double"),
    week16 = (week - 16) * I(week >= 16),
    trt.week = I(trt == 1) * week,
    trt.week16 = I(trt == 1) * week16
  )

p1 <- ggplot(cd4, aes(x = week, y = logcd4, color = trt_label)) +
  geom_smooth(method = "loess")

# pg 229
model1 <- lme(
  logcd4 ~ week + week16 + trt.week + trt.week16, 
  data = cd4,
  random = ~ week + week16 | id
)

# figure out how to perform the Wald's tests

week_values <- cd4 %>%
  count(week) %>%
  select(week)

week_pop <- bind_rows(mutate(week_values, group = 1), mutate(week_values, group = 0)) %>%
  mutate(
    group_labels = ifelse(group == 1, "triple", "double"),
    week16 = (week - 16) * I(week >= 16),
    trt.week = I(group == 1) * week,
    trt.week16 = I(group == 1) * week16
  )

week_pop <- week_pop %>%
  mutate(pred = predict(model1, level = 0, week_pop))

p2_pop <- p1 + 
  geom_line(data = week_pop, aes(x = week, y = pred, color = group_labels))

# pg 232
model2 <- lme(
  logcd4 ~ age + sex + week + week16 + trt.week + trt.week16,
  data = cd4,
  random = ~ week + week16 | id
)

age_values <- cd4 %>%
  count(age) %>%
  select(age)

age_min_max <- round(range(age_values$age))
ages <- seq(age_min_max[1], age_min_max[2], by = 1)
model2_input <- expand.grid(week = week_values$week, age = ages, group = c(0, 1), sex = c(0, 1)) %>%
  mutate(
    group_labels = ifelse(group == 1, "triple", "double"),
    sex_labels = ifelse(sex == 0, "female", "male"),
    week16 = (week - 16) * I(week >= 16),
    trt.week = I(group == 1) * week,
    trt.week16 = I(group == 1) * week16
  )

model2_input <- model2_input %>%
  mutate(pred = predict(model2, level = 0, model2_input))

p3_model2 <- ggplot(model2_input, aes(x = week, y = pred)) +
  geom_line(aes(group = age, color = age), alpha = 0.5) +
  facet_grid(group_labels ~ sex_labels)

p4_model2 <- model2_input %>%
  filter(age == 45) %>%
  ggplot(aes(x = week, y = pred)) +
  geom_line() +
  facet_grid(group_labels ~ sex_labels)
    

  

# VarCorr - calculates estimated variances between random effects terms
# intervals
