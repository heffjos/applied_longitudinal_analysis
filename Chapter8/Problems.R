library(tidyverse)
library(foreign)
library(nlme)

exercise <- read.dta("../datasets/exercise.dta")

exercise <- exercise %>%
  as_tibble() %>%
  gather(day, strength, y0:y12) %>%
  mutate(
    group_label = ifelse(group == 1, "Program1", "Program2"),
    day_num = case_when(
      day == "y0" ~ 0,
      day == "y2" ~ 2,
      day == "y4" ~ 4,
      day == "y6" ~ 6,
      day == "y8" ~ 8,
      day == "y10" ~ 10,
      day == "y12" ~ 12
    )
  ) %>%
  mutate(
    day_num2 = as.integer(day_num),
    group_label2 = factor(group_label),
    group_label3 = group - 1
  )
# Program1 = number of repetitions increased, but same weight
# Program2 = repetitions fixed, but weight increased

# 8.1.1 
# On a single graph, construct a time plot that displays the mean strength versus
# time (in days) for the two treatment groups. Describe the general characteristics
# of the time trends for the two exercise programs.
mean_pop <- exercise %>%
  group_by(group, day_num) %>%
  summarize(mean = mean(strength, na.rm = TRUE))

p1 <- mean_pop %>%
  ggplot(aes(x = day_num, y = mean, color = factor(group))) +
  geom_line() +
  labs(x = "day", y = "population mean strength", color = "Program")

# People enrolled in Program 2 appear to be inherently stronger.
# Strength in both programs increase until day 6 and then it plateaus.

# 8.1.2 
# Read the data from the external file and put the data in a "univariate" or "long"
# format, with 7 "records" per patient.

# 8.1.3
# Fit a model with randomly varying intercepts and slopes, and allow the mean
# values of the intercept and slope to depend on treatment group (i.e., include
# main effect of treatment, a linear time trend, and a atreatment by linear time
# trend ineteraction as fixed effects).

    #(a) What is the estimated variance of the random intercepts?
    #(b) What is the estimated variance of the random slopes?
    #(c) What is the estimated correlation between the random intercepts and slopes?
    #(d) Give an interpretation to the magnitude of the estimate variance of the
    #    random intercepts. For example, "approximately 95% of subjects have baseline
    #    measures of strength between a and b" (calculate the limits of the interval
    #    between a and b).
    #(e) Give an interpretation to the magnitude of the estimate variance of the random
    #    slopes.

exercise <- exercise %>%
  filter(!is.na(strength))

model1 <- lme(
  strength ~ group_label * day_num,
  data = exercise,
  random = ~ group_label * day_num | id
)

model2 <- lme(
  strength ~ group_label + day_num,
  data = exercise,
  random = ~ group_label + day_num | id
)


