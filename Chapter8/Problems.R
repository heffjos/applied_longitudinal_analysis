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
# main effect of treatment, a linear time trend, and a treatment by linear time
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

# create individual models for my purpose
exercise_grouped <- groupedData(
  strength ~ day_num | id,
  data = as_data_frame(exercise),
  labels = list(x = "day", y = "strength"),
  outer = ~ group
)

model2 <- lme(
  strength ~ group_label + day_num,
  data = exercise,
  random = ~ group_label + day_num | id
)

# (a) What is the estimated variance of the random intercepts?
# Intercept        = 9.69
# group (Program2) = 1.70

# (b) What is the estiamted variance of the random slopes?
# day (time)       = 0.047
# intereaction     = 0.029

# (c) What is the estimated correlation between the random intercepts and slopes?
# use getVarCov to get these values
# intercept - group        = -0.59
# intercept - day (time)   = 0.11
# intercept - interaction  = -0.16
# group - day (time)       = -0.07
# group - interaction      = 0.0024
# day (time) - interaction = -0.027

#(d) Give an interpretation to the magnitude of the estimate variance of the
#    random intercepts. For example, "approximately 95% of subjects have baseline
#    measures of strength between a and b" (calculate the limits of the interval
#    between a and b).
# Approximately 95% of subjects in program1 have baseline measures of strength between 
# (80.1 - 1.96 * 3.11, 80.1 + 1.96 * 3.11) = (73.9, 86.1)
#
# Variance of program2 population = (9.69 + 1.70 + 2 * (-0.59) = 10.21
# Apprixmately 95% of subjects in program2 have baseline measuers of strength between
# (81.2 - 1.96 * 3.20, 81.2 + 1.96 * 3.20) = (74.9, 87.5)

# Since there is much overlap between program1 and program2 initial strength, there is
# no significant difference between the two. This is confirmed by looking at the
# p-value (0.2901).

#(e) Give an interpretation to the magnitude of the estimate variance of the random
#    slopes.

# The estimate of the program1 mean slope is 0.11715 which is significant at the 0.05 level.
# Approximately 95% of people in program1 have strength changes between
# (0.12 - 1.96 * 0.22, 0.12 + 1.96 * 0.22) = (-0.31, 0.5512) which is not a lot.
# 36% of interval is strength loss.

# The estimate of the program2-time interaction slope is 0.049 which is not significant
# at the 0.05 level. This means under program2 strength changes at the rate 
# 0.049 + 0.12 = 0.17. The variance for this estimate is 0.047 + 0.029 + 2 * (-0.027) = 0.022
# Approximately 95% of people in program2 have strength changes between
# (0.17 - 1.96 * 0.15, 0.17 + 1.96 * 0.15) = (-0.124, 0.464). This interval is entirely
# within group1 rate of increase, hence it is not significant.

# 8.1.4 Is a model with only randomly varying intercepts defensible? Explain.
model3 <- lme(
  strength ~ group_label * day_num,
  data = exercise,
  random = ~ group_label | id
)

# From the plot of the intervals in lmList, there is some participant variation accross
# the slope day_num, but not nearly as much as the intercept. When performing an anova
# between model1 and model3, model1 is better in terms of AIC, BIC, and logLik.

# 8.1.5 What are teh mean intercept and slope in the two exercise programs?
# Program1 intercept = 80.1
# Program1 slope     = 0.12
# Program2 intercept = 80.1 + 1.3 = 81.4
# Program2 slope     = 0.12 + 0.049 = 0.169

# 8.16 Based on teh previous analysis, interpret the effect of treatment on chagnes in
# strength. Does your analysis suggest a difference between the two groups?
# There is not a difference of baseline strength between the two groups (p = 0.2901).
# Tehre is not a difference of strength change between tht two groups (p = 0.4815)

# 8.1.7 What is the estimate of Var(Yi1|bi)? What is the estimate of Var(Yi1)? Explain
# the difference.
