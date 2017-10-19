library(tidyverse)
library(foreign)
library(nlme)
library(modelr)
library(broom)
library(forcats)

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

# model1 <- lme(
#   logcd4 ~ week + week16 + week:group.factor + week16:group.factor,
#   data = cd4,
#   random = ~ week + week16 | id
# )

# 8.2.3
# Is a model with only randomly varying intercepts defensible? Explain?
model2 <- lme(
  logcd4 ~ week + week16 + week:group.factor + week16:group.factor,
  data = cd4,
  random = ~ 1 | id
)

insufficient <- cd4 %>%
  count(id) %>% 
  filter(n < 4)

sufficient <- anti_join(cd4, insufficient, by = "id")

week_model <- function(df) {
  lm(logcd4 ~ week + week16, data = df)
}

# sufficient <- sufficient %>%
#   group_by(id) %>%
#   nest() %>%
#   mutate(
#     lm_model = map(data, week_model),
#     data = map2(data, lm_model, add_predictions),
#     coefficients = map(lm_model, tidy)
#   ) %>%
#   unnest(coefficients) %>%
#   left_join(select(cd4, id, group) %>% distinct(), by = "id")

int_data <- sufficient %>%
  filter(term == "(Intercept)") %>%
  mutate(id_factor = fct_reorder2(factor(id), group, estimate))

p_int2 = int_data %>%
  ggplot(aes(x = estimate, y = id_factor)) + 
  geom_point() +
  geom_errorbarh(aes(xmax = estimate + std.error, xmin = estimate - std.error)) +
  facet_grid(group ~ .)

p_week = sufficient %>%
  filter(term == "week") %>%
  mutate(id_factor = fct_reorder2(factor(id), group, estimate)) %>%
  ggplot(aes(x = estimate, y = id_factor)) + 
  geom_point() +
  geom_errorbarh(aes(xmax = estimate + std.error, xmin = estimate - std.error)) +
  facet_grid(group ~ .)

p_week16 = sufficient %>%
  filter(term == "week16") %>%
  mutate(id_factor = fct_reorder2(factor(id), group, estimate)) %>%
  ggplot(aes(x = estimate, y = id_factor)) + 
  geom_point() +
  geom_errorbarh(aes(xmax = estimate + std.error, xmin = estimate - std.error)) +
  facet_grid(group ~ .)

# cd4_groups <- groupedData(
#   logcd4 ~ week + week16 | id,
#   data = as_data_frame(sufficient),
#   labels = list(x = "week", y = "logcd4"),
#   outer = ~ group
# )  

# No it is not defensible. According to AIC, BIC, and logLike, model1 is better.
# Also looking at the plots (p_week and p_week16) there is inidvidual varaion among the
# models which is better explained by a model with random intercepts and slopes.

# 8.2.4
# Construct a 6-degress-of-freedom test of the null hypthesis of no treatment
# group differences in the changes in log CD4 counts.

