library(foreign)
library(tidyverse)
library(ggbeeswarm)
library(nlme)
library(modelr)

# 7.1 In a study of dental growth, measurments of the distance (mm) from the center of the pituitary gland to the pteryomaxillary fissure were obtained on 11 grils and 16 boys at ages 8, 10, 12, and 14 (Potthoff adn Roy, 194).

# The raw data are stored ina n external file: dental.dat

# Each row of the data set contains the following six variables:

# ID Gender Y1 Y2 Y3 Y4

# Note: the categorical (character) variable Gender is coded F = Female, M = Male. The Third measure (at age 12) on subject ID = 20 is  apotential outlier.

# 7.1.1 On a single grpah, construct a time plot that displays the mean distance (mm) versus age (in years) for boys and girls. Describe the time trends for boys and girls.
data <- read.dta("../datasets/dental.dta") %>% as_tibble()
data_long <- data %>%
  gather(age_label, distance, y1:y4) %>%
  mutate(
    age = case_when(
      .$age_label == "y1" ~ 8,
      .$age_label == "y2" ~ 10,
      .$age_label == "y3" ~ 12,
      .$age_label == "y4" ~ 14
    )
  )

data_long_summary <- data_long %>%
  group_by(gender, age) %>%
  summarize(distance = mean(distance))

p1 <- ggplot(data_long_summary, 
  aes(x = factor(age), y = distance, color = gender, group = gender)) +
  geom_line(size = 2) +
  geom_beeswarm(data = data_long) +
  geom_beeswarm(
    data = data_long %>% filter(id == 20),
    color = "darkgreen",
    size = 2,
    show.legend = FALSE
  ) +
  labs(
    x = "age (years)",
    y = "distance (mm)",
    color = "gender"
  )

p2 <- ggplot(data_long, aes(x = age, y = distance)) +
  geom_line() +
  geom_point() +
  facet_wrap(gender ~ id)

# The general trend is an increase in distance as age increases. Males spike at age 10; females slow down at age 10 (spline?).

# 7.1.2 Read the data from the external file and put the data in a "univariate" or "long" format, with four "records" per subject.

# 7.1.3 For the "maximal" model, assume a saturated model for the mean response. Fit the following models for the covariance:
# (a) unstructure covariance          (corSymm)
# (b) compound symmetry               (corCompSymm)
# (c) heterogenous compound symmetry  (corCompSymm + weights)
# (d) autoregressive                  (corAR1)
# (e) heterogeneous autoregressive    (corAR1 + weights)
# Choose a model for the covariance that adequately fits the data.
data_long <- data_long %>%
  mutate(
    age.f = factor(age, c(8, 10, 12, 14)),
    time = case_when(
      .$age == 8 ~ 1,
      .$age == 10 ~ 2,
      .$age == 12 ~ 3,
      .$age == 14 ~ 4
    )
  )

models <- vector(mode = "list", length = 5)

# (a) unstructured covariance
models[[1]] <- gls(
  distance ~ age.f * gender,
  corr = corSymm(form = ~ time | id),
  data = data_long,
  weights = varIdent(form = ~ 1 | time)
)

# (b) compound symmetry - corresponds to uniform correlation
models[[2]] <- gls(
  distance ~ age.f * gender,
  corr = corCompSymm(form = ~ time | id),
  data = data_long
)

# (c) heterogenous compound symmetry
models[[3]] <- gls(
  distance ~ age.f * gender,
  corr = corCompSymm(form = ~ time | id),
  data = data_long,
  weights = varIdent(form = ~ 1 | time)
)

# (d) ar1
models[[4]] <- gls(
  distance ~ age.f * gender,
  corr = corAR1(form = ~ time | id),
  data = data_long
)

# (e) heterogeneous ar1
models[[5]] <- gls(
  distance ~ age.f * gender,
  corr = corAR1(form = ~ time | id),
  data = data_long,
  weights = varIdent(form = ~ 1 | time)
)

model_info <- tibble(model_id = c(1, 2, 3, 4, 5), models = models) %>%
  mutate(
    bic = map_dbl(models, ~ summary(.)$BIC),
    aic = map_dbl(models, ~ summary(.)$AIC),
    log_lik = map_dbl(models, ~ summary(.)$logLik)
  )

# now we need to pick a model, we want to minimize AIC, BIC and maximize logliklihood
# p-value in anova checks if more specific model is better than the more general one
# degrees of freedom are the number of vaules in teh final calculation of a statistic that are free to vary
# going by AIC model2 (compound symmetry) is the best model
# going by BIC model2 (compound symmetry) is again the best model
# so we want to use model2
# skip p-values, because it doesn't make logic sense
#    model1 (df=18) is not significantly better than model2 (df=10) at 0.05 level (p-value = 0.3118)
#    but model5 (df=13) is significantly better than model2 (df=10) at 0.05 level (p-value = 0.0281) even though AIC and BIC for model2 is less than that for model5

# 7.1.4 Given the choice of model for the covariance from Problem 7.1.3, treat age (or time) as a cateogrical variable and fit a model that includes the effects of age, gender, and their interactions. Determine whether the pattern of change over tiem is different for boys and girls.

# We first need to check if there is an interaction effect before looking if there is a gender effect. According to anova, there is not a significant interaction effect at the 0.05 level (p = 0.0759). There is a significant gender effect (p = 0.0029). The coefficient genderM = 1.69 meaning males have a greater distance than females.

# 7.1.5 Show how the estimated regression coefficients from Problem 7.1.4 can be used to estimate the means in the two groups at ages 8 and 14.

mean_summary <- data_long %>%
  group_by(gender, age) %>%
  summarize(distance = mean(distance)) %>%
  filter(age %in% c(8, 14))

m1_coef <- coefficients(models[[2]])
coefficient_calc <- tribble(
  ~ gender, ~ age, ~ distance,
  "F", 8, m1_coef[1],
  "F", 14, m1_coef[1] + m1_coef[4],
  "M", 8, m1_coef[1] + m1_coef[5],
  "M", 14, m1_coef[1] + m1_coef[5] + m1_coef[4] + m1_coef[8]
)

# 7.1.6 Given the chose of model for the covariance from Problem 7.1.3, treat age as a continous variable and fit a model that includes the effects of a linear trend in age, gender, and their interaction. Compare and contrast the results with those obtained in Problem 7.1.4.

data_long <- data_long %>%
  mutate(age.centered = age - mean(age))

# creating the model in this way makes the intercept equal to the mean female distance at age = 0
m_age <- gls(
  distance ~ age * gender,
  corr = corCompSymm(form = ~ time | id),
  data = data_long
)

# create the model in this way makes the intercept equal to the mean female distance at all ages 
m_age_centered <- gls(
  distance ~ age.centered * gender,
  corr = corCompSymm(form = ~ time | id),
  data = data_long
)

# When using age as a continuous variable, there is an interaction between age and gender, meaning distance does not increase at a constant rate across age for one of the genders. The plots indicate this is for males.

# 7.1.7 On a single graph, construct a time plot that displays the estimated mean distance (mm) versus age (in years) for boys and girls from the results generated from Problem 7.1.6.
grid <- data_grid(data_long, gender, age) %>%
  add_predictions(m_age) %>%
  left_join(data_long_summary, by = c("gender", "age"))
p3 <- ggplot(grid, aes(age, pred, color = gender)) +
  geom_line(size = 1.5) +
  geom_point(aes(y = distance), size = 4, show.legend = FALSE)

# 7.1.8 Show how the regression coefficients from Problem 7.1.6 can be used to estimate the means in the two groups at ages 8 and 14.

# 7.1.9 Does a model with only a linear trend in age adequately account for the pattern of change in the two groups?

# 7.1.10 The third mease (at age 12) on subject ID = 20 is a potential outlier. Repeat the analyses in Problems 7.1.3, 7.1.4, 7.1.6 and 7.1.9 excluding the third measure on subject ID = 20. Do the substantive conclusions change?

# 7.1.11 Given the results of all the previous analyses, what conclusions can be drawn about gender differences in patterns of dental growth?
