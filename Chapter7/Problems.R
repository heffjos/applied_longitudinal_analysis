library(foreign)
library(tidyverse)
library(ggbeeswarm)

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
      age_label == "y1" ~ 8,
      age_label == "y2" ~ 10,
      age_label == "y3" ~ 12,
      age_label == "y4" ~ 14
    )
  )

data_long_summary <- data_long %>%
  group_by(gender, age) %>%
  summarize(distance = mean(distance))

p1 <- ggplot(data_long_summary, aes(x = factor(age), y = distance, color = gender, group = gender)) +
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

# 7.1.3 For the "maximal" model, assume a saturated model for the mean sresponse. Fit the following models for the covariance:
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
      age == 8 ~ 1,
      age == 10 ~ 2,
      age == 12 ~ 3,
      age == 14 ~ 4
    )
  )

# (a) unstructured covariance
model1 <- gls(
  distance ~ age * gender,
  corr = corSymm(form = ~ time | id),
  data = data_long,
  weights = varIdent(form = ~ 1 | time)
)

# (b) compound symmetry
model2 <- gls(
  distance ~ age * gender,
  corr = corCompSymm(form = ~ time | id),
  data = data_long
)

# (c) heterogenous compound symmetry
model3 <- gls(
  distance ~ age * gender,
  corr = corCompSymm(form = ~ time | id),
  data = data_long,
  weights = varIdent(form = ~ 1 | time)
)

# (d) ar1
model4 <- gls(
  distance ~ age * gender,
  corr = corAR1(form = ~ time | id),
  data = data_long
)

# (e) heterogeneous ar1
model5 <- gls(
  distance ~ age * gender,
  corr = corAR1(form = ~ time | id),
  data = data_long,
  weights = varIdent(form = ~ 1 | time)
)

# now we need to pick a model, we want to minimize AIC, BIC and maximize logliklihood
# p-value in anova checks if more specific model is better than the more general one
# degrees of freedom are the number of vaules in teh final calculation of a statistic that are free to vary
# going by AIC model2 (compound symmetry) is the best model
# going by BIC model2 (compound symmetry) is again the best model
# so we want to use model2
# skip p-values, because it doesn't make logic sense
#    model1 (df=14) is not significantly better than model2 (df=6) at 0.05 level (p-value = 0.324)
#    but model5 (df=9) is significantly better than model2 (df=6) at 0.05 level (p-value = 0.0288) even though AIC and BIC for model2 is less than that for model5
