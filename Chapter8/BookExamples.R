library(foreign)
library(tidyverse)
library(nlme)
library(modelr)

set.seed(1111111)

fev1 <- read.dta("../datasets/fev1.dta")

data <- as_tibble(fev1) %>%
  filter(id != 197) %>%
  mutate(
    loght = log(ht),
    logbht = log(baseht)
  )

# plot the data
p_fev <- data %>%
  ggplot(aes(x = age, y = logfev1 - loght, group = id)) +
  geom_line(color = "blue") +
  geom_point(shape = 1, size = 2) +
  geom_smooth(method = "loess") +
  labs(group = NULL, x = "age (yrs)", y = "Log(FEV1/Height)")

# pg 216 (table 8.2)
# linear mixed effects model (random intercept and slope for age)
model1 <- lme(
  logfev1 ~ age + loght + baseage + logbht, 
  data = data, 
  random = ~ age | id
)

# linear mixed effects model (random intercept and slope for log height)
model2 <- lme(
  logfev1 ~ age + loght + baseage + logbht, 
  data = data, 
  random = ~ loght | id
)

# linear mixed effects model (random intercept and slopes for age and log height)
model3 <- lme(
  logfev1 ~ age + loght + baseage + logbht, 
  data = data, 
  random = ~ age + loght | id
)

data <- data %>%
  add_residuals(model1, var = "resid1") %>%
  add_residuals(model2, var = "resid2") %>%
  add_residuals(model3, var = "resid3") %>%
  add_predictions(model1, var = "pred1") %>%
  add_predictions(model2, var = "pred2") %>%
  add_predictions(model3, var = "pred3")

# linear mixed effects model (random intercept and slope)
fat <- as_tibble(read.dta("../datasets/fat.dta"))

# make plot
p1_fat <- fat %>%
  ggplot(aes(x = time, y = pbf)) +
  geom_point() +
  geom_smooth(method = "loess")

fat <- fat %>%
  mutate(time0 = time * (time >= 0))
model1 <- lme(pbf ~ time + time0, data = fat, random = ~ time + time0 | id)

fat <- fat %>%
  add_predictions(model1)

fat_pop <- fat %>%
  count(time) %>%
  mutate(time0 = time * (time >= 0))

fat_pop <- fat_pop %>%
  mutate(pred = predict(model1, level = 0, fat_pop))

p2_fat <- ggplot(fat, aes(x = time, y = pred)) +
  geom_line(aes(group = id)) +
  geom_line(data = fat_pop, aes(x = time, y = pred), size = 4, color = "blue")

# linear mixed effects model (hybrid model with exponential serial correlation)
model2 <- lme(
  pbf ~ time + time0, 
  random = ~ 1 | id, 
  corr = corCAR1(, form = ~ time | id), 
  data = fat
)

cd4 <- read.dta("../datasets/cd4.dta")


