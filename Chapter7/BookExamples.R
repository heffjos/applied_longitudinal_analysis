library(nlme)
library(tidyverse)
library(foreign)

ds <- read.dta("../datasets/exercise.dta") %>% as_tibble()
ds_long <- ds %>%
  gather(day_label, muscle_strength, y0:y12) %>%
  mutate(
    time = case_when(
      .$day_label == "y0" ~ 0,
      .$day_label == "y2" ~ 1,
      .$day_label == "y4" ~ 2,
      .$day_label == "y6" ~ 3,
      .$day_label == "y8" ~ 4,
      .$day_label == "y10" ~ 5,
      .$day_label == "y12" ~ 6
    ),
    day = time * 2
  ) %>%
  # filter out 1, 5 to match bootk dataset
  filter(!time %in% c(1, 5)) %>%
  mutate(
    day.f = factor(day, c(0, 4, 6, 8, 12)),
    group.f = factor(group, c(1, 2)),
    group.f2 = ifelse(group == 1, "repetitions", "weight") %>% factor(c("repetitions", "weight")),
    newtime = case_when(
      .$time == 0 ~ 1,
      .$time == 2 ~ 2,
      .$time == 3 ~ 3,
      .$time == 4 ~ 4,
      .$time == 6 ~ 5
    )
  )

# let's make a plot of the data
ds_long_summary <- ds_long  %>%
  group_by(group.f2, day) %>%
  summarize(mean_strength = mean(muscle_strength, na.rm = TRUE))

p1 <- ggplot(ds_long_summary, aes(x = as.integer(day), y = mean_strength, color = group.f2)) +
  geom_line() +
  geom_point(size = 2) +
  labs(y = "mean strength", x = "day", color = "method")

#unstructured covariance
model1 <- gls(muscle_strength ~ group.f * day.f, 
  data = ds_long,
  na.action = na.omit,
  corr = corSymm(, form = ~ newtime | id),
  weights = varIdent(form = ~ 1 | newtime)
)

# autoregressive covariance
model2 <- gls(muscle_strength ~ group.f * day.f,
  data = ds_long,
  na.action = na.omit,
  corr = corAR1(, form = ~ newtime | id)
)

# exponential covariance
model3 <- gls(muscle_strength ~ group.f * day.f, 
  data = ds_long,
  na.action = na.omit,
  corr = corExp(, form = ~ day | id)
)
