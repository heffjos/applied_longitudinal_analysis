library(foreign)
library(tidyverse)
library(nlme)
library(modelr)

smoking <- read.dta("../datasets/smoking.dta")

smoking_new <- as_tibble(smoking) %>%
  mutate(
    smoker_status = ifelse(smoker, "current", "former"),
    smoker_status = factor(smoker_status, levels = c("former", "current"))
  )

smoking_summary <- smoking_new %>%
  group_by(smoker_status, time) %>%
  summarize(mean_fev1 = mean(fev1))

# make a plot
p1 <- smoking_summary %>%
  ggplot(aes(x = time, y = mean_fev1, color = smoker_status)) +
  geom_line() +
  geom_point() +
  labs(color = "smoker", x = "Year", y = "fev1")

# prepare for models
smoking_new <- smoking_new %>%
  mutate(
    year = case_when(
      .$time == 0 ~ 1,
      .$time == 3 ~ 2,
      .$time == 6 ~ 3,
      .$time == 9 ~ 4,
      .$time == 12 ~ 5,
      .$time == 15 ~ 6,
      .$time == 19 ~ 7
    )
  )

# linear trend model
model <- gls(fev1 ~ smoker_status*time, corr = corSymm(, form = ~ year | id),
  data = smoking_new, weights = varIdent(form = ~ 1 | year))

grid <- data_grid(smoking_new, smoker_status, time) %>%
  add_predictions(model)

p2 <- ggplot(smoking_summary, aes(x = time, y = mean_fev1, color = smoker_status)) +
  geom_point(show.legend = FALSE) +
  geom_line(data = grid, aes(y = pred), size = 2) 

# linear trend model
model1 <- gls(fev1 ~ smoker_status*time, 
  corr = corSymm(, form = ~ year | id),
  weights = varIdent(form = ~ 1 | year), 
  data = smoking_new, method = "ML"
)

# quadratic trend model
model2 <- gls(fev1 ~ smoker*time + smoker*I(time^2),
  corr = corSymm(, form = ~ year | id),
  weights = varIdent(form = ~ 1 | year), 
  data = smoking_new, method = "ML"
)

# now work with lead exposed children, piecewise linear model
lead <- read.dta("../datasets/tlc.dta") %>%
  as_tibble()

lead_new <- lead %>%
  gather(timepoint, value, y0, y1, y4, y6) %>%
  mutate(
    time = case_when(
      .$timepoint == "y0" ~ 1,
      .$timepoint == "y1" ~ 2,
      .$timepoint == "y4" ~ 3,
      .$timepoint == "y6" ~ 4
    ),
    week = case_when(
      .$timepoint == "y0" ~ 0,
      .$timepoint == "y1" ~ 1,
      .$timepoint == "y4" ~ 4,
      .$timepoint == "y6" ~ 6
    ),
    week1 = (week - 1) * (week >= 1),
    trt.week = week * (trt == "Succimer"),
    trt.week1 = week1 * (trt == "Succimer")
  ) 

lead_new_summary <- lead_new %>%
  group_by(trt, week) %>%
  summarize(mean_value = mean(value))

model1 <- gls(value ~ week + week1 + trt.week + trt.week1,
  corr = corSymm(, form = ~ time | id),
  weights = varIdent(form = ~ 1 | time),
  data = lead_new
)

grid1 <- data_grid(lead_new, trt, week) %>%
  mutate(
    week1 = (week - 1) * (week >= 1),
    trt.week = week * (trt == "Succimer"),
    trt.week1 = week1 * (trt == "Succimer")
  ) %>%
  add_predictions(model1)

p3 <- ggplot(lead_new_summary, aes(x = week, y = mean_value, color = trt, group = trt)) +
  geom_point(show.legend = FALSE) +
  geom_line(color = "black") +
  geom_line(data = grid1, aes(y = pred))

model2 <- gls(value ~ week + week1 + trt:week + trt:week1,
  corr = corSymm(, form = ~ time | id),
  weights = varIdent(form = ~ 1 | time),
  data = lead_new
)

grid2 <- data_grid(lead_new, trt, week) %>%
  mutate(week1 = (week - 1) * (week >= 1)) %>%
  add_predictions(model2)

p4 <- ggplot(lead_new_summary, aes(x = week, y = mean_value, color = trt, group = trt)) +
  geom_point(show.legend = FALSE) +
  geom_line(color = "black") +
  geom_line(data = grid2, aes(y = pred))
