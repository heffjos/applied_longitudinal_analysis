# The NCGS serum cholesterol data are stored in an external file: cholesterol-data.txt.
#
# Each row of the data set contains the folloiwng seven variables:
#
# Group ID Y1 Y2 Y3 Y4 Y5
#
library(tidyverse)
library(foreign)
library(nlme)
library(stringr)
library(modelr)
library(forcats)

# 5.1.1
data <- read.dta("cholesterol.dta")
data <- as_tibble(data)

data <- data %>%
  gather(key = "timepoint", value = "serum", y1:y5) %>%
  mutate(
    months = case_when(
      .$timepoint == "y1" ~ 0,
      .$timepoint == "y2" ~ 6,
      .$timepoint == "y3" ~ 12,
      .$timepoint == "y4" ~ 20,
      .$timepoint == "y5" ~ 24,
      TRUE ~ NA_real_
    ),
    time = case_when(
      .$timepoint == "y1" ~ 1,
      .$timepoint == "y2" ~ 2,
      .$timepoint == "y3" ~ 3,
      .$timepoint == "y4" ~ 4,
      .$timepoint == "y5" ~ 5,
      TRUE ~ NA_real_
    ),
    groups = ifelse(group == 1, "high_dose", "placebo")
  ) %>%
  select(-group, -timepoint)

# 5.1.2
# Calculate the sample means, standard deviations, and variances of the serum chelestoerl levels at each occasion for each treament group.
data_summary <- data %>%
  group_by(groups, months) %>%
  summarize(
    mean_serum = mean(serum, na.rm = TRUE),
    var_serum = var(serum, na.rm = TRUE),
    std_serum = sqrt(var_serum)
  ) 

# 5.1.3
# On a singel graph, construct a time plot that displyas the mean serum cholesterol versus time (in months) for the two treatment groups. Describe the general characteristics for he time trends for the two groups.
p1 <- ggplot(data_summary, aes(x = months, y = mean_serum, color = factor(groups))) +
  geom_point() +
  geom_line() +
  labs(color = "groups")

# 5.1.4
# this was done under 5.1.1

# 5.1.5
# Assuming an unstructured covariance matrix, conduct an analysis of repsonse profiles.
# Deterimine whether the patterns of chnage over time differ in the two treatment groups 
#
# We will use gls (generalize least squares) from nlme package.
# The equation will include groups, months and the interaction between groups and months.
# The correlation estimate will be that between time and id. This is estimated for each
# id, so one for each participant in the data. 

data <- data %>% 
  ungroup() %>%
  mutate(
    months.f = factor(months),
    groups.f = factor(groups, levels = c("placebo", "high_dose"))
  ) %>%
  arrange(groups.f, months.f)

model <- gls(
  serum ~ groups.f*months.f,
  data = data, 
  corr = corSymm(, form = ~time | id),
  weights = varIdent(form = ~ 1 | months.f),
  na.action = na.omit
)

# 5.1.6
# Display the estimated 5 x 5 covariance and correlation matrices for the five repeated measurements fo serum cholesterol.
# summary(model)
# getVarCov(model)

# 5.1.7  
# Wtih baseline (month 0) and the placebo group (group 2) as the reference group, write out the regression model for mean serum cholesterol that corresponds to the analysis of response rofiles in Problem 5.1.5.
#
# This can be done by simply printing out the model

# 5.1.8
# Let L denote a matrix of known weights and beta the vector of linear regression parameters from the model assumed in Problem 5.1.7. The null hypothesis that the patterns of change over time do not differ in the two treament groups can be expressed as H0: L*beta = 0. Describe an appropriate weight matrix L for this null hypothesis.
#
# In our model the contrast looks like this:
#
# 0 0 0 0 0 0 1 -1 0 0
# 0 0 0 0 0 0 0 1 -1 0
# 0 0 0 0 0 0 0 0 1 -1

# 5.1.9
# Show how the estimated regression coefficients from an analysis of response profiles can be used to construct the time specific means in the two groups. Campare these estimated means with the sample means obtained in problem 5.1.2.
#
# The beta values are the estimated means. In our model the estimated time means for both groups are calculated using the following equations:
#
# placebo.f0 = B1
# placebo.f6 = B1 + B3
# placebo.f12 = B1 + B4
# placebo.f20 = B1 + B5
# placebo.f24 = B1 + B6
#
# high_dose.f0 = B1 + B2 
# high_dose.f6 = B1 + B2 + B3 + B7
# high_dose.f12 = B1 + B2 + B4 + B8
# high_dose.f20 = B1 + B2 + B5 + B9
# high_dose.f24 = B1 + B2 + B6 + B10
model.means <- tribble(
  ~ value, ~ groups, ~ months, ~ mean_serum, 
  "model.est", "placebo", 0, coef(model)[1],
  "model.est", "placebo", 6, coef(model)[1] + coef(model)[3],
  "model.est", "placebo", 12, coef(model)[1] + coef(model)[4],
  "model.est", "placebo", 20, coef(model)[1] + coef(model)[5],
  "model.est", "placebo", 24, coef(model)[1] + coef(model)[6],
  "model.est", "high_dose", 0, coef(model)[1] + coef(model)[2],
  "model.est", "high_dose", 6, coef(model)[1] + coef(model)[2] + coef(model)[3] + coef(model)[7],
  "model.est", "high_dose", 12, coef(model)[1] + coef(model)[2] + coef(model)[4] + coef(model)[8],
  "model.est", "high_dose", 20, coef(model)[1] + coef(model)[2] + coef(model)[5] + coef(model)[9],
  "model.est", "high_dose", 24, coef(model)[1] + coef(model)[2] + coef(model)[6] + coef(model)[10]
)

p2 <- data_summary %>% 
  mutate(value = "mean") %>%
  select(value, groups, months, mean_serum) %>%
  bind_rows(model.means) %>%
  mutate(groups.months = fct_inorder(str_c(groups, months, sep = "."))) %>%
  ggplot(aes(x = groups.months, y = mean_serum, color = value)) +
  geom_jitter(width = 0.15, height = 0) +
  theme(axis.text.x = element_text(angle = 90))

# 5.1.10
# With baseline (month 0) and the placebo group (group 2) as the reference group, provide an interpretation for each of the estimated regression coefficients in terms of the ffect of the treatments on the patterns of change in mean serum cholesterol.
#

data <- data %>%
  mutate(groups.months = fct_inorder(str_c(groups, months, sep = ".")))

model.means <- model.means %>%
  mutate(groups.months = fct_inorder(str_c(groups, months, sep = ".")))

b <- unname(coef(model))

beta.interp <- tribble(
  ~ x, ~ y, ~ xend, ~ yend, ~ effect,
  "placebo.6", b[1], "placebo.6", b[1] + b[3], "6",
  "placebo.12", b[1], "placebo.12", b[1] + b[4], "12",
  "placebo.20", b[1], "placebo.20", b[1] + b[5], "20",
  "placebo.24", b[1], "placebo.24", b[1] + b[6], "24",
  "high_dose.0", b[1], "high_dose.0", b[1] + b[2], "high_dose",
  "high_dose.6", b[1], "high_dose.6", b[1] + b[2], "6",
  "high_dose.6", b[1] + b[2], "high_dose.6", (b[1] + b[2]) + b[3], "6",
  "high_dose.6", (b[1] + b[2]) + b[3], "high_dose.6", (b[1] + b[2]) + b[3] + b[7], "high_dose:6",
  "high_dose.12", b[1], "high_dose.12", b[1] + b[2], "high_dose",
  "high_dose.12", (b[1] + b[2]), "high_dose.12", (b[1] + b[2]) + b[4], "12",
  "high_dose.12", (b[1] + b[2]) + b[4], "high_dose.12", (b[1] + b[2]) + b[4] + b[8], "high_dose:12",
  "high_dose.20", b[1], "high_dose.20", b[1] + b[2], "high_dose",
  "high_dose.20", (b[1] + b[2]), "high_dose.20", (b[1] + b[2]) + b[5], "20",
  "high_dose.20", (b[1] + b[2]) + b[5], "high_dose.20", (b[1] + b[2]) + b[5] + b[9], "high_dose:20",
  "high_dose.24", b[1], "high_dose.24", b[1] + b[2], "high_dose",
  "high_dose.24", (b[1] + b[2]), "high_dose.24", (b[1] + b[2]) + b[6], "24",
  "high_dose.24", (b[1] + b[2]) + b[6], "high_dose.24", (b[1] + b[2]) + b[6] + b[10], "high_dose:24"
)

p3 = ggplot(data, aes(x = groups.months, y = serum)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_ref_line(
    h = model.means %>% 
      filter(groups.months == "placebo.0") %>% 
      .["mean_serum"] %>%
      as.numeric(),
    colour = "darkgreen"
  ) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend, color = effect),
    data = beta.interp,
    size = 3,
    arrow = arrow(length = unit(0.25, "cm"))
  )
    
