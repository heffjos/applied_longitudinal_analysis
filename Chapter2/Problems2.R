# For this problem set we focus only on the 50 children assigned to 
# chelation treatment with succimer. Thre raw data are stored in an external
# file: lead-data.txt
#
# Eacy row of the data set contains the following 5 variables:
#   ID  Y1  Y2  Y3  Y4
#

library(tidyverse)

col_names <- c("ID", "Y1", "Y2", "Y3", "Y4")
data <- read_table("lead-data.txt", col_names=col_names)

# 2.1.1
data_short <- gather(data, key=Timepoint, value=Levels, Y1, Y2, Y3, Y4)
data_short <- data_short %>%
  mutate(
    Week = 0,
    Week = ifelse(Timepoint == "Y2", 1, Week),
    Week = ifelse(Timepoint == "Y3", 4, Week),
    Week = ifelse(Timepoint == "Y4", 6, Week)
  )
    
data_summary <- data_short %>%
  group_by(Week) %>%
  summarize(
    mean = mean(Levels),
    variance = var(Levels),
    std = sqrt(variance)
  )

# 2.1.2
p <- data_summary %>%
  ggplot(aes(x=Week, y=mean)) +
  geom_line(linetype = "dashed") +
  geom_point()

# 2.1.3
data_cov <- cov(data[, c("Y1", "Y2", "Y3", "Y4")])
data_cor <- cor(data[, c("Y1", "Y2", "Y3", "Y4")])

# 2.1.4
# verified

# 2.1.5



