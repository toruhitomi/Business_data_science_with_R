# Preparation ####
library(tidyverse)

# Load data ####
d <- read_csv("data/train_data.csv", col_types = cols())
d

# Visualization ####
# Time-series
d %>% 
  distinct(date) %>% 
  pull(date)

# Convert date as a propoer date variable
d2 <- d %>% 
  separate(date, into = c("month", "day"), sep = "/") %>% 
  mutate_at(.vars = vars(month, day), .funs = as.numeric) %>% 
  mutate(year = ifelse(month >= 4, 2020, 2021)) %>% 
  mutate(date = sprintf("%d/%d/%d", year, month, day)) %>% 
  mutate(date = as.Date.character(date, tryFormats = "%Y/%m/%d", tz = "Japan"))

d2 %>% 
  ggplot(aes(x = date, y = rain)) +
  geom_line() +
  geom_point()

d2 %>% 
  select(date, highest, lowest) %>% 
  pivot_longer(cols = c(highest, lowest), names_to = "data_type", values_to = "temperature") %>% 
  ggplot(aes(x = date, y = temperature, color = data_type)) +
  geom_line() +
  scale_color_manual(values = c("#FF6A6A", "#63B8FF"))
  

