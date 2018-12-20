library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% group_by(sex, type) %>% summarize(n = n())


set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- dat[-test_index, ]
test_set <- dat[test_index, ]


y_hat <- ifelse(x == "inclass", "Female", "Male")
y_hat

mean(y_hat == y)


table(predicted = y_hat, actual = y)


dat %>% count(sex)