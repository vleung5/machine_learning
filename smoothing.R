#Smoothing

library(dslabs)
data("polls_2008")

qplot(day, margin, data = polls_2008)

#We will start to estimate the time trend in the popular vote of the 2008 election between Obama and McCain.
#Let's using regression, the only method we have learned up to now.
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)

#The line we see does not appear to describe the trend very well.
#Note that points above the fitted line, blue, and those below, red, are not evenly distributed.
#We therefore need an alternative, a more flexible approach.


#We will try using bin smoothing with a window
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


#We will now use a normal kernel or k-smooth function to provide a smoother estimate.
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#local weighted regression or loess permits us to consider larger windows.
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

fit$fitted


polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#ggplot uses loess and the gom smooth function
polls_2008 %>% ggplot(aes(day,margin)) +
  geom_point() +
  geom_smooth(color="red", span=0.15,
              method.args = list(degree=1))