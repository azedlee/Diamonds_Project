## Scatter Plots
library(ggplot2)
pf <- pseudo_facebook

qplot(x = age, y = friend_count, data = pf)

## ggplot syntax + overplotting
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13,90) +
  coord_trans(y = "sqrt")

summary(pf$age)

## Alpha and Jitter
ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/25, position = position_jitter(h = 0)) +
  xlim(13,90) +
  coord_trans(y = "sqrt")

## Conditional Means
library(dplyr)

age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
pf.fc_by_age <- arrange(pf.fc_by_age, age)

head(pf.fc_by_age)

## Alternative
pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

head(pf.fc_by_age, 20)

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line() #connects the data points

## Overlaying Summaries with Row Data

ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/25, position = position_jitter(h = 0), color = 'orange') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = 0.1),
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = 0.5),
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = 0.9),
            linetype = 2, color = 'blue') +
  coord_cartesian(xlim = c(13,70), ylim = c(0, 1000))

quantile(pf$friendships_initiated, c(.25,.5,.75))

## Correlation
cor.test(pf$age, pf$friend_count, method = "pearson")
#alternative
with(pf, cor.test(age, friend_count, method = "p"))

## Correlation on Subsets
with(subset(pf, age <= 70), cor.test(age, friend_count), method = "pearson")

## Correlation Methods
with(subset(pf, age <= 70), cor.test(age, friend_count), method = 'spearman')

## Create Scatterplots
ggplot(aes(www_likes_received, likes_received), data = pf) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  coord_cartesian(xlim = c(0,quantile(pf$www_likes_received, 0.95)), #95% quantile
                  ylim = c(0,quantile(pf$likes_received, 0.95))) +
  geom_smooth(method = "lm", color = "red")

cor.test(pf$www_likes_received, pf$likes_received, method = "p")

## More Caution with Correlation

install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
ggplot(aes(Month, Temp), data = Mitchell) +
  geom_point()

cor.test(Mitchell$Month, Mitchell$Temp, method = "p")

## Making Sense of Data
ggplot(data = Mitchell, aes(x = (Month%%12), y = Temp)) +
  geom_point()
  
## Understanding Noise: Age to Age Months
ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()

head(pf.fc_by_age, 10)
pf.fc_by_age[17:19, ]

pf$age_with_months <- pf$age + (12 - pf$dob_month)/12

## Age with Months Means
pf.fc_by_age_months <- pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(as.numeric(friend_count)),
            n = n()) %>%
  arrange(age_with_months)

head(pf.fc_by_age_months)

ggplot(aes(x = age_with_months, y = friend_count_mean), 
       data = pf.fc_by_age_months) +
  geom_line(data = subset(pf.fc_by_age_months,age_with_months < 71)) + ##Subset with geom_point in new ggplot2
  geom_smooth(data = subset(pf.fc_by_age_months,age_with_months < 71))