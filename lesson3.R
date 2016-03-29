##Reading in Data
getwd()
list.files()
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)

##Histograms of Users' Birthdays
install.packages('ggplot2')
library(ggplot2)
ggplot(aes(x = dob_day), data = pf) +
  geom_bar() +
  scale_x_discrete(breaks = 1:31) +
  facet_wrap(~dob_month, ncol = 3)

##Friend count
names(pf)
qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 25) + ##can also use na.omit(pf)
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

##Tenure
names(pf)
qplot(x = tenure/365, data = pf, binwidth = .25,
      xlab = 'Number of years using Facebook',
      ylab = 'Number of users in sample',
      color = I('black'), fill = I("#099DD9")) + 
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))

##User Ages
names(pf)
qplot(x = age, data = pf, binwidth = 1, 
      color = I('black'), fill = I("#099DD9")) +
  scale_x_continuous(limits = c(10, 70), breaks = seq(10, 70, 10))

##Transforming Data
qplot(x = friend_count, data = pf)
summary(pf$friend_count)
summary(log10(pf$friend_count + 1))
summary(sqrt(pf$friend_count))

install.packages("gridExtra")
library(gridExtra)

#Original Coding Way
scaleOriginal = qplot(x = friend_count, data = pf) + ggtitle('Original')
scaleLog = qplot(x = log10(friend_count + 1), data = pf) +
        ggtitle('Log10')
scaleSqrt = qplot(x = sqrt(friend_count), data = pf) +
        ggtitle('Square Root')
grid.arrange(scaleOriginal, scaleLog, scaleSqrt, ncol = 1)

#Better Coding Way
pOriginal = ggplot(aes(x = friend_count), data = pf) + geom_histogram()
pLog = pOriginal + scale_x_log10()
pSqrt = pOriginal + scale_x_sqrt()
grid.arrange(pOriginal, pLog, pSqrt, ncol = 1)

grid.arrange(scaleLog, pLog, ncol = 1)

##Frequency Polygons
names(pf)
qplot(x = friend_count, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      xlab = 'Friend Count',
      ylab = 'Proportion of Users with that friend count',
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

qplot(x = www_likes, y = ..count../sum(..count..),
      data = pf,
      xlab = 'Like Count',
      ylab = 'Propotions of Users with like count',
      geom = 'freqpoly', color = gender) +
  scale_x_continuous() +
  scale_x_log10()

by(pf$www_likes, pf$gender, sum)

##Box Plots
qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

qplot(x = gender, y = friend_count, data = (subset(pf, !is.na(gender))), 
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 250)) #will not remove rows, but re-adjust the scale

by(pf$friend_count, pf$gender, summary)

##Getting Logical
summary(pf$mobile_likes)

summary(pf$mobile_likes > 0)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

mobile_check_in_percent <- NA
mobile_check_in_percent <- sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)
mobile_check_in_percent


