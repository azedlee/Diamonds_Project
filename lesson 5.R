df <- pseudo_facebook
library(ggplot2)
library(dplyr)

## Third Qualitative Variable

ggplot(aes(x = gender, y = age), data = df) +
  geom_boxplot(data = subset(df, !is.na(gender))) +
  stat_summary(fun.y = mean, geom = 'point', shape = 4)

ggplot(aes(x = age, y = friend_count), data = df) +
  geom_line(data = subset(df, !is.na(gender)), aes(color = gender), stat = 'summary', fun.y = median)

pf.fc_by_age_gender <- df %>%
  group_by(age, gender) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(gender)) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>%
  ungroup() %>%
  arrange(age, gender)
head(pf.fc_by_age_gender)

## Plotting Conditional Summaries - Median_friend_count over age by gender

ggplot(aes(x = age, y = median_friend_count), data = pf.fc_by_age_gender) +
  geom_line(data = subset(pf.fc_by_age_gender, !is.na(gender)), aes(color = gender))

## Reshaping Data
install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, age ~ gender, value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)

pf.fc_by_age_gender.wide$ratio_gender = pf.fc_by_age_gender.wide$female/pf.fc_by_age_gender.wide$male
ggplot(data = pf.fc_by_age_gender.wide, aes(x = age, y = ratio_gender)) +
  geom_line() +
  geom_hline(linetype = 2, yintercept = 1, color = 'red')

## Third Quantitative Variable

#Tenure is in days
df$year_joined <- floor(2014 - (df$tenure / 365))

## Cut a Variable
summary(df$year_joined)
table(df$year_joined)

df$year_joined.bucket <- cut(df$year_joined, breaks = c(2004,2009,2011,2012,2014), include.lowest = FALSE)
table(df$year_joined.bucket)

## Plotting it All Together

table(df$year_joined.bucket, useNA = 'ifany')

ggplot(aes(x = age, y = friend_count), data = df) +
  geom_line(data = subset(df, !is.na(year_joined.bucket)), aes(color = year_joined.bucket),
            stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2) #grand mean friend count vs age

## Friending Rate

# Subset the data so at least 1 day of tenure
tenure_subset <- subset(df, tenure > 0)
tenure_subset$friending_rate <- tenure_subset$friend_count / tenure_subset$tenure
summary(tenure_subset$friending_rate)

ggplot(data = df, aes(x = tenure, y = friendships_initiated/tenure)) +
  geom_smooth(aes(color = df$year_joined.bucket))

## And now for something completely different...
yo <- read.csv('yogurt.csv')
str(yo)

yo$id <- factor(yo$id)
str(yo)

ggplot(data = yo, aes(x = price)) + geom_histogram()
unique(yo$price)
table(yo$price)

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
ggplot(data = yo, aes(x = all.purchases)) + geom_histogram(binwidth = 1)

ggplot(data = yo, aes(x = time%%60, y = price)) +
  geom_smooth(method = 'lm')

## Looking at Samples of Households
set.seed(1234)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price), data = subset(yo, id %in% sample.ids)) +
  facet_wrap( ~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)

## Scatterplot Matrices
install.packages('GGally')
library(GGally)
theme_set(theme_minimal(20))

# set the seed for reproducible results
set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ]) #loads ~5minutes

## Even more Variables
nci <- read.table('nci.tsv')

#changing the colnames to produce a nicer plot
colnames(nci) <- c(1:64)

## Create a Heat Map!

#melt the data to long format
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200, ]))
names(nci.long.samp) <- c('gene', 'case', 'value')
head(nci.long.samp)

#make the heat map
ggplot(aes(y = gene, x = case, fill = value), data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red'))(100))


