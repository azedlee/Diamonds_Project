library(ggplot2)
data(diamonds)
summary(diamonds)

d.df <- diamonds

## Histogram with Prices, colored by Cut, Faceted by Color
ggplot(aes(x = price), data = d.df) +
  geom_histogram(aes(color = cut)) +
  facet_wrap( ~ color) +
  scale_fill_brewer(type = 'qual')

## Scatterplot Price vs Table, colored by Cut
ggplot(aes(x = table, y = price), data = d.df) +
  geom_point(aes(color = cut)) + 
  scale_fill_brewer(type = 'qual') +
  coord_cartesian(xlim = c(50,70))

## Scatterplot Price vs Volume (x * y * z), color by Clarity, Y-Axis log10 of Price, Omit top 1% of diamond volumes
d.df$volume = d.df$x * d.df$y * d.df$z
ggplot(aes(x = (volume * .99), y = price), data = d.df) +
  geom_point(aes(color = clarity)) +
  coord_trans(y = 'log10') +
  coord_cartesian(xlim = c(0,1000))

## Create new variable in pseudo_facebook.tsv called prop_initiated
## Variable should contain proportion of friendships that the user initiated
pf <- pseudo_facebook
summary(pf)
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

## Line graph of the median prop_initiated vs tenure, color by year_joined.bucket
pf$year_joined <- floor(2014 - (df$tenure / 365))

pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004,2009,2011,2012,2014), include.lowest = FALSE)
table(pf$year_joined.bucket)

ggplot(aes(x = tenure, y = prop_initiated), data = pf) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

## Smoothed previous plot
ggplot(aes(x = tenure, y = prop_initiated), data = pf) +
  geom_smooth(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

## Scatterplot Price/Caret ratio, x = cut, colored by Color, Faceted by Clarity
d.df$pc.ratio <- d.df$price / d.df$carat
ggplot(data = d.df, aes(x = cut, y = pc.ratio)) +
  geom_point(aes(color = color)) +
  facet_wrap( ~ clarity)






