library(ggplot2)
data(diamonds)
summary(diamonds)

#Price vs X
ggplot(data = diamonds, aes(x = x, y = price)) +
  geom_point()

#Price vs Depth w/ Adjustments
ggplot(data = diamonds, aes(x = depth, y = price)) +
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks = seq(0,80,2))

#Correlation of Depth vs Price
cor.test(diamonds$depth, diamonds$price)

#Carat vs Price
ggplot(data = diamonds, aes(x = carat *.99, y = price*.99)) +
  geom_point()

#Price vs Volume
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(data = diamonds, aes(x = volume, y = price)) +
  geom_point()

#Correlation of Price vs Volume, with volume of 0 or greater than or equal to 800
volume_subset <- subset(diamonds, volume != 0 & volume <= 800)
cor(x = volume_subset$volume, y = volume_subset$price)

#Price vs Volume w/ Adjustments
ggplot(data = volume_subset, aes(x = volume, y = price)) +
  geom_point()

#Mean Price by Clarity
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price, na.rm = FALSE),
            max_price = max(price, na.rm = FALSE),
            n = n()
            ) %>%
  arrange(clarity)

head(diamondsByClarity)

#Create 2 bar plots (diamonds_mp_by_clarity, diamonds_mp_by_color) and output image with grid.arrange()
library(gridExtra)
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

b1 <- ggplot(data = diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) +
  geom_bar(stat = 'identity') +
  ggtitle('Mean Price by Clarity')

b2 <- ggplot(data = diamonds_mp_by_color, aes(x = color, y = mean_price)) +
  geom_bar(stat = 'identity') +
  ggtitle('Mean Price by Color')

grid.arrange(b1, b2, ncol = 1)




