library(ggplot2)
library(gridExtra)
data(diamonds)
summary(diamonds)

price_per_carat <- NA
diamonds$price_per_carat <- diamonds$price / diamonds$carat
diamonds$price_per_carat

price_per_carat_by_color <- qplot(x = color, y = price_per_carat, data = diamonds,
                            geom = 'boxplot', color = color) +
                            coord_cartesian(ylim = c(1000,6000)) +
                            ggtitle('Price per carat by Color')

price_per_carat_by_cut <- qplot(x = cut, y = price_per_carat, data = diamonds,
                          geom = 'boxplot', color = cut) +
                          coord_cartesian(ylim = c(1000, 6000)) +
                          ggtitle('Price per carat by Cut')

grid.arrange(price_per_carat_by_color, price_per_carat_by_cut)
ggsave('diamonds_Color_Cut.png')
