library(ggplot2)
library(svglite)
library(car)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(svglite)

df <- data.frame("metric" = c("transistor density", "TFLOPs", "1080p performance", "1600p performance"), "value" = c(37, 39, 40, 78))
df

b <- ggplot(df, aes(x=metric, y=value)) + geom_bar(aes(fill = metric), stat = "identity") + coord_flip() + labs(x = "metric", y = "percentage") + ggtitle("GTX 1180 Performance Gains") + scale_fill_brewer(palette="Spectral") +  geom_text(size = 5, aes(label = value), position = position_stack(vjust = 0.5))
b
ggsave(filename = "test.svg", plot=b)

df1 <- data.frame("card" = c("1080", "1180"), "transistor density" = c(14793507294, 20329985870), "TFLOPs" = c(8873.00, 12328.65), "1080p performance" = c(127, 177.4223), "1600p performance" = c(161, 226.2672))

d1 <- ggplot(df1, aes(x = card, y = transistor.density)) + geom_bar(stat = "identity", fill = "#2B83BA") + guides(fill=FALSE) + labs(y = "transistor density")
d2 <- ggplot(df1, aes(x = card, y = TFLOPs)) + geom_bar(stat = "identity", fill = "#D7191C")  + guides(fill=FALSE)
d3 <- ggplot(df1, aes(x = card, y = X1080p.performance)) + geom_bar(stat = "identity", fill = "#FDAE61")  + guides(fill=FALSE) + labs(y = "gaming performance")

finalgraphscomposite <- grid.arrange(d1,d2,d3, ncol =3, nrow =1)
ggsave(filename = "finalgraphscomposite.svg", plot=finalgraphscomposite)

