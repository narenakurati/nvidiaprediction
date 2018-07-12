library(ggplot2)
library(svglite)
df <- data.frame("metric" = c("transistor density", "TFLOPs", "1080p performance", "1600p performance"), "value" = c(37, 39, 40, 78))
b <- ggplot(df, aes(x=metric, y=value)) + geom_bar(aes(fill = cat), stat = "identity") + coord_flip() + labs(x = "metric", y = "percentage") + ggtitle("GTX 1180 Performance Gains") + scale_fill_brewer(palette="Spectral")
b
ggsave(filename = "test.svg", plot=b)
