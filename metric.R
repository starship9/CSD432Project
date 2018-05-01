algoDF <- read.csv("Data/metrics.csv")
str(algoDF)
head(algoDF)

library(tidyverse)
library(RColorBrewer)

ggplot(data = algoDF,mapping = aes(x = X, y = auc)) + geom_point() + geom_line(aes(group = 1)) + ggtitle("AUC(Area under curve) for all algorithms") + xlab("")
ggplot(data = algoDF,mapping = aes(x = X, y = accuracy)) + geom_point() + geom_line(aes(group = 1)) + geom_text(aes(y = accuracy, label = accuracy,vjust = 1.2,hjust = 0.5)) + ggtitle("Accuracy for all algorithms") + xlab("")
ggplot(data = algoDF,mapping = aes(x = X, y = precision)) + geom_point() + geom_line(aes(group = 1)) + ggtitle("Precision for all algorithms") + xlab("") + geom_text(aes(y = precision, label = precision,hjust = 0.5, vjust = 1))
ggplot(data = algoDF,mapping = aes(x = X, y = recall)) + geom_point() + geom_line(aes(group = 1)) + ggtitle("Recall for all algorithms") + xlab("")
ggplot(data = algoDF,mapping = aes(x = X, y = specificity)) + geom_point() + geom_line(aes(group = 1)) + ggtitle("Specificity for all algorithms") + xlab("")
ggplot(data = algoDF,mapping = aes(x = X, y = f1Score)) + geom_point() + geom_line(aes(group = 1)) + ggtitle("f1scores for all algorithms") + xlab("")


