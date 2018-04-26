spam <-
  read.csv(
    "C:/Users/Nishank/Desktop/SNU/Project/Suspicious/Data/spam.csv",
    stringsAsFactors = FALSE
  )
deceptiveOpinion <-
  read.csv(
    "C:/Users/Nishank/Desktop/SNU/Project/Suspicious/Data/deceptive-opinion.csv",
    stringsAsFactors = FALSE
  )

library(tidyverse)

summary(spam)
summary(deceptiveOpinion)
str(spam)

spam <- spam[, 1:2]
str(deceptiveOpinion)

deceptiveOpinion <- deceptiveOpinion[, c(1, 5)]

#spam$v2 <- as.factor(spam$v2)
#class(spam$v2)

#deceptiveOpinion$deceptive <- as.factor(deceptiveOpinion$deceptive)


colnames(spam) <- c("suspicious", "text")
colnames(deceptiveOpinion) <- c("suspicious", "text")

summary(deceptiveOpinion)
#deceptiveOpinion <- deceptiveOpinion %>% filter(suspicious=="deceptive") %>% mutate(suspicious="spam")

deceptiveOpinion$suspicious[deceptiveOpinion$suspicious=="deceptive"] <- "suspicious"
deceptiveOpinion$suspicious[deceptiveOpinion$suspicious=="truthful"] <- "safe"

spam$suspicious[spam$suspicious=="spam"] <- "suspicious"
spam$suspicious[spam$suspicious=="ham"] <- "safe"

suspDF <- rbind(spam,deceptiveOpinion)
suspDF$suspicious <- as.factor(suspDF$suspicious)
summary(suspDF)


library(tm)

suspText <- Corpus(VectorSource(suspDF$text))
suspText <- tm_map(suspText,tolower)
suspText <- tm_map(suspText,removePunctuation)
suspText <- tm_map(suspText,removeNumbers)
suspText <- tm_map(suspText,removeWords,stopwords("english"))
suspText <- tm_map(suspText,stemDocument)

library(tidytext)

suspText <- tm_map(suspText,removeWords,c(as.character(stop_words)))
library(wordcloud)
wordcloud(suspText, min.freq = 75,random.order = FALSE,random.color = FALSE,colors = brewer.pal(9,"Spectral"))

library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(caTools)

frequencies = DocumentTermMatrix(suspText)
frequencies
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq=20)

sparse = removeSparseTerms(frequencies, 0.995)
sparse

suspSparse <- as.data.frame(as.matrix(sparse))
colnames(suspSparse) <- make.names(colnames(suspSparse))
suspSparse$suspicious <- suspDF$suspicious

set.seed(100)

trainIndex <- createDataPartition(suspSparse$suspicious,p = 0.7, list = FALSE, times = 1)
train <- suspSparse[trainIndex,]
test <- suspSparse[-trainIndex,]


logRegModel <- glm(suspicious~., family = "binomial",data = train)
summary(logRegModel)
plot(logRegModel$residuals)

logRegModelPred <- predict(logRegModel,newdata = test,type = "response")
table(logRegModelPred>0.5,test$suspicious)

library(Metrics)
#mse(test$suspicious,logRegModelPred)

#Accuracy: 91.49%
((354+1614)/(354+1614+110+73))

library(ROCR)
ROCRpred <- prediction(logRegModelPred,test$suspicious)
ROCRperf <- performance(ROCRpred,'tpr','fpr')
plot(ROCRperf,colorize = TRUE, text.adj = c(-0.2,1.7))

cartModel <- rpart(suspicious~., method = "class",data = train)
prp(cartModel)
fancyRpartPlot(cartModel,type = 2, cex = 0.6)
cartModelPred <- predict(cartModel,newdata = test,type = "class")
table(cartModelPred,test$suspicious)

#Accuracy: 87.2%
(1625+251)/(1625+251+213+62)

#10-fold cross validation
library(e1071)

trainControl <- trainControl(method = "cv",number = 10)
cp.grid <- expand.grid(.cp = (0:30)*0.0001)



cartCVModel <- train(suspicious~.,data = train, trControl = trainControl, method = "rpart",tuneGrid = cp.grid)
cartCVModel
plot(cartCVModel)

cartCVModelFinal <- rpart(suspicious~., cp = 0.0023, data = train, method = "class")
prp(cartCVModelFinal)

set.seed(100)
rfModel <- randomForest(suspicious~., data = train)
rfModelPred <- predict(rfModel, newdata = test)
table(rfModelPred, test$suspicious)

plot(rfModel)
#Accuracy: 93.67%
(1637+378)/(1637+378+86+50)

summary(rfModel)

library(rattle)
prp(cartCVModel$finalModel)
fancyRpartPlot(cartCVModel$finalModel, cex = 0.6, type = 2)
nbModel <- train(suspicious~.,data = train, trControl = trainControl, method = "naive_bayes")
summary(nbModel)
plot(nbModel$finalModel)

nbModelPred <- predict(nbModel, newdata = test)
table(nbModelPred,test$suspicious)
#Accuracy: 84.51%

#nbModel2 <- train(suspicious~., data = train, method = "naive_bayes", tuneGrid = data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0), laplace = 0.5), trControl = trainControl)

library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

library(forestFloor)
#plot(forestFloor(randomForest(suspicious~., data = train, keep.inbag = T), X = trai

library(klaR)
naiveModel <- naiveBayes(suspicious~., data = train)
plot(naiveModel)


suspSafe <- suspSparse %>% select() %>% filter(suspicious == "safe" && suspicious!="suspicious")
str(suspSafe$suspicious)
summary(suspSparse$suspicious)

suspSafe <- subset(suspSparse,suspicious=="safe")
summary(suspSparse$suspicious)
