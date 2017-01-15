#complete algorithm as function
TreeGrows <- function(trainset, features, output){
  
  #function which test all features for splitting and selects the best one
  find_best_split <- function(data, features, output){
    gini_set <- numeric(length(features))
    for (i in 1:length(features)){
      t <- table(data[, output], data[, features[i]])
      gini_column <- apply(apply(t, 2, function(x){prop.table(x)^2}), 2, function(y){1-sum(y)})
      gini_sum <- apply(t, 2, function(x){sum(x)/sum(t)})
      gini_set[i] <- sum(gini_column * gini_sum)
      
    }
    return(features[which.min(gini_set)])
  }
  
  #function which creates new nodes and saves information about children and parents
  createNode <- function(node, best_split, output){
    nextlevel <- split(node, node[, best_split])
    nextlevel <- lapply(nextlevel, function(x){x[, -which(colnames(x)==best_split)]})
    proportions <- lapply(nextlevel, function(x){prop.table(table(x[, output]))})
    classes <- data.frame(cbind(names(proportions), lapply(proportions, function(x){names(which.max(x))})))
    colnames(classes) <- c(best_split, 'y')
    continue <- lapply(proportions, function(x){max(x)<0.9})
    output <- list(nextlevel=nextlevel, classes=classes, continue=continue)
  }
  
  #find the first split
  best_split <- find_best_split(trainset, features, 'y')
  #create root node
  node1 <- createNode(trainset, best_split, 'y')
  #save rules of splitting
  rules <- node1$continue
  #save feature of splitting
  splits <- best_split
  #save outcome classes
  classes <- node1$classes
  
  #repeat the same for the next levels
  while (length(splits) < (length(features)-1)){
    best_split <- find_best_split(trainset, features[which(!features %in% c(splits, 'y'))], 'y')
    node2 <- lapply(node1[[1]], function(x){createNode(x, best_split, 'y')})
    splits <- cbind(splits, best_split)
    classes_temp <- do.call(rbind, lapply(node2, function(x){x$classes}))
    classes <- list(classes, classes_temp)
    rules <- lapply(node2, function(x){x$continue})
    node1$nextlevel <- do.call(c, lapply(node2, function(x){x$nextlevel}))
    node1$classes <- do.call(rbind, lapply(node2, function(x){x$classes}))
    node1$continue <- do.call(c, lapply(node2, function(x){x$continue}))
  }
  
  #select results and reshape them to the frame
  res <- rownames(node1$classes)
  res <- strsplit(res,'[.]')
  suppressWarnings(res <- data.frame(do.call(rbind, lapply(res, function(x){x}))))
  colnames(res) <- splits
  res <- data.frame(res)
  out <- lapply(node1$nextlevel, function(x){data.frame(table(x))})
  out <- lapply(out, function(x){x[which.max(x$Freq), 1:2]})
  out <- do.call(rbind, lapply(out, function(x){x}))
  out <- data.frame(out)
  final <- cbind(res, out)
  rownames(final) <- NULL
  final[, colnames(final)] <- sapply(final[, colnames(final)], as.character)
  final <- final[which(apply(apply(final, c(1,2), function(x){as.numeric(grepl(x, '-Inf'))}), 1, sum)==0),]
  final <- final[, colnames(trainset)]
  return(final)
}

PredictTree <- function(tree, test_data, output){
  predicted_output <- merge(test_data, tree, by = colnames(test_data), all.x = TRUE)
  return(predicted_output$y)
}

#### test code ####

#read data
datafortask <- read.csv('bank.csv', sep = ';')
#select categorical columns
categorical_data <- datafortask[, sapply(datafortask, is.factor)]
#this row let us select specific features except column 10. it is output
categorical_data <- categorical_data[, c(4:10)]

#### 10-folds running ####
accuracy <- NULL
recall <- NULL
precision <- NULL
 for (i in 1:10){
   #create partition
   train_indices <- sample(nrow(categorical_data), nrow(categorical_data)*0.8)
   #select train dataset
   trainset <- categorical_data[train_indices,]
   #select test dataset
   testset <- categorical_data[-train_indices,]
   #Grow tree
   tree <- TreeGrows(trainset, colnames(trainset)[1:dim(trainset)[2]-1], 'y')
   #run prediction
   test_predict <- PredictTree(tree, testset[, 1:dim(trainset)[2]-1], 'y')
   #create contingency table
   t <- table(testset$y, test_predict)
   #calculate accuaracy
   accuracy <- c(accuracy, sum(diag(t))/sum(t))
   #calculate recall
   recall <- c(recall, t[1,1]/(t[1,1] + t[2,1]))
   #calculate precision
   precision <- c(precision, t[1,1]/(t[1,1] + t[1,2]))
 }
