CheckArguments <- function(x, ...) UseMethod("CheckArguments")

# This function checks to see if everything is fine with the input or not.
# For example it checks to see if all the samples in a block are from a same class
CheckArguments.Pheno <- function(object){

  data <- object$data
  x <- object$x
  y <- object$y
  label <- object$label
  defaultLabel <- object$defaultLabel
  block <- object$block
  ncluster <- object$ncluster
  orderby <- object$orderby
  method <- object$method
  step <- object$step
  width <- object$width
  nfeatures <- object$nfeatures

  # check data argument
  if(is.numeric(nrow(data)) != TRUE)
    stop("invalid 'data'")
  if(nrow(na.omit(data)) == 0)
    stop("invalid 'data' after removing NA vaule")

  # check x, y arguments
  if((is.null(x) | is.null(y)) == TRUE)
    stop("invalid 'x' or 'y'")

  # check names, replicated names are allowed by using [[]]
  varNames <- colnames(data)
  inputNames <- c(x, y, label, block, orderby)
  if(all(inputNames %in% varNames) != TRUE)
    stop("invalid argument names")

  # check label and defaultLabel arguments
  if(!is.null(label) & is.null(defaultLabel))
    stop("invalid 'defaultLabel' but 'label' exists")
  if(!is.null(defaultLabel) & is.null(label))
    stop("invalid 'label' but 'defaultLabel' exists")
  if(!is.null(label) & length(label) >= 2)
    stop("invalid 'label', at most 1 dimension")
  if(!is.null(label)){
    if(length(unique(data[[label]])) < 2)
      stop("invalid 'label', which at least has two different components")
    if(!is.null(defaultLabel) & length(defaultLabel) >= length(unique(data[[label]])))
      stop("invalid 'defaultLabel', it must have less components than 'label'")
    if(!is.null(defaultLabel) & !(defaultLabel %in% unique(data[[label]])))
      stop("invalid 'defaultLabel', which should be any component of 'label'")
  }

  # check ncluster argument
  if(is.null(label) & is.null(ncluster))
    stop("invalid 'ncluster', the number of clusters")

  # check pcNumber argument
  #	if(iis.null(pcNumber))
  #		stop("invalid 'pcNumber', the number of principle components")
  #	if(!(pcNumber == round(pcNumber)) | (pcNumber < 0))
  #		stop("invalid 'pcNumber', it must be a positive integer")

  # check block argument
  if(!is.null(block) & length(block) > 2)
    stop("invalid 'block', at most 2 dimensions")

  # check orderby argument
  if(!is.null(orderby) & length(orderby) >= 2)
    stop("invalid 'orderby', at most 1 dimension")

  # check method argument
  if(is.null(method) | length(method) != 1)
    stop("invalid 'method', at most 1 dimension")
  if(!(method %in% c("SVM", "RF", "KMEANS")))
    stop("invalid 'method', only 'SVM', 'RF', 'KMEANS' are available")
  if(!is.null(label) & (method %in% c("KMEANS")))
    stop("invalid 'method', must be 'SVM' or 'RF' due to existing 'label'")
  if(is.null(label) & (method %in% c("SVM", "RF")))
    stop("invalid 'method', must be 'KMEANS' due to missing 'label'")

  # check step, width, nfeatures arguments
  if(!(step == round(step)) | (step < 0))
    stop("invalid 'step', it must be a positive integer")
  if(!(width == round(width)) | (width < 0))
    stop("invalid 'width', it must be a positive integer")
  if(!is.null(nfeatures)){
    if(!(nfeatures == round(nfeatures)) | (nfeatures < 0))
      stop("invalid 'nfeatures', it must be a positive integer")
  }
}

CheckArguments.predictPheno <- function(object){

  data <- object$data
  x <- object$x
  y <- object$y
  block <- object$block
  orderby <- object$orderby
  step <- object$step
  width <- object$width

  # check data argument
  if(is.numeric(nrow(data)) != TRUE)
    stop("invalid 'data'")
  if(nrow(na.omit(data)) == 0)
    stop("invalid 'data' after removing NA vaule")

  # check x, y arguments
  if((is.null(x) | is.null(y)) == TRUE)
    stop("invalid 'x' or 'y'")

  # check names, replicated names are allowed by using [[]]
  varNames <- colnames(data)
  inputNames <- c(x, y, block, orderby)
  if(all(inputNames %in% varNames) != TRUE)
    stop("invalid argument names")

  # check block argument
  if(!is.null(block) & length(block) > 2)
    stop("invalid 'block', at most 2 dimensions")

  # check orderby argument
  if(!is.null(orderby) & length(orderby) >= 2)
    stop("invalid 'orderby', at most 1 dimension")

  # check step, width, nfeatures arguments
  if(!(step == round(step)) | (step < 0))
    stop("invalid 'step', it must be a positive integer")
  if(!(width == round(width)) | (width < 0))
    stop("invalid 'width', it must be a positive integer")

}

SplitData <- function(data, block, orderby,
                      testBlockProp, blockOrderedLabels, blockOrderedNames){

  blockDataFrame <- data.frame(Name = blockOrderedNames,
                               Label = blockOrderedLabels)                            #blockDataFrame will be a two column data frame: the first column is block name (like A1) and the second is the label (like H)

  splitDatabyLabels <- split(blockDataFrame, as.vector(blockDataFrame$Label)) #splitDatabyLabels is a list containing seperate blocks based on their labels; for example if labels are H and D the splitDatabyLabels$H shows all blocks with H as their labels
  countSplitDatabyLabels <- sapply(splitDatabyLabels, nrow, simplify = TRUE)
  countNamesTemp <- names(countSplitDatabyLabels)
  countLabelsTemp <- as.vector(countSplitDatabyLabels)
  testSizes <- ceiling(countLabelsTemp * testBlockProp)

  #The following couple lines of codes can be used to implement a Leave One Out strategy.
   # rnd_block_test<-sample(0:1,1)
   # if(rnd_block_test==1){
   #   testSizes[1]<-0
   # }else{
   #   testSizes[2]<-0
   # }

  FUNSAMPLE <- function(x, y){
    return(sample(x, size = y, replace = FALSE))
  }
  testSample <- mapply(FUNSAMPLE, x = countLabelsTemp, y = testSizes,
                       SIMPLIFY = FALSE)
  names(testSample) <- countNamesTemp
  testNames <- c()
  for(i in seq(length(countNamesTemp))){
    testNames <- c(testNames,
                   splitDatabyLabels[[i]][testSample[[i]], ]$Name)
  }
  trainNames <- seq(nrow(blockDataFrame))[- testNames]
  testNames <- blockDataFrame$Name[mixedsort(testNames)]
  trainNames <- blockDataFrame$Name[mixedsort(trainNames)]

  testData <- data[which(data[[block]] %in% testNames), ]
  testData <- testData[order(testData[[orderby]]), ]
  trainData <- data[which(data[[block]] %in% trainNames), ]
  trainData <- trainData[order(trainData[[orderby]]), ]
  if(nrow(testData)==0){
    print("hi")
  }
  returnData <- list(test = testData, train = trainData,
                     testName = testNames)
  return(list(data = returnData))
}

#This function concatenate block[0] (for example Row) and block[1] (for example Run) and add this as a new column named "blockTemp".
#Finally it returns a subset of data only including columns such as : x, y, label, blockTemp, orderby
TransferData <- function(data, x, y, label, block, orderby){

  # to data frame
  WorkingData <- as.data.frame(data)
  WorkingData <- na.omit(WorkingData)
  block_temp <-block
  # block argument
  if(is.null(block)){
    WorkingData$blockTemp <- seq(nrow(WorkingData))
    block <- "blockTemp"
  } else{
    if(length(block) == 2){
      WorkingData$blockTemp <- paste(WorkingData[[block[1]]],
                                     WorkingData[[block[2]]], sep = "")
      block <- "blockTemp"
    }
  }

  # orderby argument
  if(is.null(orderby)){
    warning("missing 'orderby', data is ordered by default")
    WorkingData$orderbyTemp <- seq(nrow(WorkingData))
    orderby <- "orderbyTemp"
  }

  # select subset data
  if(!is.null(label)){
    WorkingDataSub <- subset(WorkingData, select = c(
      x, y, label,block_temp, block, orderby))
    WorkingDataSub <- WorkingDataSub[order(WorkingDataSub[[orderby]],
                                           decreasing = FALSE), ]
  } else{
    WorkingDataSub <- subset(WorkingData, select = c(
      x, y, block_temp,block, orderby, "keys"))            #KEYS IS ADDED, JAN 31
    WorkingDataSub <- WorkingDataSub[order(WorkingDataSub[[orderby]],
                                           decreasing = FALSE), ]
  }

  return(list(data = WorkingDataSub, block = block, orderby = orderby))
}

BayesNIGEstimate <- function(x, y, step, width) {

  # compute Bayesian NIG for any input without spliting
  #	R2FUN <- function(y, Est.y) return(1 - sum((y - Est.y) ^ 2) / sum((y - mean(y)) ^ 2))
  BETAFUN <- function(x, y) return(lm(y ~ x)$coefficients)
  #	BETAPHI2FUN <- function(x, Beta) return(Beta[1] + Beta[2] * x)
  SIGMAFUN <- function(x, y) return((summary(lm(y ~ x))$sigma) ^ 2)

  # window #
  WindowLength <- length(x) # number of windows
  WindowCenter <- seq(1, WindowLength, by = step)
  WindowStart <- ((WindowCenter - width) <= 0) * 1 +
    ((WindowCenter - width) > 0) * 1 * (WindowCenter - width)
  WindowEnd <- ((WindowLength - WindowCenter) >= width) * 1 * (WindowCenter + width) +
    ((WindowLength - WindowCenter) < width) * 1 * WindowLength
  x.list <- list()
  y.list <- list()
  for(i in seq(WindowLength)) {
    xInlist <- x[WindowStart[i] : WindowEnd[i]]
    yInlist <- y[WindowStart[i] : WindowEnd[i]]
    if(length(unique(xInlist)) == 1){
      xInlist <- xInlist + rnorm(length(xInlist), 0, 0.001)
    }
    if(length(unique(yInlist)) == 1){
      yInlist <- yInlist + rnorm(length(yInlist), 0, 0.001)
    }
    x.list[[i]] <- xInlist
    y.list[[i]] <- yInlist
  }

  # 2-dimensional Bayesian Processing #
  Linear.beta <- matrix(0, 2, WindowLength)
  for(i in seq(WindowLength)) {
    Linear.beta[ , i] <- BETAFUN(x.list[[i]], y.list[[i]])
  }
  Linear.sigma2 <- as.vector(mapply(SIGMAFUN, x = x.list, y = y.list))

  # Estimate mean and covariance of beta
  Linear.beta.mean <- apply(Linear.beta, 1, mean)
  A <- matrix(0, 2, 2)
  for(i in seq(WindowLength)) {
    A <- A + tcrossprod(Linear.beta[ , i] - Linear.beta.mean) / Linear.sigma2[i]
  }
  V.beta <- A / WindowLength

  # Estimate shape and rate of sigma2 (not necessary)
  GammaMLE <- function(x){
    fun <- function(x, a) {
      mean(log(x)) - log(mean(x)) + log(a) - digamma(a)
    }
    aHat <- uniroot(fun, c(0.000001, 10000), x = x)$root
    bHat <- mean(x) / aHat
    return(c(aHat, bHat))
  }
  GammaMMT <- function(x){
    m1 <- mean(x)
    m2 <- mean(x^2)
    aHat <- m1^2 / (m2 - m1^2)
    bHat <- (m2 - m1^2) / m1
    return(c(aHat, bHat))
  }
  a <- GammaMMT(1 / Linear.sigma2)[1]
  b <- GammaMMT(1 / Linear.sigma2)[2]
  #	invgamma.mu <- mean(Linear.sigma2)
  #	invgamma.sigma <- sqrt(var(Linear.sigma2))
  #	a <- 2 + invgamma.mu / invgamma.sigma
  #	b <- (1 + invgamma.mu / invgamma.sigma) * invgamma.mu

  result <- list(mu = Linear.beta.mean, Sigma = V.beta,
                 sigma2shape = a, sigma2rate = b)
  return(result)
}

BayesNIGPosterior <- function(x, y, step, width, Linear.beta.mean, V.beta){

  WindowLength <- length(x) # number of windows
  WindowCenter <- seq(1, WindowLength, by = step)
  WindowStart <- ((WindowCenter - width) <= 0) * 1 +
    ((WindowCenter - width) > 0) * 1 * (WindowCenter - width)
  WindowEnd <- ((WindowLength - WindowCenter) >= width) * 1 * (WindowCenter + width) +
    ((WindowLength - WindowCenter) < width) * 1 * WindowLength
  x.list <- list()
  y.list <- list()
  for(i in seq(WindowLength)) {
    x.list[[i]] <- x[WindowStart[i] : WindowEnd[i]]
    y.list[[i]] <- y[WindowStart[i] : WindowEnd[i]]
  }

  # update estimate of beta by max posterior
  BayesNIG.beta <- matrix(0, 2, WindowLength)
  for(i in seq(WindowLength)) {
    x.t <- x.list[[i]]
    X.t <- cbind(rep(1, length(x.t)), x.t)
    y.t <- y.list[[i]]
    mu.star <- ginv(ginv(V.beta) + crossprod(X.t)) %*%
      (ginv(V.beta) %*% Linear.beta.mean + crossprod(X.t, y.t))
    BayesNIG.beta[ , i] <- mu.star
  }
  result <- BayesNIG.beta
}

BayesianNIGProcess <- function(object){

  WorkingData <- object$WorkingDataTemp
  step <- object$step
  width <- object$width
  label <- object$label
  x <- object$x
  y <- object$y
  labelUniqueNames <- object$labelUniqueNames
  Beta <- NULL
  EstParametersList <- list()

  if(!(is.null(label))){
    for(i in seq(length(x))){         # For every environmental feature in x
      for(j in seq(length(y))){       # For every phynotype variable in y
        featuresTEMP <- c(x[i], y[j]) # x[i] and y[j] construct a pair of environmental-phynotype features
        featuresTEMP_history<-
        if((featuresTEMP[1]==featuresTEMP[2]) || (paste(x[i],y[j],"I",sep = "_") %in% colnames(Beta)) || (paste(y[j],x[i],"I",sep = "_") %in% colnames(Beta))){
          next
        }
        splitDatabyLabels <- split(WorkingData,  # Splits the data based on the label
                                   as.vector(WorkingData[[label]]))
        splitDataLabelNames <- names(splitDatabyLabels)
        interceptSplitData <- c()
        slopeSplitData <- c()
        labelSplitData <- c()
        blockSplitData <- c()
        for(k in seq(length(labelUniqueNames))){ # For all possible lables (like for D and H)
          DataTemp <- splitDatabyLabels[[k]]      # Select all data from a current label; like all data with D as their labels
          xTemp <- DataTemp[[featuresTEMP[1]]]   # xTemp is one environmental feature of the data like feature "LIGHT"
          yTemp <- DataTemp[[featuresTEMP[2]]]   # yTemp is one phynotype feature of the data like feature "phi2"
          labelTemp <- unique(DataTemp[[label]])
          resultTemp <- BayesNIGEstimate(xTemp, yTemp, step, width)
          mu <- resultTemp$mu
          Sigma <- resultTemp$Sigma
          sigma2shape <- resultTemp$sigma2shape
          sigma2rate <-  resultTemp$sigma2rate
          betaTemp <- BayesNIGPosterior(xTemp, yTemp, step, width,
                                        Linear.beta.mean = mu, V.beta = Sigma)
          EstParaName <- paste(x[i], y[j], sep = "_")
          EstParaListTemp <- list(mu = mu, Sigma = Sigma,
                                  sigma2shape = sigma2shape, sigma2rate = sigma2rate, Label = labelTemp)
          EstParametersList[[EstParaName]] <- EstParaListTemp

          interceptTemp <- as.vector(betaTemp[1, ])
          interceptSplitData <- c(interceptSplitData, interceptTemp)
          slopeTemp <- as.vector(betaTemp[2, ])
          slopeSplitData <- c(slopeSplitData, slopeTemp)
          labelSplitData <- c(labelSplitData, rep(splitDataLabelNames[k],
                                                  length(interceptTemp)))
          blockSplitData <-c(blockSplitData, splitDatabyLabels[[k]]$blockTemp)
        }
        BetaTemp <- cbind(interceptSplitData, slopeSplitData)
        colnames(BetaTemp) <- c(paste(x[i], y[j], "I", sep = "_"),
                                paste(x[i], y[j], "S", sep = "_"))
        Beta <- cbind(Beta, BetaTemp)
      }
    }
    Beta <- as.data.frame(Beta)
    if(is.null(blockSplitData))
      Beta <- data.frame(Beta, Label = labelSplitData)
    else
      Beta <- data.frame(Beta, Label = labelSplitData, blockTemp=blockSplitData)
  } else{  # Else if the data is a testing data and so there is no lable
    for(i in seq(length(x))){
      for(j in seq(length(y))){
        featuresTEMP <- c(x[i], y[j])
        if((featuresTEMP[1]==featuresTEMP[2]) || (paste(x[i],y[j],"I",sep = "_") %in% colnames(Beta)) || (paste(y[j],x[i],"I",sep = "_") %in% colnames(Beta))){
          next
        }
        DataTemp <- WorkingData
        xTemp <- DataTemp[[featuresTEMP[1]]]
        yTemp <- DataTemp[[featuresTEMP[2]]]
        resultTemp <- BayesNIGEstimate(xTemp, yTemp, step, width)
        mu <- resultTemp$mu
        Sigma <- resultTemp$Sigma
        sigma2shape <- resultTemp$sigma2shape
        sigma2rate <-  resultTemp$sigma2rate
        betaTemp <- BayesNIGPosterior(xTemp, yTemp, step, width,
                                      Linear.beta.mean = mu, V.beta = Sigma)
        EstParaName <- paste(x[i], y[j], sep = "_")
        EstParaListTemp <- list(mu = mu, Sigma = Sigma,
                                sigma2shape = sigma2shape, sigma2rate = sigma2rate, Label = NA)
        EstParametersList[[EstParaName]] <- EstParaListTemp

        interceptTemp <- as.vector(betaTemp[1, ])
        slopeTemp <- as.vector(betaTemp[2, ])
        BetaTemp <- cbind(interceptTemp, slopeTemp)
        colnames(BetaTemp) <- c(paste(x[i], y[j], "I", sep = "_"),
                                paste(x[i], y[j], "S", sep = "_"))
        Beta <- cbind(Beta, BetaTemp)
      }
    }
    Beta <- as.data.frame(Beta)
  }
  return(list(Beta = Beta, EstParametersList = EstParametersList))
}

# This function is the training function. It gets the input training data, transform the data and
# and train a svm or a RF.
Pheno <- function(data = NULL, x = NULL, y = NULL, label = NULL,
                  defaultLabel = NULL, ncluster = NULL, block = NULL, orderby = NULL,
                  method = "SVM",	step = 1, width = 6, nfeatures = 3, feature_selection="lmFuncs"){

  fn <- "Pheno"
  arugmentsData <- list(data = data, x = x, y = y, label = label,
                        defaultLabel = defaultLabel, block = block, ncluster = ncluster,
                        orderby = orderby, method = method, step = step, width = width,
                        nfeatures = nfeatures)
  attr(arugmentsData, 'class') <- fn
  CheckArguments(arugmentsData)

  x <- unique(x)
  y <- unique(y)
  ret.x <- x
  ret.y <- y
  ret.label <- label
  ret.defaultLabel <- defaultLabel
  ret.block <- block
  ret.orderby <- orderby

  if(length(ret.block) == 2){
    blockNamesONE <- unique(data[[block[1]]])
    blockNamesTWO <- unique(data[[block[2]]])
  } else{
    blockNamesONE <- NULL
    blockNamesTWO <- NULL
  }

  valueTransferData <- TransferData(data, x, y, label, block, orderby) # TransferData is a function to remove irrelevant features and IT CHANGES THE ORDER OF THE INPUT DATA
  WorkingData <- valueTransferData$data
  RawData <- WorkingData
  block <- valueTransferData$block
  orderby <- valueTransferData$orderby

  if(!is.null(label)){
    labelUniqueNames <- unique(WorkingData[[label]]) # LabelUniqueNames includes label values like M, N and S

    features <- c(x, y) # features includes both x and y (i.e. all  environmental and phenotype features
    WorkingDataTemp <- subset(WorkingData, select = c(features,label)) #WorkingDataTemp includes only environmental and phenotype features and the label
    OriginalData <- WorkingDataTemp
    #		crossvadalitionData <- WorkingData

    objectlist <- list(WorkingDataTemp = WorkingDataTemp, step = step,
                       width = width, label = label, x = x, y = y,
                       labelUniqueNames = labelUniqueNames)
    #		attr(objectlist, 'class') <- fn
    WorkingDataTemp <- BayesianNIGProcess(objectlist)
    EstParametersList <- WorkingDataTemp$EstParametersList
    BayesianData <- WorkingDataTemp$Beta
    WorkingDataTemp <- BayesianData
    lda_data_transformed<- NULL
    lda_index<-0
    lda_res<- NULL
    scales_matrix <- as.data.frame(matrix(0,nrow = (dim(WorkingDataTemp)[2]-1)/2,ncol = 3))
    colnames(scales_matrix) <- c("Intercept coefficient", "slope coefficient")
    if(feature_selection=="LDA"){
      for(i in seq(length(x))){
        for(j in seq(length(y))){
          xName <- paste(x[i], y[j], "I", sep = "_")
          yName <- paste(x[i], y[j], "S", sep = "_")
          if((x[i]==y[j]) || !(xName %in% colnames(WorkingDataTemp)) || !(yName %in% colnames(WorkingDataTemp))){
            next
          }
          lda_data_temp <- subset(WorkingDataTemp, select = c(xName, yName, "Label"))
          lda_index<-lda_index+1
          lda_res <- lda(Label ~ .,
                     lda_data_temp)
          #plda <- predict(object = lda_res,
          #                newdata = lda_data_temp)
          #colnames(plda$x) <-paste(x[i],y[j],sep="_")
          #lda_data_transformed <- cbind(lda_data_transformed, plda$x)
          #lda_data_transformed <- cbind(lda_data_transformed, plda$x)
          scales_matrix[lda_index,1:2] <-lda_res$scaling
          scales_matrix[lda_index,3] <- paste(x[i],y[j],sep="_")
          transformed_vector <- (lda_data_temp[,1] * scales_matrix[1,1])+(lda_data_temp[,2]*scales_matrix[1,2])
          transformed_vector <- as.matrix(transformed_vector)
          colnames(transformed_vector) <-paste(x[i],y[j],sep="_")
          lda_data_transformed <- cbind(lda_data_transformed, transformed_vector)

          #=================plot
          plot1 <- ggplot() + geom_point(data = lda_data_temp,
                                      mapping = aes(lda_data_temp[[1]], lda_data_temp[[2]],
                                                    colour = factor(lda_data_temp[[3]]))) + xlab(xName) + ylab(yName) + theme(legend.position = "none")
          ggsave(paste("PhenoPro_",xName,".png",sep = ""))
          plot_temp1<-as.data.frame(transformed_vector)
          plot_temp1<-cbind(plot_temp1, lda_data_temp$Label)
          colnames(plot_temp1)<-c(paste(x[i],y[j],sep="_"),"Labels")
          plot2 <- ggplot() + geom_point(data = plot_temp1,
                                         mapping = aes(plot_temp1[[1]], Labels,
                                                       colour = factor(plot_temp1$Labels))) + xlab(paste(x[i],y[j],sep="_")) + ylab("Labels") + theme(legend.position = "none")

          ggsave(paste("LDA_",paste(x[i],y[j],sep="_"),".png",sep = ""))

          #=================plot

          #assign(paste("lda_res",paste(x[i],y[j],sep="_"),sep="_"), lda_res, envir = .GlobalEnv)
          #plot1 <- ggplot() + geom_point(data = lda_data_temp,
          #                            mapping = aes(lda_data_temp[[1]], lda_data_temp[[2]],
          #                                          colour = factor(lda_data_temp[[3]]))) + xlab(xName) + ylab(yName) + theme(legend.position = "none")
          #ggsave(paste("PhenoPro_",xName,".png",sep = ""))
          #plot_temp1<-as.data.frame(plda$x)
          #plot_temp1<-cbind(plot_temp1, lda_data_temp$Label)
          #colnames(plot_temp1)<-c(paste(x[i],y[j],sep="_"),"Labels")
          #plot2 <- ggplot() + geom_point(data = plot_temp1,
          #                               mapping = aes(plot_temp1[[1]], Labels,
          #                                             colour = factor(plot_temp1$Labels))) + xlab(paste(x[i],y[j],sep="_")) + ylab("Labels") + theme(legend.position = "none")

          #ggsave(paste("LDA_",paste(x[i],y[j],sep="_"),".png",sep = ""))
          #plot3 <- grid.arrange(plot1, plot2, ncol = 2)
          #plot3

        }
      }

      topFeatureNames <- colnames(lda_data_transformed)
      lda_data_transformed<-as.data.frame(lda_data_transformed)
      lda_data_transformed <- cbind(lda_data_transformed, Label=WorkingDataTemp$Label)

      weights <- information.gain(Label~., lda_data_transformed)
      weights[,2]<- row.names(weights)
      weights_sorted <-sort(weights[,1], decreasing = TRUE, index.return=TRUE)
      topFeatureNames_indexes<- weights_sorted$ix[1:nfeatures]
      topFeatureNames<- weights[topFeatureNames_indexes,2]


      WorkingDataTempPCA<-subset(lda_data_transformed, select = c(topFeatureNames, "Label"))

      fullFeatureNames <- as.vector(colnames(BayesianData))
      fullFeatureNames <- fullFeatureNames[1:length(fullFeatureNames)-1]
      topNameSplit <- strsplit(topFeatureNames, "_")
      topFeatureFullNames <- c()
      for(i in seq(length(topFeatureNames))){
        topFeatureFullNames <- c(topNameSplit[[i]][1], topNameSplit[[i]][2],
                                 topFeatureFullNames)
      }
      topFeatureFullNames <- unique(as.vector(topFeatureFullNames))

      fullNameSplit <- strsplit(fullFeatureNames, "_")
      fullFeatureFullNames <- c()
      for(i in seq(length(fullFeatureNames))){
        fullFeatureFullNames <- c(fullNameSplit[[i]][1], fullNameSplit[[i]][2],
                                  fullFeatureFullNames)
      }
      fullFeatureFullNames <- unique(as.vector(fullFeatureFullNames))

    }else if(feature_selection=="PCA"){
      resPCA <- prcomp(subset(WorkingDataTemp, select = - Label),
                       center = TRUE, scale. = TRUE)
      #===================new (april 22, 2018) implementation of PCA
      WorkingDataTempPCA <- resPCA$x[,1:nfeatures]
      topFeatureNames<-NULL
      selected_features<-NULL
      numberPCA <- nfeatures
      fea <- NULL
      topFeatureFullNames <- NULL
      fullFeatureNames <- NULL
      fullFeatureFullNames <- NULL
      WorkingDataTempPCA <- as.data.frame(WorkingDataTempPCA)
      WorkingDataTempPCA[["Label"]]<- WorkingDataTemp$Label
      #WorkingDataTempPCA<-as.data.frame(cbind(WorkingDataTempPCA, Label=WorkingDataTemp$Label))
      #===================

      #==================Following lines are highlighted because I wanted to try the above code for PCA

      # propvar <- as.vector(summary(resPCA)$importance[2, ])
      # cumuvar <- as.vector(summary(resPCA)$importance[3, ])
      # numberPCA <- max(2, which(cumuvar > 0.7)[1])
      # resRotation <- resPCA$rotation[ , (1 : (numberPCA))]
      # nameTemp <- rownames(resRotation)
      # scaleRotation <- t(t(abs(resRotation)) * propvar)
#
#       # searching top feature names combining (I, S)
#       #		nameSplit <- strsplit(nameTemp, "_")
#       #		rowSumsTemp <- as.vector(rowSums(scaleRotation))
#       #		feaSums <- rep(0, length(rowSumsTemp) / 2)
#       #		feaNams <- rep(0, length(rowSumsTemp) / 2)
#       #		for(i in seq(length(rowSumsTemp) / 2)){
#       #			feaSums[i] <- rowSumsTemp[2 * i - 1] + rowSumsTemp[2 * i]
#       #			feaNams[i] <- paste(nameSplit[[2 * i]][1], nameSplit[[2 * i]][2],
#       #				sep = "_")
#       #		}
#       #		featureTemp <- data.frame(feaNams, feaSums)
#       #		featureTemp <- featureTemp[order(featureTemp$feaSums, decreasing = TRUE), ]
#       #		topFeatureNames <- as.vector(featureTemp$feaNams[1 : min(nrow(featureTemp), 3)])
#
#       # searching top feature names not combining (I, S)
#
#       rowSumsTemp <- as.vector(rowSums(scaleRotation))
#       feaSums <- rowSumsTemp
#       feaNams <- as.vector(nameTemp)
#       featureTemp <- data.frame(feaNams, feaSums)
#       featureTemp <- featureTemp[order(featureTemp$feaSums, decreasing = TRUE), ]
#       if(is.null(nfeatures)){
#         topFeatureNames <- as.vector(featureTemp$feaNams)
#       } else{
#         if(nfeatures < nrow(featureTemp)){
#           topFeatureNames <- as.vector(featureTemp$feaNams[1 : min(nrow(featureTemp), nfeatures)])
#         } else{
#           stop("please select a smaller 'nfeatures'")
#         }
#       }
#
#
#       fullFeatureNames <- as.vector(featureTemp$feaNams)
#
#       topNameSplit <- strsplit(topFeatureNames, "_")
#       topFeatureFullNames <- c()
#       for(i in seq(length(topFeatureNames))){
#         topFeatureFullNames <- c(topNameSplit[[i]][1], topNameSplit[[i]][2],
#                                  topFeatureFullNames)
#       }
#       topFeatureFullNames <- unique(as.vector(topFeatureFullNames))
#
#       fullNameSplit <- strsplit(fullFeatureNames, "_")
#       fullFeatureFullNames <- c()
#       for(i in seq(length(fullFeatureNames))){
#         fullFeatureFullNames <- c(fullNameSplit[[i]][1], fullNameSplit[[i]][2],
#                                   fullFeatureFullNames)
#       }
#       fullFeatureFullNames <- unique(as.vector(fullFeatureFullNames))
#
#       WorkingDataTempPCA <- subset(BayesianData, select = c(topFeatureNames,
#                                                             "Label"))

    }else if(feature_selection =="NA")
    {

      WorkingDataTempPCA<-BayesianData #subset(BayesianData, select = c(lmProfile$optVariables,
      #                   "Label"))
      topFeatureNames<-as.vector(colnames(BayesianData))#lmProfile$optVariables
      topFeatureNames<-topFeatureNames[1:length(topFeatureNames)-1]
      #fullFeatureNames <- as.vector(featureTemp$feaNams)
      fullFeatureNames <- as.vector(colnames(BayesianData))
      fullFeatureNames <- fullFeatureNames[1:length(fullFeatureNames)-1]
      topNameSplit <- strsplit(topFeatureNames, "_")
      topFeatureFullNames <- c()
      for(i in seq(length(topFeatureNames))){
        topFeatureFullNames <- c(topNameSplit[[i]][1], topNameSplit[[i]][2],
                                 topFeatureFullNames)
      }
      topFeatureFullNames <- unique(as.vector(topFeatureFullNames))

      fullNameSplit <- strsplit(fullFeatureNames, "_")
      fullFeatureFullNames <- c()
      for(i in seq(length(fullFeatureNames))){
        fullFeatureFullNames <- c(fullNameSplit[[i]][1], fullNameSplit[[i]][2],
                                  fullFeatureFullNames)
      }
      fullFeatureFullNames <- unique(as.vector(fullFeatureFullNames))
    #Else if the user selected subset based feature selection methods
    }else if(feature_selection=="lmFuncs" || feature_selection=="nbFuncs" || feature_selection=="rfFuncs" || feature_selection=="treebagFuncs")
    {
      library(caret)
      library(mlbench)
      library(Hmisc)
      library(randomForest)
      if(feature_selection=="lmFuncs"){
        fs_method=lmFuncs
      } else if(feature_selection=="nbFuncs"){
        fs_method=nbFuncs
      } else if(feature_selection=="rfFuncs"){
        fs_method=rfFuncs
      } else if(feature_selection=="treebagFuncs"){
        fs_method=treebagFuncs
      }

      dimensionality=dim(WorkingDataTemp)
      WorkingDataTemp_label=WorkingDataTemp[,dimensionality[2]]
      WorkingDataTemp_train=WorkingDataTemp[,1:(dimensionality[2]-1)]
      subsets_pheno <- c(1:(dimensionality[2]-1))
      set.seed(10)
      ctrl <- rfeControl(functions = fs_method,
                         method = "repeatedcv",
                         repeats = 5,
                         verbose = FALSE)
      if(feature_selection=="nbFuncs"){
        WorkingDataTemp_label_numeric <- WorkingDataTemp_label}
      else {
        WorkingDataTemp_label_numeric <- as.numeric(WorkingDataTemp_label)}
      #WorkingDataTemp_label_numeric <- WorkingDataTemp_label
      lmProfile <- rfe(WorkingDataTemp_train, WorkingDataTemp_label_numeric,
                       sizes = subsets_pheno,
                       rfeControl = ctrl)
      WorkingDataTempPCA<-subset(BayesianData, select = c(lmProfile$optVariables,
                                                          "Label"))
      topFeatureNames<-lmProfile$optVariables
      #fullFeatureNames <- as.vector(featureTemp$feaNams)
      fullFeatureNames <- as.vector(colnames(BayesianData))
      fullFeatureNames <- fullFeatureNames[1:length(fullFeatureNames)-1]
      topNameSplit <- strsplit(topFeatureNames, "_")
      topFeatureFullNames <- c()
      for(i in seq(length(topFeatureNames))){
        topFeatureFullNames <- c(topNameSplit[[i]][1], topNameSplit[[i]][2],
                                 topFeatureFullNames)
      }
      topFeatureFullNames <- unique(as.vector(topFeatureFullNames))

      fullNameSplit <- strsplit(fullFeatureNames, "_")
      fullFeatureFullNames <- c()
      for(i in seq(length(fullFeatureNames))){
        fullFeatureFullNames <- c(fullNameSplit[[i]][1], fullNameSplit[[i]][2],
                                  fullFeatureFullNames)
      }
      fullFeatureFullNames <- unique(as.vector(fullFeatureFullNames))
      # ELSE if the user selected Information Gain based feature selection
    }else if(feature_selection=="IG")
    {
      library(FSelector)

      dimensionality=dim(WorkingDataTemp)
      WorkingDataTemp_label=WorkingDataTemp[,dimensionality[2]]
      WorkingDataTemp_train=WorkingDataTemp[,1:(dimensionality[2]-1)]
      subsets_pheno <- c(1:(dimensionality[2]-1))

      weights <- information.gain(Label~., WorkingDataTemp) # This weights contains feature names and their IG score
      weights[,2]<- row.names(weights)
      weights_sorted <-sort(weights[,1], decreasing = TRUE, index.return=TRUE)
      topFeatureNames_indexes<- weights_sorted$ix[1:nfeatures]
      topFeatureNames<- weights[topFeatureNames_indexes,2]
      #========================================================
      #topFeatureNames<- c("LIGHT_PhiNPQ_I", "LIGHT_PhiNPQ_S")# For R1
      #topFeatureNames<- c("LIGHT_PhiNO_I", "LIGHT_PhiNO_S") # For V3; this one is good when PhenoPro8 is used in which during the training phase we transform block by block. However, in PhenoPro7 we first divide the training to H and D and then transform them seperately.
      #topFeatureNames<- c("LIGHT_PhiNO_I", "LIGHT_PhiNO_S","LIGHT_PhiNPQ_I", "LIGHT_PhiNPQ_S") # For the whole data including both R1 and V3
      topFeatureNames<- c("Lef_SPAD_I","Lef_SPAD_S") # For V3; April 16, 2018==== I can also use RH-Phi2 and LIGHT-SPAD and LIGHT-Fs
      #========================================================
      WorkingDataTempPCA<-subset(BayesianData, select = c(topFeatureNames,
                                                          "Label"))

      fullFeatureNames <- as.vector(colnames(BayesianData))
      fullFeatureNames <- fullFeatureNames[1:length(fullFeatureNames)-1]
      topNameSplit <- strsplit(topFeatureNames, "_")
      topFeatureFullNames <- c()
      for(i in seq(length(topFeatureNames))){
        topFeatureFullNames <- c(topNameSplit[[i]][1], topNameSplit[[i]][2],
                                 topFeatureFullNames)
      }
      topFeatureFullNames <- unique(as.vector(topFeatureFullNames))

      fullNameSplit <- strsplit(fullFeatureNames, "_")
      fullFeatureFullNames <- c()
      for(i in seq(length(fullFeatureNames))){
        fullFeatureFullNames <- c(fullNameSplit[[i]][1], fullNameSplit[[i]][2],
                                  fullFeatureFullNames)
      }
      fullFeatureFullNames <- unique(as.vector(fullFeatureFullNames))
    }

    if(method == "RF"){
      predictData <- randomForest(Label ~ ., data = WorkingDataTempPCA,
                                  importance = TRUE, proximity = TRUE)
    } else{
      predictData <- svm(Label ~ ., data = WorkingDataTempPCA)
    }
    clusterData <- NULL
  } else{
    stop("invalid 'label' which is NULL")
  }
  # if(feature_selection=="LDA"){
  #   lda_res_list_index <- 0
  #   lda_res_list <- vector("list", lda_index)
  #   for(i in seq(length(x))){
  #     for(j in seq(length(y))){
  #       if(x[i]==y[j]){
  #         next
  #       }
  #       xName <- paste(x[i], y[j], "I", sep = "_")
  #       yName <- paste(x[i], y[j], "S", sep = "_")
  #       lda_res_list_index<-lda_res_list_index+1
  #       lda_res_list[lda_res_list_index] <- eval(parse(text = paste("lda_res",paste(x[i],y[j],sep="_"),sep="_")))
  #       #assign(paste("lda_res",paste(x[i],y[j],sep="_"),sep="_"), lda_res_list[[lda_res_list_index]])
  #       }
  #     }
  #   }
  if(feature_selection=="LDA"){
    lda_model <- lda_res
    resPCA <- NULL
    numComp = NULL
    fea <- topFeatureNames
    selected_features <- topFeatureNames
  }else{
    lda_model <- NULL
    scales_matrix <- NULL
  }
  if(feature_selection=="PCA"){
    resPCA <- resPCA
    numComp <- numberPCA
    fea <- topFeatureNames
    selected_features=NULL
    weights<-NULL
  }else if(feature_selection=="NA" || feature_selection =="IG"){
    resPCA <- NULL
    numComp = NULL
    fea <- topFeatureNames
    selected_features <- topFeatureNames
    weights<-weights
  }	else if(feature_selection=="lmFuncs" || feature_selection=="nbFuncs" || feature_selection=="rfFuncs" || feature_selection=="treebagFuncs"){
    resPCA <- NULL
    numComp = NULL
    fea <- lmProfile$optVariables
    selected_features=lmProfile$optVariables
    weights <- NULL
  }
  used_blocks <- unique(WorkingData$blockTemp)
  returnData <- list(
    Raw = RawData,
    Org = OriginalData,
    nfeatures=nfeatures,
    transformed_data=WorkingDataTempPCA,
    Bay = BayesianData, # add topFeatureNames as their column names
    pre = predictData,
    EST = EstParametersList,
    feature_selection  = feature_selection,
    used_blocks = used_blocks,
    #		clu = clusterData,
    #		cv = crossvadalitionData,
    arg = list(label = label, x = x, y = y, method = method,
               block = block, defaultLabel = defaultLabel, orderby = orderby,
               step = step, width = width, blockNamesONE = blockNamesONE,
               blockNamesTWO = blockNamesTWO),
    #		pca = WorkingDataTempPCA, # matrix after rotation
    resPCA = resPCA,
    lda_model=lda_model,
    scales_matrix=scales_matrix,
    weights_features=weights,
    numComp = numComp,
    #fea = topFeatureNames,
    #fea = lmProfile$optVariables,
    fea=fea,
    feaf = topFeatureFullNames,
    fullfea = fullFeatureNames,
    selected_features=selected_features,
    fullfeaf = fullFeatureFullNames
  )
  attr(returnData,'class') <- fn
  return(returnData)
}

summary.Pheno <- function(object){

  topFeatureNames <- object$fea
  fullFeatureNames <- object$fullfea
  fullNames <- paste(fullFeatureNames[1], fullFeatureNames[2], sep = ",")
  for(i in 3 : length(fullFeatureNames)){
    fullNames <- paste(fullNames, fullFeatureNames[i], sep = ",")
  }

  resPCA <- object$resPCA
  numberPCA <- object$numComp
  infTemp <- paste("Recommended individual feature(s) : ",
                   topFeatureNames, sep = " ")
  for(i in seq(length(infTemp))){
    print(infTemp[i])
  }
  infTemp <- paste("Number of selected component(s) in PCA : ", numberPCA,
                   sep = " ")
  print(infTemp)
  infTemp <- paste("Ranked full individual feature(s) : ",
                   fullNames, sep = " ")
  print(infTemp)
  print("Information on PCA : ")
  return(summary(resPCA))
}

#You can pass the return object by the function "Pheno" as an input to the "plot.Pheno" to plot the training data and the transformed training data
plot.Pheno <- function(object){
  arg <- object$arg
  x <- arg$x
  y <- arg$y
  label <- arg$label
  OriginalData <- object$Org
  BayesianData <- object$Bay

  for(i in seq(length(x))){
    for(j in seq(length(y))){
      if(!(paste(x[i],y[j],"I",sep = "_") %in% colnames(BayesianData))){
        next
      }
      OriginalDataPlot <- subset(OriginalData, select = c(x[i], y[j], label))
      xName <- paste(x[i], y[j], "I", sep = "_")
      yName <- paste(x[i], y[j], "S", sep = "_")
      BayesianDataPlot <- subset(BayesianData, select = c(xName, yName, "Label"))

      p1 <- ggplot() + geom_point(data = OriginalDataPlot,
                                  mapping = aes(OriginalDataPlot[[x[i]]], OriginalDataPlot[[y[j]]],
                                                colour = factor(OriginalDataPlot[[label]]))) +
        xlab(x[i]) +
        ylab(y[j]) + theme(legend.position = "none")
      p2 <- ggplot() + geom_point(data = BayesianDataPlot,
                                  mapping = aes(BayesianDataPlot[[xName]], BayesianDataPlot[[yName]],
                                                colour = Label)) +
        xlab("intercept") +
        ylab("Slope")
      p12 <- grid.arrange(p1, p2, ncol = 2)
      p12
    }
  }
}

predict.Pheno <- function(object, data, x, y,
                          block, orderby, step = 1, width = 6){

  fn <- "predictPheno"
  arugmentsData <- list(data = data, x = x, y = y, block = block,
                        orderby = orderby, step = step, width = width)
  attr(arugmentsData, 'class') <- fn
  CheckArguments(arugmentsData)

  predictData <- object$pre
  topFeatureNames <- object$fea
  topFeatureFullNames <- object$feaf
  argumentsDataTemp <- object$arg
  labelTemp <- argumentsDataTemp$label
  label <- NULL
  labelUniqueNames <- NULL

  if(is.null(labelTemp))
    stop("invalid usage, prediction is not available for clustering")
  methodTemp <- argumentsDataTemp$method
  xSelect <- intersect(x, topFeatureFullNames)
  ySelect <- intersect(y, topFeatureFullNames)

  valueTransferData <- TransferData(data, xSelect, ySelect, label = label, block, orderby)
  WorkingData <- valueTransferData$data

  objectlist <-  list(WorkingDataTemp = WorkingData, step = step,
                      width = width, label = label, x = xSelect, y = ySelect,
                      labelUniqueNames = labelUniqueNames)
  WorkingDataTemp <- BayesianNIGProcess(objectlist)
  EstParametersList <- WorkingDataTemp$EstParametersList
  BayesianData <- WorkingDataTemp$Beta
  WorkingDataTemp <- BayesianData

  inputData <- subset(WorkingDataTemp, select = topFeatureNames)

  if(methodTemp == "RF"){
    par.pred <- predict(predictData, inputData)
  } else{
    par.pred <- predict(predictData, inputData)
  }
  outputData <- data.frame(inputData, Label = par.pred)
  returnData <- list(arg = list(x = x, y = y), Org = WorkingData, Bay = outputData)
  attr(returnData, 'class') <- fn
  return(returnData)
}

plot.predictPheno <- function(object){
  arg <- object$arg
  x <- arg$x
  y <- arg$y
  OriginalData <- object$Org
  BayesianData <- object$Bay

  for(i in seq(length(x))){
    for(j in seq(length(y))){
      OriginalDataPlot <- subset(OriginalData, select = c(x[i], y[j]))
      xName <- paste(x[i], y[j], "I", sep = "_")
      yName <- paste(x[i], y[j], "S", sep = "_")

      if((xName %in% colnames(BayesianData)) & (yName %in% colnames(BayesianData))){
        BayesianDataPlot <- subset(BayesianData, select = c(xName, yName, "Label"))
        p1 <- ggplot() + geom_point(data = OriginalDataPlot,
                                    mapping = aes(OriginalDataPlot[[x[i]]], OriginalDataPlot[[y[j]]])) +
          xlab(x[i]) +
          ylab(y[j]) + theme(legend.position = "none")
        p2 <- ggplot() + geom_point(data = BayesianDataPlot,
                                    mapping = aes(BayesianDataPlot[[xName]], BayesianDataPlot[[yName]],
                                                  colour = Label)) +
          xlab("intercept") +
          ylab("Slope")
        p12 <- grid.arrange(p1, p2, ncol = 2)
        p12
      }

    }
  }
}

cv <- function(x, ...) UseMethod("cv")

cv.Pheno <- function(object, cvNumber, testBlockProp, prior){

  WorkingData <- object$Raw
  argumentsDataTemp <- object$arg
  block <- argumentsDataTemp$block
  label <- argumentsDataTemp$label
  orderby <- argumentsDataTemp$orderby
  step <- argumentsDataTemp$step
  width <- argumentsDataTemp$width
  method <- argumentsDataTemp$method
  defaultLabel <- argumentsDataTemp$defaultLabel
  blockNamesONE <- argumentsDataTemp$blockNamesONE
  blockNamesTWO <- argumentsDataTemp$blockNamesTWO

  topFeatureFullNames <- object$feaf
  topFeatureNames <- object$fea
  x <- intersect(argumentsDataTemp$x, topFeatureFullNames)
  y <- intersect(argumentsDataTemp$y, topFeatureFullNames)

  blockUniqueNames <- unique(WorkingData[[block]])
  blockOrderedNames <- mixedsort(blockUniqueNames)
  labelUniqueNames <- unique(WorkingData[[label]])
  defaultLabelUniqueNames <- defaultLabel
  inverseLabel <- labelUniqueNames[- which(labelUniqueNames == defaultLabel)]

  splitDatabyBlock <- split(WorkingData, as.vector(WorkingData[[block]]))
  FUNALLEQU <- function(x, label){
    return(!any(x[[label]] != x[[label]][1]))
  }
  FUNLABEL <- function(x, label){
    return(x[[label]][1])
  }
  if(!all(sapply(splitDatabyBlock, FUNALLEQU, label = label, simplify = TRUE)))
    stop("data in some 'block' have multiple 'label'")
  blockOrderedLabels <- as.vector(sapply(splitDatabyBlock, FUNLABEL,
                                         label = label, simplify = TRUE))

  WorkingDataSub <- subset(WorkingData,
                           select = c(x, y, label, block, orderby))
  features <- c(x, y)
  inicvNumber <- 1
  outputTable <- data.frame(matrix(0, length(blockUniqueNames), 3))
  rownames(outputTable) <- blockOrderedNames
  colnames(outputTable) <- c("performance", "precision", "recall")
  outputTableTemp <- outputTable
  countTable <- rep(0, length(blockUniqueNames))
  while(inicvNumber <= cvNumber){  # The data will be devided into train and test set, a model will be created based on the train part and will be tested based on the test part
    # if(inicvNumber==4){
    #   print("sssss");
    # }
    cat("computing ", inicvNumber, "\r")
    valueSplitData <- SplitData(WorkingDataSub, block, orderby,
                                testBlockProp, blockOrderedLabels, blockOrderedNames)  # Here the data is divided to train and test data
    dataSplitData <- valueSplitData$data
    trainData <- dataSplitData$train
    testData <- dataSplitData$test
    testName <- dataSplitData$testName

    trainDataTemp <- subset(trainData, select = c(features, label))
    if(length(unique(trainDataTemp[,ncol(trainDataTemp)]))<length(labelUniqueNames)){
      print("Warning: One step of cross validation is skiped; there aren't data from all classes")
      inicvNumber<-inicvNumber+1
      next
    }
    objectlist <- list(WorkingDataTemp = trainDataTemp, step = step,
                       width = width, label = label, x = x, y = y,
                       labelUniqueNames = labelUniqueNames)

    trainBayesianData <- BayesianNIGProcess(objectlist)$Beta
    trainBayesianDataTemp <- subset(trainBayesianData,
                                    select = c(topFeatureNames, "Label"))

    testDataTemp <- subset(testData, select = c(features, label))
    if(length(unique(trainDataTemp[,ncol(trainDataTemp)]))<length(labelUniqueNames)){
      print("Warning: One step of cross validation is skiped; there aren't data from all classes")
      inicvNumber<-inicvNumber+1
      next
    }
    if(prior == TRUE){
      objectlist <- list(WorkingDataTemp = testDataTemp, step = step,
                         width = width, label = label, x = x, y = y,
                         labelUniqueNames = labelUniqueNames)
      testBayesianData <- BayesianNIGProcess(objectlist)$Beta
      testBayesianDataTemp <- subset(testBayesianData,
                                     select = c(topFeatureNames, "Label"))
      inputData <- subset(testBayesianDataTemp, select = topFeatureNames)
    } else{
      objectlist <- list(WorkingDataTemp = testDataTemp, step = step,
                         width = width, label = NULL, x = x, y = y,
                         labelUniqueNames = NULL)
      testBayesianData <- BayesianNIGProcess(objectlist)$Beta
      testBayesianDataTemp <- data.frame(subset(testBayesianData,
                                                select = c(topFeatureNames)), Label = testDataTemp[[label]])
      inputData <- subset(testBayesianDataTemp, select = topFeatureNames)
    }

    #Added by Sajjad
    #trainBayesianDataTemp<-subset(trainBayesianData,
    #                             select = c(object$selected_features, "Label"))
    #inputData <- subset(inputData, select = object$selected_features)
    # End of added by Sajjad

    if(method == "RF"){
      predictData <- randomForest(Label ~ ., data = trainBayesianDataTemp,
                                  importance = TRUE, proximity = TRUE)
      par.pred <- predict(predictData, inputData)
    } else{
      predictData <- svm(Label ~ ., data = trainBayesianDataTemp)
      par.pred <- predict(predictData, inputData)
    }

    DI <- DD <- II <- ID <- 0
    for(i in seq(length(par.pred))){
      if(par.pred[i] == testBayesianDataTemp$Label[i]){
        if(par.pred[i] %in% inverseLabel){
          II <- II + 1
        } else{
          DD <- DD + 1
        }
      } else{
        if(par.pred[i] == defaultLabel){
          DI <- DI + 1
        } else{
          if(testBayesianDataTemp$Label[i] == defaultLabel){
            ID <- ID + 1
          }
        }
      }
    }
    performance <- as.numeric((DD + II) / length(par.pred), 4)
    recall <- as.numeric(DD / length(which(testBayesianDataTemp$Label == defaultLabel)), 4)
    precision <- as.numeric(DD / (DD + DI), 4)

    inicvNumber <- inicvNumber + 1
    rowID <- which(rownames(outputTable) %in% testName)
    countTable[rowID] <- countTable[rowID] + 1
    outputTableTemp[rowID, 1] <- outputTableTemp[rowID, 1] +
      performance
    outputTableTemp[rowID, 2] <- outputTableTemp[rowID, 2] +
      precision
    outputTableTemp[rowID, 3] <- outputTableTemp[rowID, 3] +
      recall
  }

  outputTable <- outputTableTemp / countTable
  returnData <- list(
    outputTable = outputTable,
    blockNamesONE = blockNamesONE,
    blockNamesTWO = blockNamesTWO,
    topFeatureNames = topFeatureNames
  )
  attr(returnData,'class') <- "cvPheno"
  return(returnData)
}

plot.cvPheno <- function(object){

  blocknamesONE <- object$blockNamesONE
  blocknamesTWO <- object$blockNamesTWO
  if(is.null(blocknamesONE))
    stop("invalid 'block', which must have two components")
  topFeatureNames <- object$topFeatureNames
  output <- object$outputTable
  Lrow <- length(blocknamesONE)
  Lrun <- length(blocknamesTWO)

  rowN <- c()
  for(i in seq(Lrow)){
    rowN <- c(rowN, rep(blocknamesONE[i], Lrun))
  }
  colN <- rep(blocknamesTWO, Lrow)
  Values <- c(output[ , 1], output[ , 2], output[ , 3])
  Series <- c(rep("performance", Lrow * Lrun), rep("precision", Lrow * Lrun),
              rep("recall", Lrow * Lrun))
  A <- data.frame(rowN, colN, Values, Series)
  avgPerformance <- round(mean(output[ , 1], na.rm = TRUE), 4)
  avgPrecision <- round(mean(output[ , 2], na.rm = TRUE), 4)
  avgRecall <- round(mean(output[ , 3], na.rm = TRUE), 4)

  gg <- ggplot(A, aes(x = colN, y = rowN, fill = Values))
  gg <- gg + geom_tile(color = "white", size = 0.1)
  gg <- gg + scale_fill_viridis(name = "rate")
  gg <- gg + coord_equal()
  gg <- gg + facet_wrap( ~ Series, ncol = 1)
  gg <- gg + labs(x = NULL, y = NULL,
                  title = paste("Feature: ", toString(topFeatureNames), "\n",
                                "Averaged performance ", avgPerformance,
                                ", precision ", avgPrecision,
                                ", recall ", avgRecall))
  gg <- gg + theme_tufte(base_family = "sans")
  gg <- gg + theme(axis.ticks = element_blank())
  gg <- gg + theme(axis.text = element_text(size = 10))
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(plot.title = element_text(hjust = 0))
  gg <- gg + theme(strip.text = element_text(size = 15, hjust = 0))
  gg <- gg + theme(panel.margin.x = unit(0.5, "cm"))
  gg <- gg + theme(panel.margin.y = unit(0.5, "cm"))
  gg <- gg + theme(legend.title = element_text(size = 10))
  gg <- gg + theme(legend.title.align = 1)
  gg <- gg + theme(legend.text = element_text(size = 10))
  gg <- gg + theme(legend.position = "bottom")
  gg <- gg + theme(legend.key.size = unit(0.2, "cm"))
  gg <- gg + theme(legend.key.width = unit(1, "cm"))
  gg
}

BlockSplit <- function(x, blockName, label, discard = FALSE){ # This function seperate blocks which include more than one lable; for example, if a blocm contains both healthy and diseased plants this function break the block into two smaller blocks
  Name1 <- blockName[1]
  Name2 <- blockName[2]
  LabelLevel <- as.vector(unique(x[[label]]))
  blockNameMerge <- paste(as.vector(x[[Name1]]), as.vector(x[[Name2]]), sep = "")
  x$blockNameMerge <- blockNameMerge
  x$idTemp <- seq(nrow(x))
  x$NewBlockNameOne <- as.vector(x[[Name1]])
  x$NewBlockNameTwo <- as.vector(x[[Name2]])
  blockVector <- as.vector(unique(x$blockNameMerge))
  index <- 1
  for(i in seq(length(blockVector))){
    xTemp <- subset(x, blockNameMerge == blockVector[i])  # Select one block of data
    if(length(as.vector(unique(xTemp[[label]]))) != 1){   # If the block contains samples from more than one class
      number1 <- length(which(xTemp[[label]] == LabelLevel[1]))
      number2 <- length(which(xTemp[[label]] == LabelLevel[2]))
      if(number1 >= number2){
        idToChange <- (xTemp[which(xTemp[[label]] == LabelLevel[2]),])$idTemp
        x$NewBlockNameOne[idToChange] <- "NEW_BLOCK_NAME_X"
        indexTemp <- paste("NEW_BLOCK_NAME_Y", index, sep = "_")
        x$NewBlockNameTwo[idToChange] <- rep(indexTemp, times = length(idToChange))
      } else{
        idToChange <- (xTemp[which(xTemp[[label]] == LabelLevel[1]),])$idTemp
        x$NewBlockNameOne[idToChange] <- "NEW_BLOCK_NAME_X"
        indexTemp <- paste("NEW_BLOCK_NAME_Y", index, sep = "_")
        x$NewBlockNameTwo[idToChange] <- rep(indexTemp, times = length(idToChange))
      }
      index <- index + 1
    }
  }
  x$NewBlockNameTwo <- as.character(x$NewBlockNameTwo)
  dropnames <- names(x) %in% c("idTemp", "blockNameMerge")
  y <- x[!dropnames]
  if(discard == TRUE){
    res <- y[- which(y$NewBlockNameOne == "NEW_BLOCK_NAME_X"), ]
    return(res)
  } else{
    return(y)
  }
}

# this function devide the input data into train and test sets
train.test.generation <- function(data=NULL, x=NULL, y=NULL,label=NULL,defaultLabel=NULL, block=NULL, orderby=NULL,testBlockProp=0.2){
  valueTransferData_sds <-PhenoPro7::TransferData(data = data, x = x, y = y, label=label, block=block, orderby = orderby)
  WorkingData_sds<- valueTransferData_sds$data
  block_sds <- valueTransferData_sds$block
  orderby_sds = valueTransferData_sds$orderby

  blockUniqueNames <- unique(WorkingData_sds[[block_sds]])
  blockOrderedNames <- mixedsort(blockUniqueNames)
  labelUniqueNames <- unique(WorkingData_sds[[label]])
  defaultLabelUniqueNames <- defaultLabel
  inverseLabel <- labelUniqueNames[- which(labelUniqueNames == defaultLabel)]

  splitDatabyBlock <- split(WorkingData_sds, as.vector(WorkingData_sds[[block_sds]]))
  FUNALLEQU <- function(x, label){
    return(!any(x[[label]] != x[[label]][1]))
  }
  FUNLABEL <- function(x, label){
    return(x[[label]][1])
  }
  if(!all(sapply(splitDatabyBlock, FUNALLEQU, label = label, simplify = TRUE)))
    stop("data in some 'block' have multiple 'label'")
  blockOrderedLabels <- as.vector(sapply(splitDatabyBlock, FUNLABEL,
                                         label = label, simplify = TRUE))

  WorkingDataSub_sds <- subset(WorkingData_sds,
                               select = c(x, y, label, block, block_sds, orderby_sds))
  features <- c(x, y)

  valueSplitData_sds <-PhenoPro7::SplitData(WorkingDataSub_sds, block_sds, orderby_sds,
                                            testBlockProp, blockOrderedLabels, blockOrderedNames)  # Here the data is divided to train and test data
  dataSplitData_sds <- valueSplitData_sds$data
  trainData_sds <- dataSplitData_sds$train
  testData_sds <- dataSplitData_sds$test
  testName_sds <- dataSplitData_sds$testName

  returnData <- list(
    train.set=trainData_sds,
    test.set=testData_sds,
    testName=testName_sds
  )
  attr(returnData,'class') <- "train.test.generation"
  return(returnData)
}

# This function test an unseen data. The labels of the test set are not passed to this function.
# In fact, this function just gets some test blocks, add a key index to the samples, transforms test blocks one by one, and then concatenates
# concatenates all transformed blocks, predicts the lables using the model trained in the Pheno function and return the predictions and the key indexes.
test.unseen.pheno<-function(WorkingData=NULL, predictive_model=NULL, pca_model = NULL, scales_matrix=NULL, nfeatures=NULL,feature_selection  = NULL, block=NULL, block_orig= NULL, orderby=NULL, step=1, width=6, method="SVM",defaultLabel=NULL, topFeatureFullNames=NULL, topFeatureNames=NULL, x=NULL, y=NULL, prior=TRUE){
  keys<-seq(1:nrow(WorkingData))
  label<-NULL
  # This key is added to the data because the function in next line
  # next line changes the order of samples. Therefore this key will ba later used to keep track of test samples and test labels
  WorkingData[["keys"]]<-keys
  valueTransferData_test <- PhenoPro7::TransferData(WorkingData, x, y, label, block_orig, orderby)
  WorkingData<- valueTransferData_test$data

  x_temp<- x
  y_temp <- y
  if(feature_selection != "PCA"){
  x <- intersect(x_temp, topFeatureFullNames)
  y <- intersect(y_temp, topFeatureFullNames)}
  blockUniqueNames <- unique(WorkingData[[block]])
  #blockOrderedNames <- mixedsort(blockUniqueNames)
  #labelUniqueNames <- unique(WorkingData[[label]])
  defaultLabelUniqueNames <- defaultLabel

  inputData_temp<-NULL
  features <- c(x, y)
  for(i in 1:length(blockUniqueNames)){
    # One testing block is selected
    block_temp<- WorkingData[which(WorkingData$blockTemp==blockUniqueNames[i]),]

    testDataTemp <- subset(block_temp, select = c(features, block,"keys"))
    objectlist <- list(WorkingDataTemp = testDataTemp, step = step,
                       width = width, label=NULL , x = x, y = y,
                       labelUniqueNames = NULL)
    # The selected testing block is transformed
    testBayesianData <- BayesianNIGProcess(objectlist)$Beta
    if(feature_selection=="LDA"){
      lda_test_transformed<- NULL
      for(i in 1:length(topFeatureNames)){
        #lda_model_temp <- eval(parse(text = paste("lda_res",topFeatureNames[i],sep = "_")))
        feature_I <- paste(topFeatureNames[i], "I",sep = "_")
        feature_S <- paste(topFeatureNames[i], "S", sep = "_")
        lda_data_temp <- subset(testBayesianData, select = c(feature_I, feature_S))
        scales_matrix_temp <- as.matrix(scales_matrix[which(scales_matrix[,3]==topFeatureNames[i]),1:2])
        transformed_vector <- as.data.frame((lda_data_temp[,1] * scales_matrix_temp[1])+(lda_data_temp[,2]*scales_matrix_temp[2]))
        colnames(transformed_vector) <-topFeatureNames[i]
        transformed_vector <- as.matrix(transformed_vector)
        lda_test_transformed <- cbind(lda_test_transformed, transformed_vector)

        #plda <- predict(object = lda_model_temp,
        #                 newdata = lda_data_temp)
        #colnames(plda$x) <-paste(topFeatureNames[i])
        #lda_test_transformed <- cbind(lda_test_transformed, plda$x)
        #lda_test_transformed <- scales_matrix



        #plot1 <- ggplot() + geom_point(data = lda_data_temp,
        #                               mapping = aes(lda_data_temp[[1]], lda_data_temp[[2]],colour = factor(1))) + xlab(colnames(lda_data_temp)[1]) + ylab(colnames(lda_data_temp)[2]) + theme(legend.position = "none")
      }
      testBayesianDataTemp <- as.data.frame(lda_test_transformed)
    }else if(feature_selection=="PCA"){
        test_pca_data<-predict(pca_model, newdata = testBayesianData)
        testBayesianDataTemp <- as.data.frame(test_pca_data[,1:nfeatures])

    }else{
      testBayesianDataTemp <- subset(testBayesianData,
                                     select = c(topFeatureNames))
    }
    testBayesianDataTemp[["keys"]]<-block_temp$keys
    testBayesianDataTemp[["block"]]<-block_temp$blockTemp
    # The transformed test block will be appended to the test set
    inputData_temp <- rbind(inputData_temp,testBayesianDataTemp)
  }

  keys_vec<-inputData_temp$keys
  # Just keep the features
  inputData<-inputData_temp[, !names(inputData_temp) %in% c("keys","block")]
  # Predict the samples using the test samples and the already trained model
  par.pred <- predict(predictive_model, inputData)
  # Concatenate block name, index keys and predictions and return that.
  blocks_labels_preds <-  cbind.data.frame(inputData_temp$block, inputData_temp$keys,par.pred)
  colnames(blocks_labels_preds) <- c("blockTemp", "keys", "par.pred")


  returnData <- list(
    blocks_labels_preds=blocks_labels_preds,
    blockNamesONE = block_orig[1],
    blockNamesTWO = block_orig[2],
    used_blocks = blockUniqueNames,
    transformed_testD=inputData,
    test_original=WorkingData,
    test_transformed=inputData_temp,
    topFeatureNames = topFeatureNames
  )
  #attr(returnData,'class') <- "cvPheno"
  return(returnData)
}

# After training a model and testing the model using test blocks, this function gets
# gets the predictions (out put of the "test.unseen.pheno") and actual labels and calculate the performance
calc.performance<-function(blocks_labels_preds=NULL, labels=NULL, pos_label="D"){
  blockUniqueNames<-unique(blocks_labels_preds$block)
  block_based_res<-data.frame(matrix(0, nrow = length(blockUniqueNames), ncol = 7)) # Columns are: block name, II, DD, DI, ID, performance, precision and recall
  block_based_res[,1]<- blockUniqueNames
  colnames(block_based_res)<-c("block_name", "TN", "TP", "FN","FP","label","size")
  rownames(block_based_res)<- blockUniqueNames
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  # This loop uses the key indexes and replace the indexes with actual labels of samples.
  for(i in 1:length(labels)){
    blocks_labels_preds$keys[i]<-as.character(labels[as.integer(blocks_labels_preds$keys[i])])
  }
  for(ind_per in 1:length(blockUniqueNames)){
    # One block is selected
    blocks_labels_preds_temp <- blocks_labels_preds[which(blocks_labels_preds$blockTemp == blockUniqueNames[ind_per]),]
    block_voted_label <- names(which.max(table(blocks_labels_preds_temp$par.pred)))
    block_actual_label <- names(which.max(table(blocks_labels_preds_temp$keys)))
    if(block_voted_label== block_actual_label){
      if(block_voted_label != pos_label){
        TN <- TN+1
        block_based_res[(which(block_based_res$block_name==blockUniqueNames[ind_per])) ,2] <- 1
      }else{
        TP<-TP+1
        block_based_res[(which(block_based_res$block_name==blockUniqueNames[ind_per])) ,3] <- 1
      }
    }else{
      if(block_voted_label==pos_label){
        FP <- FP+1
        block_based_res[(which(block_based_res$block_name==blockUniqueNames[ind_per])) ,5] <- 1
      }else{
        FN <- FN+1
        block_based_res[(which(block_based_res$block_name==blockUniqueNames[ind_per])) ,4] <- 1
      }
    }
    block_based_res[(which(block_based_res$block_name==blockUniqueNames[ind_per])) ,6]<- block_actual_label
    block_based_res[(which(block_based_res$block_name==blockUniqueNames[ind_per])) ,7]<- nrow(blocks_labels_preds_temp)
  }
  performance_t <- (TP+TN)/(TP+TN+FP+FN)
  precision_t <- (TP)/(TP+FP)
  recall_t<- (TP)/(TP+FN)
  tp_t<-TP
  tn_t<-TN
  fp_t<-FP
  fn_t<-FN
  block_based_res <- replace(block_based_res, is.na(block_based_res), 0)
  returnData <- list(
    performance_test=performance_t,
    precision_test=precision_t,
    recall_test=recall_t,
    tp_test=tp_t,
    tn_test=tn_t,
    fn_test= fn_t,
    fp_test = fp_t,
    block_based_res=block_based_res,
    used_blocks = blockUniqueNames
  )
  attr(returnData,'class') <- "cvPheno"
  return(returnData)
}

plot.Bayes.Data <- function(BayesD=NULL, BasesDL=NULL){
  for(i in seq(length(x))){
    for(j in seq(length(y))){
      OriginalDataPlot <- subset(OriginalData, select = c(x[i], y[j], label))
      xName <- paste(x[i], y[j], "I", sep = "_")
      yName <- paste(x[i], y[j], "S", sep = "_")
      BayesianDataPlot <- subset(BayesianData, select = c(xName, yName, "Label"))

      p1 <- ggplot() + geom_point(data = OriginalDataPlot,
                                mapping = aes(OriginalDataPlot[[x[i]]], OriginalDataPlot[[y[j]]],
                                              colour = factor(OriginalDataPlot[[label]]))) +
      xlab(x[i]) +
      ylab(y[j]) + theme(legend.position = "none")
      p2 <- ggplot() + geom_point(data = BayesianDataPlot,
                                mapping = aes(BayesianDataPlot[[xName]], BayesianDataPlot[[yName]],
                                              colour = Label)) +
        xlab("intercept") +
        ylab("Slope")
      p12 <- grid.arrange(p1, p2, ncol = 2)
      p12
    }
  }

}
require(ggplot2)
require(MASS)
require(e1071)
require(randomForest)
require(gridExtra)
require(gtools)
require(viridis)
require(ggthemes)
require(FSelector)



