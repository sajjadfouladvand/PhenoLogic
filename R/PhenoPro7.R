CheckArguments <- function(x, ...) UseMethod("CheckArguments")

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
		Label = blockOrderedLabels)

	splitDatabyLabels <- split(blockDataFrame, as.vector(blockDataFrame$Label))
	countSplitDatabyLabels <- sapply(splitDatabyLabels, nrow, simplify = TRUE)
	countNamesTemp <- names(countSplitDatabyLabels)
	countLabelsTemp <- as.vector(countSplitDatabyLabels)
	testSizes <- ceiling(countLabelsTemp * testBlockProp)

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

	returnData <- list(test = testData, train = trainData,
		testName = testNames)
	return(list(data = returnData))
}

TransferData <- function(data, x, y, label, block, orderby){

	# to data frame
	WorkingData <- as.data.frame(data)
	WorkingData <- na.omit(WorkingData)

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
			x, y, label, block, orderby))
		WorkingDataSub <- WorkingDataSub[order(WorkingDataSub[[orderby]],
			decreasing = FALSE), ]
	} else{
		WorkingDataSub <- subset(WorkingData, select = c(
			x, y, block, orderby))
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
		for(i in seq(length(x))){
			for(j in seq(length(y))){
				featuresTEMP <- c(x[i], y[j])
				splitDatabyLabels <- split(WorkingData,
					as.vector(WorkingData[[label]]))
				splitDataLabelNames <- names(splitDatabyLabels)
				interceptSplitData <- c()
				slopeSplitData <- c()
				labelSplitData <- c()
			# 	 if(is.null(dim(splitDatabyLabels$S)) || is.null(dim(splitDatabyLabels$M)) || is.null(dim(splitDatabyLabels$N))){
			# #	   next
			# 	   print("ddd")
			# 	 }
				for(k in seq(length(labelUniqueNames))){
				  # print("******************")
				  # print(dim(splitDatabyLabels$S))
				  # print(dim(splitDatabyLabels$M))
				  # print(dim(splitDatabyLabels$N))
		#		  if(labelUniqueNames[k]=='M' && (is.null(dim(splitDatabyLabels$M)))){
				    #if(is.null(dim(splitDatabyLabels$M))){
				    #DataTemp<-NULL
				    #xTemp <- NULL
				    #yTemp <- NULL
				    #labelTemp <- NULL
				    #}
		#		    next
		#		  }
		#		  if(labelUniqueNames[k]=='N' && (is.null(dim(splitDatabyLabels$N)))){
				   # if(is.null(dim(splitDatabyLabels$N))){
				  #  DataTemp<-NULL
				   # xTemp <- NULL
				  #  yTemp <- NULL
				  #  labelTemp <- NULL
				    #}
		#		    next
		#		  }
		#		  if(labelUniqueNames[k]=='S' && (is.null(dim(splitDatabyLabels$S)))){
				 #   if(is.null(dim(splitDatabyLabels$S))){
				  #  DataTemp<-NULL
				  #  xTemp <- NULL
				   # yTemp <- NULL
				  #  labelTemp <- NULL
				#    }
		#		    next
			#	  }

					DataTemp <- splitDatabyLabels[[k]]
					xTemp <- DataTemp[[featuresTEMP[1]]]
					yTemp <- DataTemp[[featuresTEMP[2]]]
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
				}
				BetaTemp <- cbind(interceptSplitData, slopeSplitData)
				colnames(BetaTemp) <- c(paste(x[i], y[j], "I", sep = "_"),
					paste(x[i], y[j], "S", sep = "_"))
				Beta <- cbind(Beta, BetaTemp)
			}
		}
		Beta <- as.data.frame(Beta)
		Beta <- data.frame(Beta, Label = labelSplitData)
	} else{
		for(i in seq(length(x))){
			for(j in seq(length(y))){
				featuresTEMP <- c(x[i], y[j])
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

	valueTransferData <- TransferData(data, x, y, label, block, orderby) # TransferData is a function to remove irrelevant features
	WorkingData <- valueTransferData$data
	RawData <- WorkingData
	block <- valueTransferData$block
	orderby <- valueTransferData$orderby

	if(!is.null(label)){
		labelUniqueNames <- unique(WorkingData[[label]]) # LabelUniqueNames includes label values like M, N and S

		features <- c(x, y) # features includes both x and y (i.e. all  environmental and phenotype features
		WorkingDataTemp <- subset(WorkingData, select = c(features, label)) #WorkingDataTemp includes only environmental and phenotype features and the label
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
    if(feature_selection=="PCA"){
	  	resPCA <- prcomp(subset(WorkingDataTemp, select = - Label),
		  	center = TRUE, scale. = TRUE)

		  propvar <- as.vector(summary(resPCA)$importance[2, ])
		  cumuvar <- as.vector(summary(resPCA)$importance[3, ])
		  numberPCA <- max(2, which(cumuvar > 0.7)[1])
		  resRotation <- resPCA$rotation[ , (1 : (numberPCA))]
		  nameTemp <- rownames(resRotation)
		  scaleRotation <- t(t(abs(resRotation)) * propvar)

		  # searching top feature names combining (I, S)
  #		nameSplit <- strsplit(nameTemp, "_")
  #		rowSumsTemp <- as.vector(rowSums(scaleRotation))
  #		feaSums <- rep(0, length(rowSumsTemp) / 2)
  #		feaNams <- rep(0, length(rowSumsTemp) / 2)
  #		for(i in seq(length(rowSumsTemp) / 2)){
  #			feaSums[i] <- rowSumsTemp[2 * i - 1] + rowSumsTemp[2 * i]
  #			feaNams[i] <- paste(nameSplit[[2 * i]][1], nameSplit[[2 * i]][2],
  #				sep = "_")
  #		}
  #		featureTemp <- data.frame(feaNams, feaSums)
  #		featureTemp <- featureTemp[order(featureTemp$feaSums, decreasing = TRUE), ]
  #		topFeatureNames <- as.vector(featureTemp$feaNams[1 : min(nrow(featureTemp), 3)])

		# searching top feature names not combining (I, S)

	  	rowSumsTemp <- as.vector(rowSums(scaleRotation))
		  feaSums <- rowSumsTemp
		  feaNams <- as.vector(nameTemp)
		  featureTemp <- data.frame(feaNams, feaSums)
		  featureTemp <- featureTemp[order(featureTemp$feaSums, decreasing = TRUE), ]
		  if(is.null(nfeatures)){
			  topFeatureNames <- as.vector(featureTemp$feaNams)
		  } else{
			  if(nfeatures < nrow(featureTemp)){
				  topFeatureNames <- as.vector(featureTemp$feaNams[1 : min(nrow(featureTemp), nfeatures)])
			  } else{
				  stop("please select a smaller 'nfeatures'")
			  }
		  }


		  fullFeatureNames <- as.vector(featureTemp$feaNams)

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

		  WorkingDataTempPCA <- subset(BayesianData, select = c(topFeatureNames,
			"Label"))
    }else if(feature_selection =="NA")
		{
      #*******************************************************
      #Added by Sajjad Fouladvand

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
      # End of added by Sajjad Fouladvand


      #*******************************************************
    }else if(feature_selection=="lmFuncs" || feature_selection=="nbFuncs" || feature_selection=="rfFuncs" || feature_selection=="treebagFuncs")
		{
      library(caret)
      library(mlbench)
      library(Hmisc)
      library(randomForest)
		  #Added by Sajjad Fouladvand
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
		  # End of added by Sajjad Fouladvand
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
	#Added by Sajjad
  if(feature_selection=="PCA"){
    resPCA <- resPCA
    numComp <- numberPCA
    fea <- topFeatureNames
    selected_features=NULL
  }else if(feature_selection=="NA"){
    resPCA <- NULL
    numComp = NULL
    fea <- topFeatureNames
    selected_features <- topFeatureNames
  }	else{
	  resPCA <- NULL
	  numComp = NULL
	  fea <- lmProfile$optVariables
	  selected_features=lmProfile$optVariables

	}
	#Added by Sajjad
	returnData <- list(
		Raw = RawData,
		Org = OriginalData,
		Bay = BayesianData, # add topFeatureNames as their column names
		pre = predictData,
		EST = EstParametersList,
#		clu = clusterData,
#		cv = crossvadalitionData,
		arg = list(label = label, x = x, y = y, method = method,
			block = block, defaultLabel = defaultLabel, orderby = orderby,
			step = step, width = width, blockNamesONE = blockNamesONE,
		blockNamesTWO = blockNamesTWO),
#		pca = WorkingDataTempPCA, # matrix after rotation
		resPCA = resPCA,
		numComp = numComp,
		#fea = topFeatureNames,   # COMMENTED BY SAJJAD
    # Added by Sajjad
    #fea = lmProfile$optVariables,
    fea=fea,
    # Added by Sajjad
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

plot.Pheno <- function(object){
	arg <- object$arg
	x <- arg$x
	y <- arg$y
	label <- arg$label
	OriginalData <- object$Org
	BayesianData <- object$Bay

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
#Added by Sajjad
BlockSplit <- function(x, blockName, label, discard = FALSE){
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
    xTemp <- subset(x, blockNameMerge == blockVector[i])
    if(length(as.vector(unique(xTemp[[label]]))) != 1){
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
#Added by Sajjad
test.pheno<-function(WorkingData=NULL, block=NULL, label=NULL, orderby=NULL, step=1, width=6, method="SVM",defaultLabel=NULL, blockNamesONE=NULL, blockNamesTWO=NULL, topFeatureFullNames=NULL, topFeatureNames=NULL, x=NULL, y=NULL, feaf=NULL, fea=NULL, cvNumber=10, testBlockProp=0.2, prior=TRUE){
  #WorkingData <- object$Raw
  #argumentsDataTemp <- object$arg
  #block <- argumentsDataTemp$block
  #label <- argumentsDataTemp$label
  #orderby <- argumentsDataTemp$orderby
  #step <- argumentsDataTemp$step
  #width <- argumentsDataTemp$width
  #method <- argumentsDataTemp$method
  #defaultLabel <- argumentsDataTemp$defaultLabel
  #blockNamesONE <- argumentsDataTemp$blockNamesONE
  #blockNamesTWO <- argumentsDataTemp$blockNamesTWO

  #topFeatureFullNames <- object$feaf
  #topFeatureNames <- object$fea
  #valueTransferData_test <- PhenoPro7::TransferData(data, x, y, label, block, orderby)
  x_temp<- x
  y_temp <- y
  #x <- intersect(argumentsDataTemp$x, topFeatureFullNames)
  #y <- intersect(argumentsDataTemp$y, topFeatureFullNames)
  x <- intersect(x_temp, topFeatureFullNames)
  y <- intersect(y_temp, topFeatureFullNames)
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

require(ggplot2)
require(MASS)
require(e1071)
require(randomForest)
require(gridExtra)
require(gtools)
require(viridis)
require(ggthemes)



