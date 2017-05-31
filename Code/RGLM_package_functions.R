randomGLM <- function(x, 
                      y, 
                      xtest = NULL, 
                      maxInteractionOrder = 1, 
                      classify = is.factor(y) | length(unique(y)) < 4, 
                      multiClass.global = TRUE, 
                      multiClass.pairwise = FALSE, 
                      multiClass.minObs = 1, 
                      multiClass.ignoreLevels = NULL, 
                      nBags = 100, 
                      replace = TRUE, 
                      sampleWeight = NULL, 
                      nObsInBag = if (replace) nrow(x) else as.integer(0.632 * nrow(x)), 
                      nFeaturesInBag = ceiling(ifelse(ncol(x) <= 10, ncol(x), ifelse(ncol(x) <= 300, (1.0276 - 0.00276 *ncol(x)) * ncol(x), ncol(x)/5))), 
                      minInBagObs = min(max(nrow(x)/2, 5), 2 * nrow(x)/3), 
                      nCandidateCovariates = 50, 
                      corFncForCandidateCovariates = cor, 
                      corOptionsForCandidateCovariates = list(method = "pearson",  use = "p"), 
                      mandatoryCovariates = NULL, 
                      interactionsMandatory = FALSE, 
                      keepModels = is.null(xtest), 
                      thresholdClassProb = 0.5, 
                      interactionSeparatorForCoefNames = ".times.", 
                      randomSeed = 12345, 
                      nThreads = NULL, 
                      verbose = 0) 
{
  ySaved = y
  xSaved = x
  if (classify) {
    originalYLevels = sort(unique(y))
    if (length(originalYLevels) > 2) {
      if (is.na(multiClass.minObs)) 
        multiClass.minObs = 0
      if (multiClass.minObs < 1) {
        .cat.nl("Warning: invalid input of 'multiClass.nimObs' changed to 1.")
        multiClass.minObs = 1
      }
      if (length(originalYLevels) > length(y)/multiClass.minObs | 
          length(originalYLevels) == length(y)) {
        stop("The response 'y' has too many levels for classification.\n", 
             "   Perhaps you should set 'classify = FALSE'?")
      }
      else {
        .cat.nl("randomGLM: transforming multi-level response to a series of binary variables.")
      }
      yBin = .binarizeCategoricalVar(as.character(y), 
                                     minCount = multiClass.minObs, val1 = 0, val2 = 1, 
                                     nameSep = ".vs.", namePrefix = "", ignore = multiClass.ignoreLevels, 
                                     includePairwise = multiClass.pairwise, includeLevelVsAll = multiClass.global, 
                                     levelOrder = NULL)
      nY = ncol(yBin)
      yBinNames = colnames(yBin)
      yBinLevels = attr(yBin, "includedLevels")
      out = list(binaryPredictors = list())
      for (iy in 1:nY) {
        if (verbose > 0) 
          .cat.nl("..Working on binary variable ", yBinNames[iy], 
                  " (", iy, " of ", nY, ")")
        out$binaryPredictors[[iy]] = randomGLM(x = x, 
                                               y = yBin[, iy], xtest = xtest, maxInteractionOrder = maxInteractionOrder, 
                                               classify = classify, nBags = nBags, replace = replace, 
                                               sampleWeight = sampleWeight, nObsInBag = nObsInBag, 
                                               nFeaturesInBag = nFeaturesInBag, nCandidateCovariates = nCandidateCovariates, 
                                               corFncForCandidateCovariates = corFncForCandidateCovariates, 
                                               corOptionsForCandidateCovariates = corOptionsForCandidateCovariates, 
                                               mandatoryCovariates = mandatoryCovariates, 
                                               interactionsMandatory = interactionsMandatory, 
                                               keepModels = keepModels, thresholdClassProb = thresholdClassProb, 
                                               interactionSeparatorForCoefNames = interactionSeparatorForCoefNames, 
                                               randomSeed = randomSeed, nThreads = nThreads, 
                                               verbose = verbose - 1)
      }
      names(out$binaryPredictors) = yBinNames
      out$predictedOOB = as.matrix(sapply(out$binaryPredictors, 
                                          getElement, "predictedOOB"))
      colnames(out$predictedOOB) = .spaste("PredictionFor.", 
                                           yBinNames)
      out$predictedOOB.response = do.call(cbind, lapply(out$binaryPredictors, 
                                                        getElement, "predictedOOB.response"))
      responseNames = .spaste(rep(yBinNames, rep(2, nY)), 
                              ".ProbabilityOfClass.", as.vector(yBinLevels))
      colnames(out$predictedOOB.response) = responseNames
      rownames(out$predictedOOB.response) = rownames(out$predictedOOB) = rownames(x)
      if (!is.null(xtest)) {
        out$predictedTest = sapply(out$binaryPredictors, 
                                   getElement, "predictedTest")
        colnames(out$predictedTest) = yBinNames
        out$predictedTest.response = do.call(cbind, 
                                             lapply(out$binaryPredictors, getElement, "predictedTest.response"))
        colnames(out$predictedOOB.response) = responseNames
        rownames(out$predictedTest) = rownames(out$predictedTest.response) = rownames(xtest)
      }
      out$levelMatrix = yBinLevels
      out$thresholdClassProb = thresholdClassProb
      class(out) = c("randomGLM", class(out))
      return(out)
    }
    y = as.numeric(as.factor(y)) - 1
    numYLevels = sort(unique(y))
    minY = min(y, na.rm = TRUE)
    maxY = max(y, na.rm = TRUE)
  }
  else {
    if (!is.numeric(y)) 
      stop("For quantitative prediction, the response must be numeric.")
  }
  x = as.matrix(x)
  featureNames.original = colnames(x)
  nVars = ncol(x)
  if (is.null(featureNames.original)) {
    featureNames.original = featureNames = .spaste("F", 
                                                   .prependZeros(c(1:nVars)))
    colnames(x) = featureNames.original
    namesChanged = FALSE
  }
  else {
    featureNames = make.names(featureNames.original, unique = TRUE)
    if (isTRUE(all.equal(featureNames, featureNames.original))) {
      namesChanged = FALSE
    }
    else {
      namesChanged = TRUE
      nameTranslationTable = data.frame(Column = c(1:nVars), 
                                        OriginalName = featureNames.original, CoefficientName = featureNames)
      colnames(x) = featureNames
    }
  }
  if (length(y) != nrow(x)) {
    stop("x and y must have the same number of observations.")
  }
  nSamples = length(y)
  if (nSamples < 8) {
    .cat.nl("*****************************************************************\n", 
            "* Warning in randomGLM: there are 7 or fewer observations.\n", 
            "*   This may be too few to perform meaningful model selection\n", 
            "*   on in-bag (i.e., even fewer) samples.\n", "*   Model selection algorithm will likely output additional warnings.\n", 
            "*   The resulting predictor should be used with caution.\n", 
            "*****************************************************************")
  }
  nonMandatCovars = setdiff(c(1:nVars), mandatoryCovariates)
  nMandatoryCovariates = length(mandatoryCovariates)
  if (nVars < nFeaturesInBag) {
    nFeaturesInBag = ncol(x)
    .cat.nl("Warning in randomGLM: nFeaturesInBag is larger than number of features (ncol(x)).\n", 
            "   Will use nFeaturesInBag equal to the number of features.")
  }
  if (nCandidateCovariates > nFeaturesInBag) {
    .cat.nl("Warning in randomGLM: nCandidateCovariates is larger than nFeaturesInBag.\n", 
            "  Will use nCandidateCovariates=nFeaturesInBag")
    nCandidateCovariates = nFeaturesInBag
  }
  mandatoryCovarsGiven = !is.null(mandatoryCovariates)
  if (mandatoryCovarsGiven & nCandidateCovariates < length(mandatoryCovariates)) {
    stop("Error: number of mandatoryCovariates >= nCandidateCovariates")
  }
  if (thresholdClassProb < 0 | thresholdClassProb > 1) 
    stop("Error: thresholdClassProb takes values between 0  and 1.")
  xSD = apply(x, 2, sd, na.rm = TRUE)
  validFeatures = xSD > 0
  x[, !validFeatures] = 0
  doTest = !is.null(xtest)
  if (doTest) {
    xtestSaved = xtest
    xtest = as.matrix(xtest)
    if (ncol(x) != ncol(xtest)) 
      stop("Number of learning and testing predictors (columns of x, xtest) must equal.")
    if (!is.null(colnames(xtest))) {
      if (!isTRUE(all.equal(colnames(xtest), featureNames.original))) 
        stop("Column names of 'x' and 'xtest' disagree.")
    }
    colnames(xtest) = colnames(x)
    nTestSamples = nrow(xtest)
    xtest[, !validFeatures] = 0
    predictedTestMat = matrix(NA, nTestSamples, nBags)
  }
  if (!is.null(randomSeed)) {
    if (exists(".Random.seed")) {
      saved.seed = .Random.seed
      seedSaved = TRUE
    }
    else seedSaved = FALSE
    set.seed(randomSeed)
  }
  on.exit(.disableThreads())
  nThreads = .enableThreads(nThreads, verbose)
  combinePredictors = function(...) {
    preds = list(...)
    out = list()
    out$predictedMat = sapply(preds, getElement, "predicted")
    if (doTest) 
      out$predictedTestMat = sapply(preds, getElement, 
                                    "predictedTest")
    out$candidateFeatures = lapply(preds, getElement, "candidateFeatures")
    out$featuresInForwardRegression = lapply(preds, getElement, 
                                             "featuresInForwardRegression")
    out$coefOfForwardRegression = lapply(preds, getElement, 
                                         "coefOfForwardRegression")
    out$interceptOfForwardRegression = sapply(preds, getElement, 
                                              "interceptOfForwardRegression")
    out$models = lapply(preds, getElement, "model")
    out
  }
  bagFeatures = matrix(NA, nFeaturesInBag, nBags)
  bagObsIndx = matrix(NA, nObsInBag, nBags)
  for (bag in 1:nBags) {
    yBagVar = 0
    while (yBagVar == 0) {
      bagSamples = sample(nSamples, nObsInBag, replace = replace, 
                          prob = sampleWeight)
      yBag = y[bagSamples]
      yBagVar = var(yBag, na.rm = TRUE)
      nUniqueInBag = length(unique(bagSamples))
      if (nUniqueInBag == nSamples | nUniqueInBag < minInBagObs) 
        yBagVar = 0
    }
    bagObsIndx[, bag] = bagSamples
    bagFeatures[, bag] = c(mandatoryCovariates, sample((1:nVars)[nonMandatCovars], 
                                                       nFeaturesInBag - nMandatoryCovariates))
  }
  singleBagIteration = function(bag, verbose) {
    if (verbose > 0) {
      .cat.nl("..bag ", bag)
    }
    out = list()
    bagSamples = bagObsIndx[, bag]
    oob = c(1:nSamples)[-unique(bagSamples)]
    nOOB = length(oob)
    features = bagFeatures[, bag]
    xBag = x[bagSamples, features, drop = FALSE]
    yBag = y[bagSamples]
    if (doTest) {
      xTestBag = rbind(x[oob, features, drop = FALSE], 
                       xtest[, features, drop = FALSE])
    }
    else {
      xTestBag = x[oob, features, drop = FALSE]
    }
    pr = .forwardSelection(xBag, yBag, xTestBag, classify = classify, 
                           maxInteractionOrder = maxInteractionOrder, nCandidateCovariates = nCandidateCovariates, 
                           corFncForCandidateCovariates = corFncForCandidateCovariates, 
                           corOptionsForCandidateCovariates = corOptionsForCandidateCovariates, 
                           NmandatoryCovariates = nMandatoryCovariates, interactionsMandatory = interactionsMandatory, 
                           keepModel = keepModels, interactionSeparatorForCoefNames = interactionSeparatorForCoefNames)
    out$predicted = rep(NA, nSamples)
    out$predicted[oob] = pr$predicted[1:nOOB]
    if (doTest) {
      out$predictedTest = pr$predicted[(nOOB + 1):(nOOB + 
                                                     nTestSamples)]
    }
    dictionary = rbind(c(0, 0), cbind(1:nFeaturesInBag, 
                                      features))
    out$candidateFeatures = .translate(pr$candidateFeatures, 
                                       dictionary)
    out$featuresInForwardRegression = .translate(pr$featuresInForwardRegression, 
                                                 dictionary)
    out$coefOfForwardRegression = pr$coefOfForwardRegression
    out$interceptOfForwardRegression = pr$interceptOfForwardRegression
    if (keepModels) 
      out$model = pr$model
    rm(xBag, pr, xTestBag, features, bagSamples, oob, yBag)
    out
  }
  if (nThreads > 1) {
    ensemble = foreach(bag = 1:nBags, .combine = combinePredictors, 
                       .multicombine = TRUE, .maxcombine = nBags) %dopar% 
      singleBagIteration(bag, verbose = verbose)
  }
  else {
    bagRes = list()
    for (bag in 1:nBags) {
      bagRes[[bag]] = singleBagIteration(bag, verbose = verbose)
    }
    ensemble = do.call(combinePredictors, bagRes)
  }
  featuresInForwardRegression.all = do.call(cbind, ensemble$featuresInForwardRegression)
  timesSelectedByForwardRegression = .countsInInteractionMatrix(featuresInForwardRegression.all, 
                                                                nVars)
  colnames(bagObsIndx) = names(ensemble$candidateFeatures) = names(ensemble$featuresInForwardRegression) = names(ensemble$coefOfForwardRegression) = names(ensemble$interceptOfForwardRegression) = .spaste("Bag", 
                                                                                                                                                                                                            1:nBags)
  if (!is.null(colnames(xSaved))) {
    colnames(timesSelectedByForwardRegression) = colnames(xSaved)
  }
  predictedOOB.response1 = rowMeans(ensemble$predictedMat, 
                                    na.rm = TRUE)
  predictedOOB.response1[rowSums(!is.na(ensemble$predictedMat)) == 
                           0] = NA
  if (!is.null(rownames(xSaved))) {
    names(predictedOOB.response1) = rownames(xSaved)
  }
  out = list(predictedOOB.response = predictedOOB.response1, 
             predictedOOB = predictedOOB.response1, candidateFeatures = ensemble$candidateFeatures, 
             featuresInForwardRegression = ensemble$featuresInForwardRegression, 
             coefOfForwardRegression = ensemble$coefOfForwardRegression, 
             interceptOfForwardRegression = ensemble$interceptOfForwardRegression, 
             bagObsIndx = bagObsIndx, timesSelectedByForwardRegression = timesSelectedByForwardRegression, 
             models = if (keepModels) ensemble$models else NULL, 
             featureNamesChanged = namesChanged, nameTranslationTable = if (namesChanged) nameTranslationTable else NULL, 
             classify = classify, nFeatures = nVars, maxInteractionOrder = maxInteractionOrder, 
             yLevels = if (classify) originalYLevels else NULL, x.original = xSaved, 
             y.original = ySaved, x = x, y = y, thresholdClassProb = thresholdClassProb)
  if (doTest) {
    predictedTest.response1 = rowMeans(ensemble$predictedTestMat, 
                                       na.rm = TRUE)
    predictedTest.response1[rowSums(!is.na(ensemble$predictedTestMat)) == 
                              0] = NA
    if (!is.null(rownames(xtestSaved))) {
      names(predictedTest.response1) = rownames(xtestSaved)
    }
    out$predictedTest.response = predictedTest.response1
    out$predictedTest = predictedTest.response1
  }
  if (classify) {
    predictedOOB = ifelse(predictedOOB.response1 > thresholdClassProb, 
                          1, 0)
    predictedOOB = originalYLevels[predictedOOB + 1]
    predictedOOB.response = cbind(1 - predictedOOB.response1, 
                                  predictedOOB.response1)
    colnames(predictedOOB.response) = as.character(originalYLevels)
    out$predictedOOB = predictedOOB
    out$predictedOOB.response = predictedOOB.response
    if (doTest) {
      predictedTest = ifelse(predictedTest.response1 > 
                               thresholdClassProb, 1, 0)
      predictedTest = originalYLevels[predictedTest + 
                                        1]
      predictedTest.response = cbind(1 - predictedTest.response1, 
                                     predictedTest.response1)
      colnames(predictedTest.response) = as.character(originalYLevels)
      out$predictedTest.response = predictedTest.response
      out$predictedTest = predictedTest
    }
  }
  class(out) = c("randomGLM", class(out))
  out
}



.forwardSelection <- function (xBag, yBag, xTestBag, classify, maxInteractionOrder, 
          nCandidateCovariates, corFncForCandidateCovariates, corOptionsForCandidateCovariates, 
          NmandatoryCovariates, interactionsMandatory, keepModel, 
          interactionSeparatorForCoefNames) 
{
  indxMiss = apply(is.na(xBag), 2, sum) > 0
  indxVar = apply(xBag, 2, var, na.rm = T) == 0
  removeFeatures = indxMiss | indxVar
  xBag = xBag[, !removeFeatures, drop = F]
  xTestBag = xTestBag[, !removeFeatures, drop = F]
  nFeatures = ncol(xBag)
  if (NmandatoryCovariates > 0) {
    mandatCovars = c(1:NmandatoryCovariates)
    mandatCovars = mandatCovars[!removeFeatures[mandatCovars]]
    NmandatoryCovariates = length(mandatCovars)
  }
  else mandatCovars = numeric(0)
  nonMandatCovars = setdiff(c(1:nFeatures), mandatCovars)
  interactionMatrix = .interactionMatrix(nFeatures, maxInteractionOrder, 
                                         originalNames = colnames(xBag), setColNames = TRUE, 
                                         featureSeparator = interactionSeparatorForCoefNames)
  if (interactionsMandatory) {
    mandatoryInteractions = apply(interactionMatrix, 2, 
                                  function(x) {
                                    any(x %in% mandatCovars)
                                  })
  }
  else mandatoryInteractions = mandatCovars
  nMandatoryInteractions = length(mandatoryInteractions)
  if (nMandatoryInteractions > nCandidateCovariates) 
    stop("Number of mandatory interactions is larger than number of candidate covariates.")
  nInteractions = ncol(interactionMatrix)
  nonMandatInteractions = setdiff(c(1:nInteractions), mandatoryInteractions)
  x.int = .generateInteractions(xBag, maxInteractionOrder, 
                                interactionMatrix = interactionMatrix, setColNames = TRUE)
  xTest.int = .generateInteractions(xTestBag, maxInteractionOrder, 
                                    interactionMatrix = interactionMatrix, setColNames = TRUE)
  corOptionsForCandidateCovariates$x = x.int
  corOptionsForCandidateCovariates$y = yBag
  absGS = abs(do.call(corFncForCandidateCovariates, corOptionsForCandidateCovariates))
  nCandidateCovariates = min(nCandidateCovariates, ncol(x.int))
  rank = rank(-absGS[nonMandatInteractions], ties.method = "f")
  indx = c(mandatoryInteractions, nonMandatInteractions[rank <= 
                                                          (nCandidateCovariates - nMandatoryInteractions)])
  x.int = x.int[, indx, drop = F]
  xTest.int = xTest.int[, indx, drop = F]
  absGS = absGS[indx]
  candidateFeatures = interactionMatrix[, indx, drop = FALSE]
  featureMax = which.max(absGS)
  modelData = data.frame(x.int)
  rm(xBag, xTestBag, x.int, corOptionsForCandidateCovariates, 
     interactionMatrix)
  initialFormula = paste("yBag~", paste(colnames(modelData)[if (nMandatoryInteractions > 
                                                                0) 
    mandatoryInteractions
    else featureMax], collapse = " + "))
  if (classify) {
    lmInit = glm(formula(initialFormula), data = modelData, 
                 family = binomial(link = "logit"))
    lmUpper = glm(yBag ~ ., data = modelData, family = binomial(link = "logit"))
  }
  else {
    lmInit = lm(formula(initialFormula), data = modelData)
    lmUpper = lm(yBag ~ ., data = modelData)
  }
  model = stepAIC(lmInit, scope = list(upper = lmUpper), direction = "forward", 
                  trace = FALSE)
  sum = summary(model)
  selected = rownames(sum$coef)[-1]
  featuresInForwardRegression = candidateFeatures[, match(selected, 
                                                          colnames(candidateFeatures)), drop = FALSE]
  coefOfForwardRegression = sum$coef[-1, 1]
  interceptOfForwardRegression = sum$coef[1, 1]
  outHat = predict(model, newdata = as.data.frame(xTest.int), 
                   type = "response")
  out = list(predicted = outHat, candidateFeatures = candidateFeatures, 
             featuresInForwardRegression = featuresInForwardRegression, 
             coefOfForwardRegression = coefOfForwardRegression, interceptOfForwardRegression = interceptOfForwardRegression, 
             model = if (keepModel) model else NULL)
  varList = ls(all.names = TRUE)
  rm(list = setdiff(varList, c("out")))
  out
}
