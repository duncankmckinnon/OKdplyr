
#' extract all performance metrics for a binomial classification model
#'
#' @export
#' @param mod a general linear model of family binomial
#' @param pi_0 the threshold of determination used in classifaction (optional, default = 0.5)
#' @return A list of classification performance metrics derived from the confusion matrix
#' \itemize{
#'  \item{y.hat}{      : the model classification predictions}
#'  \item{confmatrix}{ : the confusion matrix}
#'  \item{tpos}{       : count of true positives}
#'  \item{tneg}{       : count of true negatives}
#'  \item{fneg}{       : count of false negatives}
#'  \item{fpos}{       : count of false positives}
#'  \item{acc}{        : accuracy score}
#'  \item{recall}{     : recall score}
#'  \item{precision}{  : precision score}
#'  \item{tpr}{        : true positive rate (same as recall)}
#'  \item{fpr}{        : false positive rate}
#'  \item{fdcr}{       : false discovery rate}
#'  \item{fnr}{        : false negative rate}
#'  \item{fomr}{       : false omission rate}
#'  \item{tnr}{        : true negative rate}
#'  \item{ppv}{        : positive predicted value (same as precision)}
#'  \item{npv}{        : negative predicted value}
#'  \item{F1}{         : F1 scoring metric}
#' }
#' @examples
#' X <- rnorm(100)
#' Y <- ifelse(X < 0.5, 0, 1 - sample(0:1, prob = c(0.9, .1)))
#' f <- glm(Y ~ X, family='binomial')
#' classification_metrics(f)
classification_metrics <- function(mod, y, y.hat, pi_0 = 0.5){

  # assert basic requirements for either model or vector input
  assertthat::assert_that(missing(y) != missing(mod))
  assertthat::assert_that(is.double(pi_0) && pi_0 > 0 && pi_0 < 1)
  if( !missing(mod) ){
    assertthat::assert_that(all(class(mod) == c('glm', 'lm')))
    y.hat <- ifelse(mod$fitted.values >= pi_0, 1, 0)
    y <- mod$y
  } else {
    assertthat::assert_that(missing(y) == missing(y.hat))
    assertthat::assert_that(is.numeric(y) && is.numeric(y.hat))
    assertthat::assert_that(length(y) == length(y.hat))
    y.hat <- ifelse(y.hat >= pi_0, 1, 0)
  }

  # get y and y.hat as factor (for confusion matrix)
  y.hat <- factor(y.hat, levels = c(1,0), labels = c('1','0'), ordered = T)
  y <- factor(y, levels = c(1,0), labels = c('1','0'), ordered = T)

  return( .classification_metrics(y, y.hat) )
}


.classification_metrics <- function(y, y.hat){
  # get confusion matrix and individual entries
  confm <- table(y.hat, y)
  zeallot::`%<-%`(c(tp,fn,fp,tn), confm)

  # calculate and return all classification metrics
  recall <- (tp / (tp + fn))
  precision <- (tp / (tp + fp))
  return(
    list(
      'y.hat' = y.hat,
      'confmatrix' = confm,
      'tpos' = tp,
      'tneg' = tn,
      'fneg' = fn,
      'fpos' = fp,
      'acc' = (tn + tp) / sum(confm),
      'recall' = recall,
      'precision' = precision,
      'tpr' = recall,
      'fpr' = (fp / (tn + fp)),
      'fdcr' = (fp / (tp + fp)),
      'fnr' = (fn / (tp + fn)),
      'fomr' = (fn / (tn + fn)),
      'tnr' = (tn / (tn + fp)),
      'ppv' = precision,
      'npv' = (tn / (tn + fn)),
      'F1' = 2 * ((precision * recall) / (precision + recall))
    )
  )
}

#' create training, testing and validation datasets in specified proportion
#'
#' @export
#' @param data a data.frame type object to split
#' @param prop_train proportion of data in training set
#' @param prop_test proportion of data in test set
#' @param prop_validation proportion of data in validation set (optional)
#' @return a list containing the train and test data (and validation data if proportion > 0)
#' @examples
#' data.ex <- data.frame('A' = sample(1:100,100), 'B' = sample(101:200,100))
#' train_test_val(data.ex, 0.6)
#' train_test_val(data.ex, 0.6, 0.3, 0.1)
train_test_val <- function(data, prop_train = 0.8, prop_test = 0, prop_validation = 0){

  # should be easy to only specify training size
  prop_test <- ifelse(prop_test == 0 && prop_validation == 0, (1 - prop_train), prop_test)

  # all assertions
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(all( c(prop_train, prop_test, prop_validation) >= 0 ))
  assertthat::assert_that(assertthat::are_equal((prop_train + prop_test + prop_validation), 1))

  # split train and test set using multiple assignment
  zeallot::`%<-%`(c(train, test), data.split(data, ( prop_test + prop_validation )))

  if(prop_validation > 0){
    # get proportion split between test and validation
    validation_split <- prop_validation / (prop_test + prop_validation)

    # split out test and validation set from test set using multiple assignment
    zeallot::`%<-%`(c(test, validation), data.split(test, validation_split))
    return(
      list(
        'train' = train,
        'test' = test,
        'validation' = validation
      )
    )
  } else {
    return(
      list(
        'train' = train,
        'test' = test
      )
    )
  }
}


#' helper function for splitting out data sets by proportion
#'
#' @param data a data.frame type object
#' @param prop the proportion of the data in the secondary (smaller) subset
#' @return a list with the primary and secondary subsets in specified proportion
data.split <- function(data, prop){
  zeallot::`%<-%`(c(n,m), dim(data))

  # sample indices for secondary subset
  indices <- sample(1:n, ceiling(prop * n))

  return(
    list(
      data[-indices,],
      data[indices,]
    )
  )
}

