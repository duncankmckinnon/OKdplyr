

#' generate summary for a given attribute grouped by a target attribute
#'
#' @export
#' @importFrom tidyselect all_of
#' @importFrom dplyr vars select_at summarise_at group_by_at count_
#' @param data a data frame or tibble type object
#' @param summary_attr a column label as a string to summarize
#' @param group_attr a column label to group by before summarizing (optional)
#' @returns a grouped summary table for the attribute(s)
#' @examples
#' # summarise all columns
#' attribute.summary(iris)
#'
#' # summarise only 'Petal.Length'
#' attribute.summary(iris, summary_attr = 'Petal.Length')
#'
#' # summarise 'Petal.Length' grouped by 'Species'
#' attribute.summary(iris, summary_attr = 'Petal.Length', group_attr = 'Species')
#'
#' # summarise all columns grouped by 'Species'
#' attribute.summary(iris, group_attr = 'Species')
attribute.summary <- function(data, summary_attr = NULL, group_attr = NULL){

  # check assertions ( used in first attributes request )
  .assertions(data, c(summary_attr, group_attr), isRun = T)

  # collect column names needed for summary
  if( !is.null(summary_attr) ) {
    if( !is.null(group_attr) ){
      attr_names <- c(summary_attr, group_attr)
    } else {
      attr_names <- summary_attr
    }
  } else {
    attr_names <- names(data)
  }

  # select columns to use in summarise
  response_data <- select_at(data, .vars = vars(all_of(attr_names)))
  return( attributes.summarise( response_data, group_attr, F ) )
}

#' generate a summary of each attribute in the dataset grouped by a target attribute
#'
#' @param data a data frame or tibble type object
#' @param group_attr a column label as a string to group by before summarizing (optional)
attributes.summarise <- function(data, group_attr = NULL, .checkAssertions = T){

  # check assertions ( used in first attributes request )
  .assertions(data, group_attr, isRun = .checkAssertions)

  # get columns to summarize
  attributes <- names(data)

  # group data by column
  if( !is.null(group_attr) ){
    data <- group_by_at(data, .vars = vars(all_of(group_attr)))

    # remove grouping column from summary columns
    attributes <- attributes[ attributes != group_attr ]
  }
  return( sapply(attributes, function(x) attribute.stats(data, x, F), USE.NAMES = T, simplify = F) )
}

#' get the summary statistics for an attribute (grouped or otherwise)
#'
#' @param data a data frame or tibble type object
#' @param stats_attr a column label as a string to get stats for
#' @returns a tibble with the summary stats for the specified column/grouping
attribute.stats <- function(data, stats_attr = NULL, .checkAssertions = T){

  # for requests where column is already selected - syntactic ease of use
  if( missing(stats_attr) && length(data) == 1 ) stats_attr <- names(data)

  # check assertions ( used in first attributes request )
  .assertions(data, stats_attr, T, isRun = .checkAssertions)

  # get class for each column in summary data
  attribute_types <- attribute.class(data, F)

  # summarise column by class
  if( attribute_types[stats_attr] %in% c('integer', 'numeric', 'complex', 'double') ){

    # summarise numeric type attributes
    return(
      tibble::as_tibble(summarise_at(
        data,
        .vars = vars(all_of(stats_attr)),
        .funs = list(
          'min' = ~min(.x, na.rm = T),
          'p25' = ~quantile(.x, probs = 0.25, names = F, na.rm = T),
          'mean' = ~mean(.x, na.rm = T),
          'median' = ~median(.x, na.rm = T),
          'p75' = ~quantile(.x, probs = 0.25, names = F, na.rm = T),
          'max' = ~max(.x, na.rm = T),
          'sd' = ~sd(.x, na.rm = T)
        ))))
  } else if( attribute_types[stats_attr] == 'Date' ){

    # summarise datetime type attributes
    return(
      tibble::as_tibble(summarise_at(
        data,
        .vars = vars(all_of(stats_attr)),
        .funs = list(
          'min' = ~min(.x, na.rm = T),
          'p25' = ~quantile(.x, probs = 0.25, names = F, type = 1, na.rm = T),
          'mean' = ~mean(.x, na.rm = T),
          'median' = ~median(.x, na.rm = T),
          'p75' = ~quantile(.x, probs = 0.25, names = F, type = 1, na.rm = T),
          'max' = ~max(.x, na.rm = T),
          'sd' = ~sd(.x, na.rm = T)
        ))))
  } else {

    # summarise categorical type attributes
    return(
      suppressWarnings(
        ### need to use deprecated dplyr tools until updated
        tibble::as_tibble(count_( data, stats_attr, sort = T ))))
  }
}

#' get the class for each column in data, as a named character vector
#'
#' @export
#' @param data a data frame or tibble type object
#' @returns a character vector of the class type of each column
#' @examples
#' chr.ex <- sample(c('a','b','c'), 100, TRUE)
#' int.ex <- sample(c(1,2,3), 100, TRUE)
#' dttm.ex <- sample(as.Date.character(paste('2020-01-', 1:30, sep = '')), 100, TRUE)
#' sample.data <- data.frame('chr' = chr.ex, 'int' = int.ex, 'dttm' = dttm.ex)
#'
#' # get data classes for all columns
#' attribute.class(sample.data)
attribute.class <- function(data, .checkAssertions = T){
  # check assertions ( used in first attributes request )
  .assertions(data, isRun = .checkAssertions)

  # get classes for all attributes
  return( sapply(data, class, USE.NAMES = T, simplify = T) )
}



