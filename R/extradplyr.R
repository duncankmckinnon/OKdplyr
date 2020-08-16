

#' Get data frame summary statistics for one or all columns of a dataset (with option to group by target attribute)
#' @description generate summary for a given attribute grouped by a target attribute
#' @importFrom tidyselect all_of
#' @importFrom dplyr vars select_at summarise_at group_by_at count_ summarise group_by_all ungroup n
#' @importFrom magrittr `%>%`
#' @param data a data frame or tibble type object
#' @param summary_attr a column label as a string to summarize
#' @param group_attr a column label to group by before summarizing (optional)
#' @param .checkAssertions private variable used to check function assertions (if first in chain)
#' @return a list containing a summary table for all attributes or the specified summary_attr (if present),
#' grouped by the group_attr (if present)
#' @export
#' @examples
#' # summarise all columns
#' attribute_summary(iris)
#'
#' # summarise only 'Petal.Length'
#' attribute_summary(iris, summary_attr='Petal.Length')
#'
#' # summarise 'Petal.Length' grouped by 'Species'
#' attribute_summary(iris, summary_attr='Petal.Length', group_attr='Species')
#'
#' # summarise all columns grouped by 'Species'
#' attribute_summary(iris, group_attr='Species')
attribute_summary <- function(data, summary_attr = NULL, group_attr = NULL, .checkAssertions = TRUE){

  # check assertions ( used in first attributes request )
  .assertions(data, c(summary_attr, group_attr), isRun = .checkAssertions)

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
  response_data <- select_at(data, .vars = dplyr::vars(all_of(attr_names)))
  return( attributes_summarize( response_data, group_attr, F ) )
}

#' Summarize Attributes
#' @description generate a summary of each attribute in the dataset grouped by a target attribute
#' (helper function for attribute summary)
#' @param data a data frame or tibble type object
#' @param group_attr a column label as a string to group by before summarizing (optional)
#' @param .checkAssertions private variable used to check function assertions (if first in chain)
#' @return a list with stats for each attribute (grouped by group_attr if present)
#' @keywords internal
#' @examples
#' attributes_summarize(iris)
#' attributes_summarize(iris, 'Species')
attributes_summarize <- function(data, group_attr = NULL, .checkAssertions = TRUE){

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
  return( sapply(attributes, function(x) attribute_stats(data, x, F), USE.NAMES = T, simplify = F) )
}

#' Get data frame summary statistics for a column of a data frame or tibble
#' @description get the summary statistics for an attribute (grouped or otherwise)
#' @param data a data frame or tibble type object
#' @param stats_attr a column label as a string to get stats for
#' @param .checkAssertions private variable used to check function assertions (if first in chain)
#' @return a tibble with the summary stats for the specified column/grouping
#' @export
#' @examples
#' attribute_stats(iris)
#' attribute_stats(iris, 'Petal.Length')
attribute_stats <- function(data, stats_attr = NULL, .checkAssertions = TRUE){

  # for requests where column is already selected - syntactic ease of use
  if( missing(stats_attr) && ncol(data) == 1 ) stats_attr <- names(data)

  # check assertions ( used in first attributes request )
  .assertions(data, stats_attr, T, isRun = .checkAssertions)

  # get class for each column in summary data
  attribute_types <- attribute_class(data, F)

  # summarise column by class
  if( attribute_types[stats_attr] %in% bidirected.type ){

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
          'p75' = ~quantile(.x, probs = 0.75, names = F, na.rm = T),
          'max' = ~max(.x, na.rm = T),
          'sd' = ~sd(.x, na.rm = T)
        ))))
  } else if( attribute_types[stats_attr] %in% temporal.type ){

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
          'p75' = ~quantile(.x, probs = 0.75, names = F, type = 1, na.rm = T),
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

#' Get class type for all columns in a data frame or tibble
#' @description get the class for each column in data, as a named character vector
#' @param data a data frame or tibble type object
#' @param .checkAssertions private variable used to check function assertions (if first in chain)
#' @return a character vector of the class type of each column
#' @export
#' @examples
#' chr.ex <- sample(c('a','b','c'), 100, TRUE)
#' int.ex <- sample(c(1,2,3), 100, TRUE)
#' dttm.ex <- sample(as.Date.character(paste('2020-01-', 1:30, sep='')), 100, TRUE)
#' sample.data <- data.frame('chr'=chr.ex, 'int'=int.ex, 'dttm'=dttm.ex)
#'
#' # get data classes for all columns
#' attribute_class(sample.data)
attribute_class <- function(data, .checkAssertions = TRUE){
  # check assertions ( used in first attributes request )
  .assertions(data, isRun = .checkAssertions)

  # get classes for all attributes
  return( sapply(data, class, USE.NAMES = T, simplify = T) )
}


#' Counts and proportions for each unique row in a data frame or tibble
#' @description counts all unique row occurrences, returns a data frame with a count for each row
#' @param data a data frame or tibble type object
#' @param include_percent whether a column should be added to show group percentages
#' @param .checkAssertions private variable used to check function assertions (if first in chain)
#' @return a grouped data frame of unique rows with column count ( and percent )
#' @export
#' @examples
#' unique_counts(cars)
#' unique_counts(iris, include_percent = TRUE)
unique_counts <- function( data, include_percent = TRUE, .checkAssertions = TRUE ) {
  # check assertions ( used in first request )
  .assertions(data, isRun = .checkAssertions)

  # get all unique group counts in data
  grouped_data <- data %>% group_by_all()
  if( include_percent  ) {
    grouped_data <- grouped_data %>% summarise( count = n() , percent = round ( count / nrow( data ), 3 ) )
  } else {
    grouped_data <- grouped_data %>% summarise( count = n() )
  }
  return( grouped_data %>% ungroup() )
}
