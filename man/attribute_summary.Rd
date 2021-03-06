% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extradplyr.R
\name{attribute_summary}
\alias{attribute_summary}
\title{Get data frame summary statistics for one or all columns of a dataset (with option to group by target attribute)}
\usage{
attribute_summary(
  data,
  summary_attr = NULL,
  group_attr = NULL,
  .checkAssertions = TRUE
)
}
\arguments{
\item{data}{a data frame or tibble type object}

\item{summary_attr}{a column label as a string to summarize}

\item{group_attr}{a column label to group by before summarizing (optional)}

\item{.checkAssertions}{private variable used to check function assertions (if first in chain)}
}
\value{
a list containing a summary table for all attributes or the specified summary_attr (if present),
grouped by the group_attr (if present)
}
\description{
generate summary for a given attribute grouped by a target attribute
}
\examples{
# summarise all columns
attribute_summary(iris)

# summarise only 'Petal.Length'
attribute_summary(iris, summary_attr='Petal.Length')

# summarise 'Petal.Length' grouped by 'Species'
attribute_summary(iris, summary_attr='Petal.Length', group_attr='Species')

# summarise all columns grouped by 'Species'
attribute_summary(iris, group_attr='Species')
}
