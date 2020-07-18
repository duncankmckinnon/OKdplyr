
#' Generate quick informative summary histograms
#' @description Special format for histograms showing the summary statistics on the x axis, and the mean as a line
#' For numerical data, generates vertical mean lines for each group.  For categorical data, generates a horizontal line
#' representing the mean count.
#' @importFrom ggplot2 ggplot geom_histogram geom_vline geom_hline aes aes_string theme theme_bw element_text scale_x_continuous scale_x_discrete labs
#' @importFrom dplyr pull
#' @param data the dataset, a data.frame or tibble object
#' @param summary_attr the attribute to show (x - variable)
#' @param group_attr the grouping attribute (fill - variable)
#' @param ... arguments passed to geom_histogram, (e.g. bins, binwidth)
#' @param fill_color the default fill
#' @param line_color the default line color
#' @return a ggplot object to display
#' @export
gghistogram <- function( data, summary_attr = NULL, group_attr = NULL, ..., fill_color = 'blue', line_color = 'red'){
  # assertions for grouped data
  .assertions(data, c(summary_attr, group_attr), requires_attributes = T, isRun = T)

  # initial ggplot
  p <- ggplot( data, mapping = aes_string( x = summary_attr ) ) + theme_bw()
  if( is.null( group_attr ) ) {
    return( .histogram.summary(p, data, summary_attr, ..., fill_color, line_color ) )
  } else {
    return( .histogram.group(p, data, summary_attr, group_attr, ..., fill_color, line_color ) )
  }
}

.histogram.summary <- function( p, data, summary_attr, ..., fill_color, line_color ){
  summary_data <- attribute.summary( data, summary_attr, .checkAssertions = F )[[summary_attr]]
  smmryattr <- pull( data, summary_attr )
  if( is.comparable( smmryattr ) ) {
    brks <- c(summary_data$min, summary_data$p25, summary_data$median, summary_data$p75, summary_data$max)
    labs <- paste(c('Min','25%','Med','75%','Max'), brks, sep = ': ')
    return( p + geom_histogram( fill = fill_color, ...,
                                stat = "bin", show.legend = F) +
      geom_vline(aes(xintercept = summary_data$mean), color = line_color, show.legend = T) +
      scale_x_continuous(breaks = brks, labels = labs) +
      labs(title = paste0('Histogram of ', summary_attr),
                    x = paste(summary_attr, ' Distribution Statistics'),
                    y = 'Count', vline = 'mean') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) )
  } else if( is.categorical( smmryattr ) ) {
    brks <- pull( summary_data, summary_attr )
    labs <- brks
    return( p + geom_histogram( mapping = aes_string(fill = summary_attr), ...,
                                      stat = "count", show.legend = F) +
      geom_hline(aes(yintercept = mean(pull( summary_data, 'n' ))), color = line_color, show.legend = T) +
      scale_x_discrete(breaks = brks, labels = labs) +
      labs(title = paste0('Histogram of ', summary_attr),
                    x = paste(summary_attr, ' Levels'),
                    y = 'Count', hline = 'mean') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) )
  }
  return( geom_histogram() )
}

### TODO: expand on grouped logic
.histogram.group <- function( p, data, summary_attr, group_attr, ..., fill_color, line_color ){
  p <- p + geom_histogram(aes_string(fill = group_attr), ... )
  return( p )
}


