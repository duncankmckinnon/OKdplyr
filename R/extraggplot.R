

gghistogram <- function( data, summary_attr = NULL, group_attr = NULL, bins = 30, binwidth = NULL, fill_color = 'blue', line_color = 'red'){

  p <- ggplot2::ggplot( data, mapping = ggplot2::aes_string( x = summary_attr ) ) + ggplot2::theme_bw()
  if( is.null( group_attr ) ) {
    return( .histogram.summary( p, summary_attr, bins, binwidth, fill_color, line_color ) )
  } else {
    return( .histogram.group( p, summary_attr, group_attr, bins, binwidth, fill_color, line_color ) )
  }
}

.histogram.summary <- function( p, summary_attr, bins, binwidth, fill_color, line_color ){
  # assertions run while getting summary
  summary_data <- attribute.summary( data, summary_attr, group_attr )[[summary_attr]]
  smmryattr <- dplyr::pull( data, summary_attr )
  if( is.comparable( smmryattr ) ) {
    brks <- c(summary_data$min, summary_data$p25, summary_data$median, summary_data$p75, summary_data$max)
    labs <- paste(c('Min','25%','Med','75%','Max'), brks, sep = ': ')
    p <- p + ggplot2::geom_histogram( fill = fill_color, bins = bins, binwidth = binwidth,
                                      stat = "bin", show.legend = F) +
      ggplot2::geom_vline(aes(xintercept = summary_data$mean), color = line_color, show.legend = T) +
      ggplot2::scale_x_continuous(breaks = brks, labels = labs) +
      ggplot2::labs(title = paste0('Histogram of ', summary_attr),
                    x = paste(summary_attr, ' Distribution Statistics'),
                    y = 'Count', vline = 'mean') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else if( is.categorical( smmryattr ) ) {
    brks <- dplyr::pull( summary_data, summary_attr )
    labs <- brks
    p <- p + ggplot2::geom_histogram( mapping = aes_string(fill = summary_attr),
                                      stat = "count", show.legend = F) +
      ggplot2::geom_hline(aes(yintercept = mean(pull( summary_data, 'n' ))), color = line_color, show.legend = T) +
      ggplot2::scale_x_discrete(breaks = brks, labels = labs) +
      ggplot2::labs(title = paste0('Histogram of ', summary_attr),
                    x = paste(summary_attr, ' Levels'),
                    y = 'Count', hline = 'mean') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  return( p )
}

.histogram.group <- function( p, summary_attr, group_attr, bins, binwidth, fill_color, line_color ){
  p <- p + ggplot2::geom_histogram(ggplot2::aes_string(fill = group_attr), bins = bins, binwidth = binwidth )
  return( p )
}


