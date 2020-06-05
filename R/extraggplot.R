

gghistogram <- function( data, summary_attr = NULL, group_attr = NULL, bins = 30, binwidth = NULL ){
  # assertions run while getting summary
  summary_data <- attribute.summary( data, summary_attr, group_attr )

  p <- ggplot2::ggplot( data, mapping = aes_string( x = summary_attr ) ) + ggplot2::theme_bw()

  if( is.comparable( data[ summary_attr ] ) )

  if( is.null( group_attr ) ) {
    brks <- c(summary_data$min, summary_data$p25, summary_data$median, summary_data$p75, summary_data$max)
    labs <- paste(c('Min','25%','Med.','75%','Max'), brks, sep = ': ')
    p <- p + geom_histogram( bins = bins, binwidth = binwidth ) +
      geom_vline(xintercept = summary_data['mean'], show.legend = F) +
      scale_x_continuous(breaks = brks, labels = labs) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    brks <- sapply( summary_data )
    p <- p + geom_histogram(aes_string(fill = group_attr), bins = bins, binwidth = binwidth )
  }

}
