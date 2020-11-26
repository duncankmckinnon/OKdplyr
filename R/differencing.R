
getDifferences <- function(xts, is.seasonal = FALSE, maxdiffs = 2){
  ns <- 0
  nd <- 0
  fq <- frequency(xts)
  x <- xts
  if( is.seasonal ){
    ns <- forecast::nsdiffs(x, max.D = 1)
    if( ns > 0 ){
      x <- diff(x, lag = fq, differences = ns)
    }
  }

  nd <- forecast::ndiffs(x, max.d = maxdiffs)
  return( list('ns' = ns, 'nd' = nd, 'fq' = fq) )
}


autoDifference <- function( xts, is.seasonal = FALSE, maxdiffs = 2 ){
  original <- NULL
  ns <- 0
  nd <- 0
  seasonal <- NULL
  final <- xts
  f <- frequency(xts)
  ns <- ifelse( is.seasonal == TRUE, forecast::nsdiffs(xts, max.D = 1), 0 )
  if( ns > 0 ){
    final <- diff( xts, lag = f, differences = ns)
    seasonal <- xts[1:(f*ns)]
    nd <- forecast::ndiffs(seasonal, max.d = maxdiffs)
    if( nd > 0 ){
      sequential <- final[1:nd]
      final <- diff( final, differences = nd)
    }
  } else {
    nd <- forecast::ndiffs( xts, max.d = maxdiffs )
    if( nd > 0 ) {
      final <- diff( xts, differences = nd)
      sequential <- xts[1:nd]
    }
  }
  return(
    list(
      'nd' = nd,
      'ns' = ns,
      'sequential' = sequential,
      'seasonal' = seasonal,
      'final' = final
    )
  )
}

autoInversedifference <- function( final, ns, nd, seasonal = NULL, original = NULL){

}
