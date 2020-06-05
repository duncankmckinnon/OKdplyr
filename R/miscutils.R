

.assertions <- function(data, attributes = NULL, requires_attributes = F, isRun = T){
  if( isRun ){
    assertthat::assert_that( !missing(data) )
    assertthat::assert_that( is.data.frame(data) )
    has_attributes <- any( !is.null( attributes ) )
    if( has_attributes || requires_attributes ) {
      assertthat::assert_that( has_attributes )
      assertthat::assert_that( are.unique( attributes ) )
      if( has_attributes ) assertthat::assert_that(all( is.character( attributes ) ) && all( attributes %in% names(data) ))
    }
  }
}

are.unique <- function( v ) length( v ) == length( unique( v ) )
on_failure(are.unique) <- function(call, env) paste0(deparse(call$v), ' are not unique')

comparable.type <- c( 'integer', 'numeric', 'complex', 'double' )
is.comparable <- function( x ) class( x ) %in% comparable.type

categorical.type <- c( 'character', 'factor', 'logical' )
is.categorical <- function( x ) class( x ) %in% categorical.type

