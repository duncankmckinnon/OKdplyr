### Assertions ###

#' Assertions used to change data entries and attributes
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

#' check if elements of entry are all unique
are.unique <- function( v ) length( v ) == length( unique( v ) )
assertthat::on_failure(are.unique) <- function(call, env) paste0(deparse(call$v), ' are not unique')

### checks for super-groups of classes ###

bidirected.type <- c( 'integer', 'numeric', 'complex', 'double')
is.bidirected <- function( x ) class( x ) %in% bidirected.type

temporal.type <- c('Date', 'POSIXct', 'POSIXt', 'POSIXlt')
is.temporal <- function( x ) class( x ) %in% temporal.type

comparable.type <- c( bidirected.type, temporal.type )
is.comparable <- function( x ) class( x ) %in% comparable.type

categorical.type <- c( 'character', 'factor', 'logical' )
is.categorical <- function( x ) class( x ) %in% categorical.type

