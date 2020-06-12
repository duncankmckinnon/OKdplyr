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
      if( has_attributes ) are.attributes( attributes, data )
    }
  }
}

#' check that non-missing attribute argument is character and is a valid index into the data
is.attribute <- function( attr, nms ) ifelse( is.null( attr ), TRUE, ( is.character( attr ) && ( attr %in% nms ) ) )
assertthat::on_failure(is.attribute) <- function(call, env) paste0(deparse(call$attr), ' is not a valid attribute')

#' check that all non-missing attributes are character and are valid inexs into the data
are.attributes <- function( attrs, data ) sapply( attrs, function( attr, nmd ) assertthat::assert_that( is.attribute( attr, nmd ) ), names(data) )

#' check that all attributes are greater than zero
are.greaterthanzero <- function( attrs ) all( attrs > 0 )
assertthat::on_failure(are.greaterthanzero) <- function(call, env) paste0('all ', deparse(call$attrs), ' are not greater than zero')

#' check that there are attribute arguments
has.attributes <- function( attrs ) any( !is.null( attrs ) )
assertthat::on_failure(has.attributes) <- function(call, env) 'attributes required but are all null'


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
