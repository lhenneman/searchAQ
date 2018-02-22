#' return column with colname containing string
#'
#' \code{colCall} matches column names in raw SEARCH datasets that contain a string and returns column as vector
#'
#' @param df dataframe for matching
#' @param name string for matching in column names of \code{df}
#' @return This function returns a vector from the datafram

colCall <- function( df,
                     name) {
  name.col <- grep( paste( '^(Average )*',
                           name,
                           '(\\[.*\\])*$',
                           sep = ''),
                    colnames( df))
  if ( length( name.col) == 0 ) {
    out <- rep( NA, dim( df)[1])
  } else {
    out <- df[, name.col]
    }
  out
}
