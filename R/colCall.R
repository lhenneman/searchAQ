#function to return column with colname containing string
colCall <- function( df,
                     name) {
  name.col <- grep( paste( '^(Average )*', name, '(\\[.*\\])*$', sep = ''),
                    colnames( df))
  if ( length( name.col) == 0 ) {
    out <- rep( NA, dim( df)[1])
  } else {
    out <- df[, name.col]
    }
  out
}
