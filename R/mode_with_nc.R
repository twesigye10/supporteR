#' Calculate mode with No Consensus for ties
#'
#' @param x Specify a vector whose mode is to be calculated
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
mode_with_nc <- function(x, na.rm = FALSE) {

  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  # extract unique values
  val <- unique(x)
  # extract frequencies for values
  data_freqs <- tabulate(match(x, val))
  # max freq
  max_data_val <- max(data_freqs)
  # if ties, no consensus
  if (length(data_freqs[data_freqs >= max_data_val]) > 1) {
    return("NC")
  } else{
    return(val[which.max(data_freqs)])
  }

}
