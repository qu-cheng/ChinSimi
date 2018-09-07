#' Convert Chinese strings to wubi code (based on radicals).
#'
#' @param Chin.str The string need to be converted
#' @param sep Character used to seperate different characters
#' @param parallel Whether or not use parallel calculation
#' @return wubi code of \code{Chin.str}.
#' @examples
#' ChStr2wb(c("海上生明月","天涯共此时"))



ChStr2wb <- function(Chin.strs = "", sep = "_", parallel = FALSE)
{
  # Convert one string to wubi code
  ChStr2wb <- function(Chin.str, WBlib){
    Sys.setlocale(category = 'LC_ALL', locale = 'chs')
    Chin.char <- unlist(strsplit(Chin.str, split = "")) # divide the string to characters

    # convert a single character to wubi code
    ChChar2wb <- function(Chin.char){
      ChCharwb <- WBlib[[Chin.char]]
      return(ChCharwb)
    }

    paste(sapply(Chin.char, ChChar2wb), collapse = sep)
  }

  # Use parallel computing to convert strings if parallel is TRUE
  if(parallel)
  {
    no_cores <- parallel::detectCores() - 1  # Get the number of available string
    cl <- parallel::makeCluster(no_cores)   # Initiate cluster
    wbcode <- parallel::parSapply(cl, X = Chin.strs, FUN = ChStr2wb, WBlib)
    parallel::stopCluster(cl)
    return(wbcode)
  } else {
    sapply(Chin.strs, ChStr2wb, WBlib)
  }
}
