#' Convert Chinese strings to four corner code.
#'
#' @param Chin.str The string need to be converted
#' @param sep Character used to seperate different characters
#' @param parallel Whether or not use parallel calculation
#' @return four corner code of \code{Chin.str}.
#' @examples
#' ChStr2fc(c("海上生明月","天涯共此时"))



ChStr2fc <- function(Chin.strs = "", sep = "_", parallel = FALSE)
{
  # Convert one string to four corner code
  ChStr2fc <- function(Chin.str, FClib){
    Sys.setlocale(category = 'LC_ALL', locale = 'chs')
    if(is.na(Chin.str)) return(NA)
    Chin.char <- unlist(strsplit(Chin.str, split = "")) # divide the string to characters

    # convert a single character to pinyin
    ChChar2fc <- function(Chin.char){
      ChCharfc <- FClib[[Chin.char]]
      if(length(ChCharfc) == 0) ChCharfc = Chin.char
      return(ChCharfc)
    }

    paste(sapply(Chin.char, ChChar2fc), collapse = sep)
  }

  # Use parallel computing to convert strings if parallel is TRUE
  if(parallel)
  {
    no_cores <- parallel::detectCores() - 1  # Get the number of available string
    cl <- parallel::makeCluster(no_cores)   # Initiate cluster
    fccode <- parallel::parSapply(cl, X = Chin.strs, FUN = ChStr2fc, FClib)
    parallel::stopCluster(cl)
    return(fccode)
  } else {
    sapply(Chin.strs, ChStr2fc, FClib)
  }
}
