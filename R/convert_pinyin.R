#' Convert Chinese strings to pinyin.
#'
#' @param Chin.str The string need to be converted
#' @param method Whether the output should be toned or toneless.
#' @param multi Whether the output should contain more than one pinyin
#' @param sep Character used to seperate different characters
#' @param parallel Whether or not use parallel calculation
#' @return pinyin of \code{Chin.str}.
#' @examples
#' ChStr2py(c("海上生明月","天涯共此时"))

ChStr2py <- function(Chin.strs = "", method = c("toneless", "tone"), multi = FALSE, sep = "_", parallel = FALSE)
{
  method <- match.arg(method)

  # Convert a string to pinyin
  ChStr2py <- function(Chin.str, pylib){
    Chin.char <- unlist(strsplit(Chin.str, split = "")) # divide the string to characters

    # convert a single character to pinyin
    ChChar2Py <- function(Chin.char){
      ChCharpy <- pylib[[Chin.char]]
      ChCharpy <- switch(method, tone = ChCharpy, toneless = gsub("[1-4]","", ChCharpy))
      if(multi){
        ChCharpy <- ifelse(grepl(",", ChCharpy), paste0("[", ChCharpy, "]"),  ChCharpy)
      }
      return(ChCharpy)
    }

    paste(sapply(Chin.char, ChChar2Py), collapse = sep)
  }

  # Use parallel computing to convert strings if parallel is TRUE
  if(parallel)
  {
    no_cores <- parallel::detectCores() - 1  # Get the number of available string
    cl <- parallel::makeCluster(no_cores)   # Initiate cluster
    pinyin <- parallel::parSapply(cl, X = Chin.strs, FUN = ChStr2py, pylib)
    parallel::stopCluster(cl)
    return(pinyin)
  } else {
    sapply(Chin.strs, ChStr2py, pylib)
  }

}



