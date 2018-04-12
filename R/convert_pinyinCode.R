#' Convert Chinese strings to pinyin.
#'
#' @param Chin.str The string need to be converted
#' @param method Whether the output should be toned or toneless.
#' @param multi Whether the output should contain more than one pinyin
#' @param sep Character used to seperate different characters
#' @param parallel Whether or not use parallel calculation
#' @return pinyin code of \code{Chin.str} according to the lookup table in [基于音形码的中文字符串相似度算法](https://blog.csdn.net/chndata/article/details/41114771). The code is organized as Consonant + Main vowel + Connection Vowel + Tone. For example, these parts for mian4 is m, an, i, and 4, respectively.
#' @examples
#' ChStr2pyc(c("海上生明月","天涯共此时"))

ChStr2pyc <- function(Chin.strs = "", method = c("toneless", "tone"), multi = FALSE, sep = "_", parallel = FALSE)
{
  method <- match.arg(method)

  # Convert a string to pinyin
  ChStr2py <- function(Chin.str, pylibnew){
    Chin.char <- unlist(strsplit(Chin.str, split = "")) # divide the string to characters

    # convert a single character to pinyin
    ChChar2Py <- function(Chin.char){
      ChCharpy <- pylibnew[[Chin.char]]
      ChCharpy <- switch(method, tone = ChCharpy, toneless = substr(ChCharpy,1,3))
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
    pinyin <- parallel::parSapply(cl, X = Chin.strs, FUN = ChStr2py, pylibnew)
    parallel::stopCluster(cl)
    return(pinyin)
  } else {
    sapply(Chin.strs, ChStr2py, pylibnew)
  }

}



