#' Support function for Pinyin with more than one pronunciation. Expects format to be "...[py1,py2]..."
#'
#' @param s A string of pinyin-converted Chinese characters
#' @return A vector of all possible pronunciations for the full string \code{s}.
#' @examples
#' homonym('[lei1,luo4]ta2')

 homonym <- function(s){
      if(!grepl('\\[',s)) return(s)
      s. = unlist(strsplit(s,'\\[|\\]'))
      s. = s.[nchar(s.)>0]
      
      hinds = grep(',',s.)
      ambs = s.[hinds]
      dinds = (1:length(s.))[-hinds]
      ncomb = 2^length(hinds)
      
      stabs = matrix(rep(s.,ncomb),nrow=ncomb,byrow = T) 
      
      for(i in 1:length(hinds)){
        ind = hinds[i]
        s. = unlist(strsplit(ambs[i],','))
        stabs[,ind] = rep(rep(s., each = ncomb/2^(i)),2^(i-1))
      }
      
      apply(stabs,1,paste0,collapse='')
    }
