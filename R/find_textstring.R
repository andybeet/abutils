#' Find snippets of text human readable files
#'
#' Scans all files in specified folder for \code{textSnippet}
#'
#'@param textSnippet Character string. A snippet of text
#'@param folder Character string. Folder to search. (Full path required)
#'@param fileType Character String. File type extension. eg ("R","Rmd","txt")
#'
#' @return Character vector. Names of the files that contain the text
#'
#' @examples
#' \dontrun{
#' #search all R files in the project root for the string "parameter"
#' find_text_string("parameter", here::here(),"R")
#'
#' # search all Rmd files in the vignettes folder for the string "help"
#' find_text_string("help", here::here("vignettes"),"Rmd")
#'
#' }
#'
#' @export

find_textstring <- function(textSnippet,folder,fileType) {

  # find files
  filenames <- list.files(folder,pattern=paste0("\\.",fileType))

  options(warn=-1)
  foundFiles <- vector(mode="character",length=0L)

  # loop over each file
  for (afile in filenames) {
    # read in file content
    fileContent <- readLines(paste0(folder,"/",afile))

    lineFound <- fileContent[grepl(textSnippet,fileContent)]
    if (length(lineFound) == 0) next

    foundFiles = c(foundFiles,afile)
  }
  return(foundFiles)

}
