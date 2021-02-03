#' Find packages in R files
#'
#' Scans all R files in specified folder for packages of the form pkg::func()
#'
#'@param folder Character string. Folder to search. (Full path required)
#'
#' @return Character vector. Names of the packages found
#'
#' @examples
#' \dontrun{
#' #search all R files in the project root for packages used
#' find_packages(here::here("R"))
#'
#' }
#'
#' @export

find_packages <- function(folder) {

  # find files
  fnr <- list.files(folder,pattern=paste0("\\.r$"))
  fnR <- list.files(folder,pattern=paste0("\\.R$"))
  fnrmd <- list.files(folder,pattern=paste0("\\.rmd$"))
  fnRmd <- list.files(folder,pattern=paste0("\\.Rmd$"))
  filenames <- c(fnr,fnR,fnrmd,fnRmd)
  message(paste0(c("Files to search = ",filenames),collapse = ", "))


  options(warn=-1)
  foundPackages <- vector(mode="character",length=0L)

  # loop over each file
  for (afile in filenames) {
    # read in file content
    fileContent <- readLines(paste0(folder,"/",afile))

    lineFound <- fileContent[grepl("::",fileContent)]
    if (length(lineFound) == 0) next


    parts <- strsplit(lineFound,"::")

    ps <- NULL
    for (ip in 1:length(parts)) { # for each found line
      for (ap in head(parts[[ip]],-1)) {# for each split part of line
        p <- stringr::str_match(ap,"[a-zA-Z\\.0-9]+$")
        ps <- unique(c(ps,p))
      }
    }
    foundPackages = unique(c(foundPackages,ps))

  }
  return(sort(foundPackages))

}



