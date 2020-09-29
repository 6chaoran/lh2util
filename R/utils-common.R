#' @importFrom logging logReset basicConfig addHandler
#' @importFrom logging writeToFile writeToConsole
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom tidyr unite
NULL

#' util.init.logger
#'
#' initiator for logger functions
#'
#' @examples
#' util.init.logger()
#' @export
#'
util.init.logger <- function(logger = "", level = 'INFO',
                             log.file = NULL, reset.log = TRUE){

  logReset()

  basicConfig(level = level)
  if(!is.null(log.file)) {
    if(reset.log && file.exists(log.file)) file.remove(log.file)
    addHandler(writeToFile, file=log.file, logger = logger)
  } else {
    addHandler(writeToConsole, logger = logger)
  }

}

#' util.init.dir
#'
#' create a directory/file if it doesn't exist
#'
#' @examples
#' util.init.dir('./tmp1/')
#' util.init.dir('./tmp2/text.txt')
#' unlink('tmp1', recursive = T)
#' unlink('tmp2', recursive = T)
#' @export
util.init.dir <- function(path){

  if(grepl('/$', path)){
    # trailing "/" => directory
  } else {
    # else => file
    # init the parent directory
    path <- dirname(path)
  }

  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE)
    message(glue('[{path}] is created'))
  }
}

#' util.file.summ
#'
#' output the file summary from bash command `ls`
#'
#' @export
#' @examples
#' util.file.summ('./')
#'
util.file.summ <- function(dir){
  res <- system(glue('ls -lh {dir}'), intern = T)
  res <- res[2:length(res)] %>%
    str_split(' +', simplify = T)

  file.summ <- data.frame(
    Filename = res[,9],
    Filesize = res[,5],
    a = res[,6],
    b = res[,7]
  ) %>%
    unite("Received Date", a:b, sep = '/')

  file.summ
}

#' util.lh2.fill
#' 
#' default color code
#' 
#' @return color code
#' @export
#' @examples 
#' util.lh2.fill()
util.lh2.fill <- function(){
  '#008B8B'
}
