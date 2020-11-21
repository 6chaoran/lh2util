#' @importFrom logging logReset basicConfig addHandler
#' @importFrom logging writeToFile writeToConsole
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom tidyr unite
NULL

#' initialize logger function
#'
#' initiator for logger functions
#'
#' @param logger name of logger
#' @param level The logging level of the root logger.
#' @param log.file external path of log file
#' @param reset.log boolean. TRUE -> reset logger
#' @examples
#' util.init.logger()
#' logging::loginfo('this is an info')
#' logging::logwarn('this is a warning')
#' logging::logerror('this is an error')
#' @export
#'
util.init.logger <- function(logger = "", 
                             level = 'INFO',
                             log.file = NULL, 
                             reset.log = TRUE){

  logReset()

  basicConfig(level = level)
  if(!is.null(log.file)) {
    if(reset.log && file.exists(log.file)) file.remove(log.file)
    addHandler(writeToFile, file=log.file, logger = logger)
  } else {
    addHandler(writeToConsole, logger = logger)
  }

}

#' initialize directories
#'
#' create a directory/file if it doesn't exist. 
#' directory is required to end with "/" (e.g. ./temp/)
#' when filename is provided, the parent directory will be created.
#' (e.g. ./tmp/ will be created is ./tmp/test.csv is provided) 
#' 
#' @param path directory/file to be created 
#' @examples
#' util.init.dir('./tmp1/')
#' util.init.dir('./tmp2/text.txt')
#' unlink('tmp1', recursive = TRUE)
#' unlink('tmp2', recursive = TRUE)
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

#' tabulate the file summary from `ls` command
#'
#' output the file summary from bash command `ls`
#' @param dir directory to be summarized
#' @return data.frame
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

#' default color code
#' 
#' default color code
#' 
#' @importFrom dplyr case_when
#' @param code either int or string. \cr
#' 1,'red \cr
#' 2,'orange' \cr
#' 3,'yellow' \cr
#' 4,'lightgreen' \cr
#' 5,'green' \cr
#' anything else, default color \cr
#' @return color code
#' @export
#' @examples 
#' util.lh2.fill()
util.lh2.fill <- function(code = NA) {
  case_when(
    is.na(code) ~ '#008B8B',
    code == 'red' | code == 1 ~ '#cc3232',
    code == 'orange' | code == 2 ~ '#db7b2b',
    code == 'yellow' | code == 3 ~ '#e7b416',
    code == 'lightgreen' | code == 4 ~ '#99c140',
    code == 'green' | code == 5 ~ '#2dc937',
    TRUE ~ '#008B8B'
  )
}
