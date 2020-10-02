#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec
#' @importFrom scales percent
#' @importFrom DT JS datatable
#' @importFrom data.table %between%
#' @importFrom dplyr mutate_if
NULL

#' util.text.wrap
#' 
#' add line break in text
#' 
#' @param x input text
#' @param wrap.len the max of string length before adding line break
#' @param br line break separator
#' @return character
#' @export
#' @examples 
#' x <- paste(rep('a',20), collapse = '')
#' util.text.wrap(x, wrap.len = 5)
util.text.wrap <- function(x, wrap.len = 50, br = '<br>'){

  x <- as.character(x)
  if(is.na(x)) return(x)
  if(wrap.len >= nchar(x)) {return(x)}
  res <- c()
  for(i in seq(ceiling(nchar(x)/wrap.len))){
    res <- c(res, substr(x, 1, wrap.len))
    x <- substr(x, wrap.len+1, nchar(x))
  }
  paste(res, collapse = br)
}

#' util.vis.kable
#' 
#' visualize table using kable package
#' 
#' @param df input data.frame
#' @param convert_pct boolean, TRUE -> convert number to percent if possible
#' @param accuracy decimal for percentage, e.g. 1/0.1/0.01
#' @param full_width boolean, TRUE -> full width table
#' @param escape boolean, TRUE escape html tags
#' @param caption character, caption of the table
#' @param align character, a combined string of text alignment. e.g. 'lcrr' for 4 columns
#' @param align.left int, number of columns left aligned.
#' @param digits int, number of decimal digits to keep
#' @param position {'left','right','float_left','center'}
#' @param pretty.header boolean, TRUE -> colored header
#' @param ... extra params from kable_styling function
#' @return rendered html table
#' @export
#' @examples 
#' util.vis.kable(head(mtcars))
util.vis.kable <- function(df, convert_pct = F,
                           accuracy = 1,
                           full_width = F,
                           escape = F,
                           caption = NULL,
                           align = NULL,
                           align.left = 1L,
                           digits = 1,
                           position = 'center',
                           pretty.header = T,
                           ...){

  is.percentage <- function(i){
    i <- i[!is.na(i)]
    res <- FALSE
    if(length(i) >0){
      if(is.numeric(i)){
        if(all(i %between% c(0,1))){
          res <- TRUE
        }
      }
    }
    res
  }
  
  if (is.null(align)) {
    align <-
      paste(c(rep('l', align.left), rep('r', ncol(df) - align.left)), 
            collapse = '')
  }

  if (convert_pct) {
    df <- df %>%
      mutate_if(is.percentage, function(i) {
        scales::percent(i, accuracy = accuracy)
      })
  }

  out <- df %>%
    kable('html',
        format.args = list(big.mark=','),
        escape = escape,
        align = align,
        caption = caption,
        digits = digits,
        ) %>%
    kable_styling(bootstrap_options = c('striped','hover'),
                  full_width  = full_width,
                  position = position,
                  ...)
  if(pretty.header){
    out <- out %>%
      row_spec(0, bold = T, color = 'white', background = util.lh2.fill())
  }
  out
}

#' util.vis.datatable
#' 
#' visualize table using kable package
#' 
#' @param df input data.frame
#' @param type character, {'buttons','scroll-y'}
#' @param dom character, a combined string to indicate table elements
#' @param options list, extra options from `DT` js package
#' @param scrollY int, number of pixels to the scrollable window
#' @param rownames boolean, TRUE -> show rowname of table
#' @param filter character, {'none','top','bottom'}, position of filter
#' @param background.color character, header color
#' @param pretty.header boolean, TRUE -> enable colored header
#' @param ... extra params from `datatable` function
#' @return rendered html table
#' @export
#' @examples 
#' util.vis.datatable(head(mtcars), type = 'buttons')
#' util.vis.datatable(head(mtcars), type = 'scroll-y')

util.vis.datatable <- function(df, type = c(),
                               dom = 'frtip',
                               options = NULL,
                               scrollY = 400,
                               rownames = F,
                               filter = 'none',
                               background.color ='#008B8B',
                               pretty.header = F,
                               ...){

  color <- '#fff'
  if(filter != 'none') color <- '#000000'

  js <- JS(
    "function(settings, json) {",
    paste0("$(this.api().table().header()).css({'background-color': '",
           background.color,"', 'color': '", color ,"'});"),
    "}")

  extensions <- NULL

  if(is.null(options)){
    options <- list()
  }

  if(pretty.header) options[['initComplete']] <- js

  options[['dom']] <- dom

  if('buttons' %in% type){

    extensions <- c(extensions, 'Buttons')
    options[['dom']] <- paste0('B', options[['dom']])
    options[['buttons']] <- c('copy', 'csv', 'excel', 'print')
  }

  if('scroll-y' %in% type){
    options[['scroller']] <- TRUE
    options[['scrollY']] <- scrollY
    options[['scrollCollapse']] <- TRUE
    options[['scrollX']] <- TRUE
    options[['paging']] <- FALSE
  }

  df %>%
    datatable(rownames = rownames,
              extensions = ifelse(is.null(extensions), list(), extensions),
              options = options,
              filter = filter,
              ...)

}
