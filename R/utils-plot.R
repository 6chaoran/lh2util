#' @importFrom ggplot2 theme element_rect element_text element_line element_blank
#' @importFrom ggplot2 geom_bar geom_text 
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 labs scale_y_continuous
#' @importFrom rlang sym
#' @importFrom dplyr mutate group_by ungroup summarise lag n
NULL

#' util.lh2.theme
#' 
#' default theme
#' 
#' @param angle int, rotation of x.axis.text
#' @param legend.position char, {'right','left','top','bottom','none'}
#' @param font.size int font size of text
#' @return a ggplot2 theme
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + 
#'   geom_point() + 
#'   util.lh2.theme()
util.lh2.theme <- function(angle = 45, legend.position = 'right',
                           font.size = 12){
  theme(
    panel.background = element_rect(fill = "white", colour = "grey",size = 1, linetype = "solid"),
    plot.title = element_text(color = 'black', size = font.size + 2, face = 'bold'),
    axis.title.x = element_text(color = 'black', size = font.size + 1, face = 'bold'),
    axis.title.y = element_text(color = 'black', size = font.size + 1, face = 'bold'),
    axis.text.x = element_text(size = font.size, angle = angle, hjust = 1),
    axis.text.y = element_text(size = font.size),
    panel.grid.minor = element_line(size = (0.2), colour="grey"),
    panel.grid.major = element_line(size = (0.2), colour="grey"),
    legend.position = legend.position)
  
}

#' util.theme.no.axis
#' 
#' quick function to remove the x/y axis
#' @param axis char, {'x','y'}
#' @return ggplot theme
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + 
#'   geom_point() + 
#'   util.lh2.theme() +
#'   util.theme.no.axis('x')
util.theme.no.axis <- function(axis = 'x'){
  if (axis == 'x'){
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())
  }
  
  if (axis == 'y'){
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
  }
}

#' util.plot.stack.bar
#' 
#' plot stack bar with proprtion percentage for two categorical varaibles
#' 
#' @param df data.frame, input data
#' @param x char, variable to plot for statistics
#' @param fill char, variable to partition by color
#' @param label.threshold float, only label percentage greater than threshold
#' @param ... extra param for function `geom_bar`
#' @export
#' @examples 
#' mtcars['am'] <- as.factor(mtcars[['am']])
#' mtcars['gear'] <- as.factor(mtcars[['gear']])
#' util.plot.stack.bar(mtcars, 'gear', 'am')
util.plot.stack.bar <- function(df, x, fill,
                                label.threshold = 0.1,
                                ...){
  
  df %>%
    mutate(x = !!rlang::sym(x),
           fill = !!rlang::sym(fill)) %>%
    group_by(x, fill) %>%
    summarise(n = n()) %>%
    group_by(x) %>%
    mutate(pct = n / sum(n),
           start = cumsum(pct),
           end = cumsum(lag(pct, 1, 0)),
           position = 1 - (start + end) /2 ,
           label = ifelse(pct > label.threshold,
                          scales::percent(pct, 1), '')) %>%
    ungroup() %>%
    ggplot(aes(x = x, y = pct, fill = fill, label = label)) +
    geom_bar(stat = 'identity', ...) +
    geom_text(aes(y = position)) +
    labs(x = x, fill = fill, y = '') +
    scale_y_continuous(labels = scales::percent)
}
