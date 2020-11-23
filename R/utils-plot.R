#' @importFrom ggplot2 theme element_rect element_text element_line element_blank
#' @importFrom ggplot2 geom_bar geom_text 
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 labs scale_y_continuous
#' @importFrom rlang sym
#' @importFrom dplyr mutate group_by ungroup summarise lag n
NULL

#' The default theme of plots
#' 
#' default ggplot2 theme
#' 
#' @param angle int, rotation of x.axis.text
#' @param legend.position char, {'right','left','top','bottom','none'}
#' @param font.size int font size of text
#' @param skip.axis char, {'x','y'}, skip style setting for x,y axis
#' @param only.axis char, {'x','y'}, only do style setting for x,y axis
#' @return a ggplot2 theme
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + 
#'   geom_point() + 
#'   util.lh2.theme()
util.lh2.theme <- function(angle = 45,
                           legend.position = 'right',
                           font.size = 12,
                           skip.axis = '',
                           only.axis = '') {
  
  panel.background <- element_rect(
    fill = "white",
    colour = "grey",
    size = 1,
    linetype = "solid"
  )
  plot.title <- element_text(color = 'black',
                             size = font.size + 2,
                             face = 'bold')
  axis.title.y <- element_text(color = 'black',
                               size = font.size + 1,
                               face = 'bold')
  axis.text.y <- element_text(size = font.size)
  axis.title.x <-
    element_text(color = 'black',
                 size = font.size + 1,
                 face = 'bold')
  axis.text.x <-
    element_text(size = font.size,
                 angle = angle,
                 hjust = 1)
  panel.grid.minor <- element_line(size = (0.2), colour = "grey")
  panel.grid.major <- element_line(size = (0.2), colour = "grey")
  
  if (skip.axis == 'x') {
    theme(
      panel.background = panel.background,
      plot.title = plot.title,
      axis.title.y = axis.title.y,
      axis.text.y = axis.text.y,
      panel.grid.minor = panel.grid.minor,
      panel.grid.major = panel.grid.major,
      legend.position = legend.position
    )
  } else if (skip.axis == 'y') {
    theme(
      panel.background = panel.background,
      plot.title = plot.title,
      axis.title.x = axis.title.x,
      axis.text.x = axis.text.x,
      panel.grid.minor = panel.grid.minor,
      panel.grid.major = panel.grid.major,
      legend.position = legend.position
    )
  } else if (only.axis == 'x') {
    theme(
      axis.title.x = axis.title.x,
      axis.text.x = axis.text.x
    )
  } else if (only.axis == 'y') {
    theme(
      axis.title.y = axis.title.y,
      axis.text.y = axis.text.y
    )
  } else {
    theme(
      panel.background = panel.background,
      plot.title = plot.title,
      axis.title.x = axis.title.x,
      axis.title.y = axis.title.y,
      axis.text.x = axis.text.x,
      axis.text.y = axis.text.y,
      panel.grid.minor = panel.grid.minor,
      panel.grid.major = panel.grid.major,
      legend.position = legend.position
    )
  }
}

#' Add-on theme without x/y axis
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
util.theme.no.axis <- function(axis = 'x') {
  if (axis == 'x') {
    return(
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
      )
    )
  }
  
  if (axis == 'y') {
    return(
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
      )
    )
  }
}

#' visualize two categorical variables in stacked bars
#' 
#' plot stack bar with proportion percentage for two categorical variables
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

#' combine plots and share axis
#' 
#' modify plots by hiding x/y axes if shared x/y axis is used, plots combination
#' is done by \code{patchwork} package. \cr
#' plotlist can also be output by setting \code{return.plots = TRUE}.
#' 
#' @importFrom ggplot2 coord_flip theme
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @param ... ggplot objects
#' @param shared.axis {'x', 'y'},  defaults to 'y'
#' @param plots.theme ggplot theme, defaults to \code{util.lh2.theme}
#' @param plots.coord ggplot coord, defaults to \code{coord_flip}
#' @param title str, plot global title
#' @param subtitle str, plot global subtitle
#' @param caption str, plot global caption
#' @param nrow int, number of rows of the layout
#' @param ncol int, number of ncols of the layout
#' @param legend.position {'right','left','bottom','top'}
#' @param return.plots if TRUE -> return list of plots only, defaults to FALSE
#' @return ggplot
#' @export
#' @examples 
#' library(ggplot2)
#' library(dplyr)
#' library(patchwork)
#' 
#' p1 <- mtcars %>%
#'   mutate(gear = factor(gear), am = factor(am)) %>%
#'   util.plot.stack.bar('gear','am') +
#'   labs(title = 'p1')
#' p2 <- mtcars %>%
#'   mutate(am = factor(gear)) %>%
#'   group_by(am) %>%
#'   summarise(n = n()) %>%
#'   ggplot(aes(x = am, y = n)) +
#'   geom_bar(stat = 'identity', fill = util.lh2.fill('orange'))+
#'   labs(title = 'p2')
#'   
#' util.plots.share.axis(p1, p2, 
#'   title = 'Shared Axis Plot')
util.plots.share.axis <- function(..., 
                                  shared.axis = 'y', 
                                  plots.theme = NULL, 
                                  plots.coord = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  nrow = NULL,
                                  ncol = NULL,
                                  legend.position = 'bottom',
                                  return.plots = FALSE){
  
  if((!shared.axis %in% c('x','y')) & (is.null(nrow) | is.null(ncol))){
    stop('[shared.axis] should be "x" or "y", otherwise [nrow] & [ncol] are required.')
  }
  if((!is.null(nrow) & !is.null(ncol) & shared.axis %in% c('x','y'))){
    message(glue('[nrow], [ncol] overides [shared.axis] parameter.'))
  }
  
  plots <- list(...)
  len <- length(plots)
  
  if (is.null(plots.theme))
    plots.theme <- util.lh2.theme
  
  if (!is.null(nrow) & !is.null(ncol)) shared.axis <- ''
  
  if (shared.axis == 'y') {
    nrow <- 1
    ncol <- len
  }
  if (shared.axis == 'x') {
    nrow <- len
    ncol <- 1
  }
  
  # chart with axis
  axis.chart.index <- ifelse(shared.axis == 'y', 1, len)
  for (i in seq(len)) {
    row.id <- floor((i - 1) / ncol) + 1  
    col.id <- (i - 1) %% ncol + 1

    if (col.id == 1) {
      # left most column
      if (row.id == nrow) {
        plots[[i]] <- plots[[i]] + 
          plots.theme()
      } else {
        plots[[i]] <- plots[[i]] + 
          plots.theme() + 
          util.theme.no.axis(axis = 'x')
      }
    } else if (row.id == nrow) {
      # bottom row
      plots[[i]] <-
        plots[[i]] + plots.theme() + 
        util.theme.no.axis(axis = 'y')
    } else {
      plots[[i]] <-
        plots[[i]] + plots.theme() + 
        util.theme.no.axis(axis = 'x') + 
        util.theme.no.axis(axis = 'y')
    }
  }
  
  if(return.plots) return(plots)
  
  p <- (
    wrap_plots(plots, nrow = nrow, ncol = ncol) +
      plot_layout(guides = 'collect')
  )
  
  if(!is.null(plots.coord)){
    p <- p & plots.coord(expand = F)
  }
  
  p <- p + plot_annotation(title = title,
                           subtitle = subtitle,
                           caption = caption, 
                           theme = plots.theme()) &
    theme(legend.position = legend.position)
  
  return(p)
}


#' compute pairwise correlation with p-value
#' 
#' categorical variables are encoded to integers. (this is a big assumption)
#' Pearson's correlation is used.
#' 
#' @importFrom tidyselect one_of
#' @importFrom dplyr mutate_if bind_rows select pull arrange
#' @importFrom progress progress_bar
#' @importFrom stats cor.test
#' @param data input data.frame
#' @param fnames variables to perform correlation computation
#' @return data.frame
#' @export
#' @examples 
#' util.corr.compute(mtcars, c('am','gear','mpg'))
util.corr.compute <- function(data, fnames){
  # convert categorical to integer (big assumption)
  data <- data %>%
    select(one_of(fnames)) %>%
    mutate_if(is.character, function(i) as.integer(factor(i))) %>%
    mutate_if(is.factor, as.integer)

  # only implement Pearson test
  total <- length(fnames) * (length(fnames) - 1) / 2
  pb <- progress_bar$new(
    format = "  processing :what [:bar] :percent eta: :eta",
    clear = FALSE,
    width = 60,
    total = total)

  out <- list()
  for(i in fnames){
    for(j in fnames){
      if(i > j){
        pb$tick(tokens = list(what = glue("{i}-{j}   ")))
        cor.ij <- cor.test(data %>% pull(i),
                           data %>% pull(j),
                           use = 'complete.obs')
        row <- data.frame(var1 = i,
                          var2 = j,
                          corr = cor.ij$estimate,
                          p.value = cor.ij$p.value)
        out <- append(out, list(row))
      }

    }
  }

  out <- bind_rows(out) %>%
    mutate(corr = round(corr,2),
           p.value = round(p.value, 2))

  out.diag <- data.frame(
    var1 = fnames,
    var2 = fnames,
    corr = 1,
    p.value = 0
  )

  return(out %>% 
           bind_rows(out.diag) %>%
           arrange(var1, var2))

}

#' plot pairwise correlation
#' 
#' plot pairwise correlation, used together with `util.corr.compute`
#' 
#' @param out data.frame output from `util.corr.compute`
#' @param title title of the chart
#' @importFrom ggplot2 labs aes
#' @importFrom ggplot2 geom_text geom_tile scale_fill_gradient2
#' @return plot
#' @export
#' @examples 
#' out <- util.corr.compute(mtcars, colnames(mtcars))
#' util.corr.plot(out)
util.corr.plot <- function(out, title = 'Pairwise Correlation Plot'){
  out %>%
    mutate(p = ifelse(p.value < 0.05, '*', ''),
           label = glue('{corr}{p}'),
           label = ifelse(var1 == var2, '', label)) %>%
    ggplot(aes(x = var2, y = var1, fill = corr, label = label)) +
    geom_tile(color = 'red') +
    geom_text(size = 3) +
    scale_fill_gradient2(high = util.lh2.fill()) +
    labs(fill = 'correlation',
         title = title,
         subtitle = '* indicates correlation signifcance',
         x = '',
         y = '') +
    util.lh2.theme(font.size = 10)
}


#' density plot for single numeric variable
#' 
#' profile the numeric variable colored by segment, used for segment profiling
#' 
#' @param data input data.frame
#' @param x numeric variable for profiling
#' @param segment variable name of the segment
#' @param alpha float density plot transparency
#' @param scale float scale of density plot
#' @return list(data.frame, plot)
#' @export
#' @importFrom dplyr rename filter
#' @importFrom ggridges geom_density_ridges_gradient
#' @importFrom ggplot2 coord_cartesian
#' @importFrom forcats fct_rev
#' @examples 
#' res <- util.profile.num(mtcars, x = 'wt', segment = 'am')
#' res$data
#' res$plot
#' 
util.profile.num <- function(data, x, segment = 'segment',
                             alpha = 0.8, scale = 1){
  
  data <- data %>%
    rename(x = !!rlang::sym(x),
           segment = !!rlang::sym(segment))

  plot <- data %>%
    filter(!is.na(x)) %>%
    filter(x <= quantile(x, 0.99, na.rm = T)) %>%
    filter(x >= quantile(x, 0.01, na.rm = T)) %>%
    ggplot(aes(x = x, y = fct_rev(factor(segment)), fill = factor(segment))) +
    geom_density_ridges_gradient(scale = scale, 
                                 alpha = alpha,
                                 quantile_lines = TRUE) +
    labs(x = x, y = 'Segment', fill = 'Segment') +
    coord_cartesian(expand = F) +
    util.lh2.theme()

  data <- data %>%
    group_by(segment) %>%
    summarise(
      count = n(),
      min = min(x, na.rm = T),
      Q1 = quantile(x, 0.25, na.rm = T),
      median = median(x, 0.5, na.rm = T),
      mean = mean(x, na.rm = T),
      Q3 = quantile(x, 0.75, na.rm = T),
      max = max(x, na.rm = T)
    )

  return(list(data = data, plot = plot))

}

#' stacked bar plot for single categorical variable
#' 
#' profile the numeric variable by segment, used for segment profiling
#' 
#' @inheritParams util.profile.num
#' @param x char, categorical variable name
#' @return list(data, plot)
#' @export
#' @importFrom ggplot2 coord_flip
#' @importFrom tidyr pivot_wider
#' @examples 
#' res <- util.profile.cat(mtcars, x = 'am', segment = 'gear')
#' res$data
#' res$plot

util.profile.cat <- function(data, x, segment = 'segment', 
                             alpha = 0.8){

  data <- data %>%
    rename(x = !!rlang::sym(x),
           segment = !!rlang::sym(segment))
  res <-  data %>%
    filter(!is.na(x) & x != '') %>%
    mutate(x = factor(x),
           segment = as.character(segment)) %>%
    group_by(segment,x) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(segment) %>%
    mutate(pct = n / sum(n),
           start = cumsum(pct),
           end = cumsum(lag(pct, 1, 0)),
           position = 1 - (start + end) /2 ,
           label = ifelse(pct > 0.1, scales::percent(pct, 1), '')) %>%
    ungroup()

  plot <- res %>%
    ggplot(aes(x = fct_rev(factor(segment)), y = pct, fill = x)) +
    geom_bar(stat = 'identity', position = 'stack', alpha = alpha) +
    geom_text(aes(y = position, label = label)) +
    labs(fill = x, x='Segment', y = '% of users') +
    scale_y_continuous(labels = scales::percent) +
    coord_flip(expand = F) +
    util.lh2.theme()

  total <- res %>%
    group_by(x) %>%
    summarise(n = sum(n)) %>%
    mutate(segment = 'overall',
           pct = n / sum(n)) %>%
    ungroup()

  data <- res %>%
    select(segment, x, n, pct) %>%
    bind_rows(total) %>%
    mutate(pct = scales::percent(pct, 1),
           n = format(n, big.mark = ','),
           value = glue("{n} ({pct})")) %>%
    rename(!!rlang::sym(x):= x) %>%
    pivot_wider(!!rlang::sym(x),
                names_from = segment,
                values_from = value)

  list(data = data, plot = plot)

}

#' density/stacked bar plots for both numeric/categorical variables 
#' 
#' profile both numeric (numeric type) and categorical (character or factor type) variables
#' 
#' @inheritParams util.profile.num
#' @param data input data.frame
#' @param fnames variables to be profiled
#' @return list(data, plot)
#' @export
#' @examples 
#' mtcars['am'] <- factor(mtcars[['am']])
#' res <- util.profile.fnames(mtcars, c('am','wt'), segment = 'gear')
#' library(patchwork)
#' wrap_plots(res$plot) + plot_layout(guides = 'collect')

util.profile.fnames <- function(data, fnames, segment,
                                alpha = 0.8, 
                                scale = 1){

  profiles.data <- list()
  profiles.plot <- list()

  pb <- progress_bar$new(
    format = "  processing :what [:bar] :percent eta: :eta",
    clear = FALSE,
    width = 60,
    total = length(fnames))

  for(fname in fnames){

    pb$tick(tokens = list(what = glue("{fname}   ")))

    if(is.numeric(data[[fname]])){
      res <- util.profile.num(data, fname, segment,
                              alpha = alpha,
                              scale = scale)
      profiles.data[[fname]] <- res$data
      profiles.plot[[fname]] <- res$plot
    }

    if(is.character(data[[fname]]) |
       is.factor(data[[fname]])){
      res <- util.profile.cat(data, fname, segment,
                              alpha = alpha)
      profiles.data[[fname]] <- res$data
      profiles.plot[[fname]] <- res$plot
    }
  }

  return(list(data = profiles.data, plot = profiles.plot))
}

#' summarize segments with custom aggregation function
#' 
#' summarize segments in tables
#' 
#' @param data output from `util.profile.fnames`
#' @param segment char, segment variable name
#' @param summ.func segment summarize function `e.g function(i) summarise(i, n = n())`
#' @param highlight.tol tolerance for highlighting
#' @param kable boolean, TRUE -> return kable table
#' @param ... extra params from `util.vis.kable`
#' @export
#' @importFrom dplyr n case_when
#' @importFrom kableExtra cell_spec
#' @examples 
#' util.segment.summarize(mtcars, segment = 'gear')
#' library(dplyr)
#' summ.func <- function(i){
#'   summarise(i, n = n(), 
#'                `am-0` = mean(am == 0),
#'                `am-1` = mean(am == 1))
#' }
#' util.segment.summarize(mtcars, 'gear', summ.func)
util.segment.summarize <- function(data,
                                   segment,
                                   summ.func = NULL,
                                   highlight.tol = 0.2,
                                   kable = TRUE,
                                   ...) {
  
  # highlight.tol:
  # beyond the percentage of difference from the global will be highlighted
  # segment.summ.func:
  # custom function used for summarization
  
  data <- data %>%
    rename(segment = !!rlang::sym(segment))

  if(is.null(summ.func)) summ.func <- function(i) summarise(i, n = n())
  # summarize segments
  stat.segment <- data %>%
    group_by(segment = as.character(segment)) %>%
    summ.func

  # summarize global
  stat.global <- data %>%
    summ.func %>%
    mutate(segment = 'Global')

  # bind rows of two stat. tables
  res <- stat.segment %>%
    mutate(segment = as.character(glue('Segment-{segment}'))) %>%
    bind_rows(stat.global)

  if ('n' %in% colnames(res)) {
    res <- res %>%
      mutate(n = format(n, big.mark = ',')) %>%
      rename(`# of user` = n)
  }
  
  if (kable) {
    res %>%
      mutate_if(is.numeric, function(i) {
        cell_spec(
          x = case_when(
            all(between(i, 0, 1)) ~ scales::percent(i, 1),
            i >= 1000 ~ format(as.integer(i), big.mark = ","),
            TRUE ~ as.character(round(i, 0))
          ),
          color = case_when(
            i > tail(i, 1) * (1 + highlight.tol) ~ 'red',
            i < tail(i, 1) * (1 - highlight.tol) ~ 'blue',
            i == tail(i, 1) ~ 'black',
            TRUE ~ 'gray'
          )
        )
      }) %>%
      util.vis.kable(...)
  } else {
    res
  }

}
