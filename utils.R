# common -----

util.init.logger <- function(logger = "", level = 'INFO', 
                             log.file = NULL, reset.log = TRUE){
  
  if(!require(logging)){
    stop('[logging] package is not installed')
  }
  
  logReset()
  
  basicConfig(level = level)
  if(!is.null(log.file)) {
    if(reset.log && file.exists(log.file)) file.remove(log.file)
    addHandler(writeToFile, file=log.file, logger = logger)
  } else {
    addHandler(writeToConsole, logger = logger)
  }
  
}


library(glue)
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

library(glue)
library(dplyr)
library(tidyr)
library(stringr)

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

# tabling -----

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

library(knitr)
library(kableExtra)
util.vis.kable <- function(df, convert_pct = F,
                           accuracy = 1,
                           full_width = F,
                           escape = F,
                           caption = NULL,
                           align = NULL,
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
  
  if(convert_pct){
    df <- df %>%
      mutate_if(is.percentage, function(i){
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


library(DT)
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

# plotting -----
library(ggplot2)

util.lh2.fill <- function(){
  '#008B8B'
}

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


# input data schema -----
library(jsonlite)

schema <- list(
  activity = list(character = c(
    'PartitionKey',
    'Date',
    'Device'
  ),
  integer = c(
    'RestingHeartRate',
    'VeryActiveMinutes',
    'FatburnMinutes',
    'CardioMinutes',
    'PeakMinutes',
    'Steps'     
  ),
  numeric = c(
    'CalsActivity',
    'CalsBMR',
    'ExerciseMinutes',
    'FairlyActiveMinutes',
    'VO2Max'
  )),
  sleepdata = list(
    character = c(
      'PartitionKey',
      'Date',
      'Device',
      'MainSleepInBedStart'
    ),
    integer = c(
      "MainSleepInBedMinutes",
      "TotalMinutesAsleep"
    )
  ),
  biometric = list(
    character = c(
      "PartitionKey",
      "BiometricsUpdatedDate",
      "DateOfBirth" ,
      "Sex" ,
      "Timezone" 
    ),
    numeric = c(
      "HeightCm",
      "WeightKg",
      "WaistCm",
      "HipCm",
      "BodyFatPercentage"
    )
  )
)

write_json(schema, './util/schema.json', pretty = T)


# clean data -----

util.get.mode <- function(x, na.rm = T){
  if(na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

util.agg.value <- function(x, agg.func = max){
  if(is.character(x)){
    na <- NA_character_
  } else {
    na <- NA_real_
  }
  
  x <- x[!is.na(x)]
  if(length(x) > 0){
    agg.func(x)
  } else {
    na
  }
}

util.cap.outlier <- function(x, verbose = F){

  x <- as.numeric(x)
  iqr <- IQR(x, na.rm = TRUE)
  q75 <- quantile(x, 0.75, na.rm = TRUE)
  q25 <- quantile(x, 0.25, na.rm = TRUE)
  limit.lo <- q25 - iqr * 1.5
  limit.hi <- q75 + iqr * 1.5
  if(verbose) message(glue('cap range: [{round(limit.lo,1)},{round(limit.hi,1)}]'))
  x[x < limit.lo & !is.na(x)] <- limit.lo
  x[x > limit.hi & !is.na(x)] <- limit.hi
  return(x)
}

# aggregate data -----
util.fill.na <- function(x, fill.with = 0){
  
  # fill.with can be constant or a function
  
  if(class(fill.with) == 'function'){
    x.without.na <- x[!is.na(x)]
    fill.with.value <- fill.with(x.without.na)
  } else {
    fill.with.value <- fill.with
  }
  
  ifelse(is.na(x), fill.with.value, x)
}

# prepare features ------
# center / scale to unit variance
util.feature.scale <- function(df, fnames, scaler = NULL){
  if(is.null(scaler)){
    x.mean <- df %>% summarise_at(fnames, mean, na.rm = T)
    x.sd <- df %>% summarise_at(fnames, sd, na.rm = T)
    scaler <- list(mean = x.mean, sd = x.sd)
  } else {
    x.mean <- scaler$mean
    x.sd <- scaler$sd
  }
  
  X <- df
  for(fname in fnames){
    X <- X %>%
      mutate(!!rlang::sym(fname):= (!!rlang::sym(fname) - as.numeric(x.mean[fname])) / as.numeric(x.sd[fname])) %>%
      mutate(!!rlang::sym(fname):= util.fill.na(!!rlang::sym(fname), fill.with = 0))
  }
  
  return(list(data = X, scaler = scaler))
}

# EDA -----

# correlation
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
  
  return(out %>% bind_rows(out.diag)%>%
           arrange(var1, var2))
  
}

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


# profile segments -----

util.profile.num <- function(data, x, segment = 'segment'){
  
  plot <- data %>%
    rename(x = !!rlang::sym(x),
           segment = !!rlang::sym(segment)) %>%
    filter(!is.na(x)) %>%
    filter(x <= quantile(x, 0.99, na.rm = T)) %>%
    filter(x >= quantile(x, 0.01, na.rm = T)) %>%
    ggplot(aes(x = x, y = fct_rev(factor(segment)), fill = factor(segment))) +
    geom_density_ridges_gradient(scale = 2, quantile_lines = TRUE) +
    labs(x = x, y = 'Segment', fill = 'Segment') +
    coord_cartesian(expand = F) +
    util.lh2.theme()
  
  data <- data %>%
    rename(x = !!rlang::sym(x)) %>%
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


util.profile.cat <- function(data, x, segment = 'segment'){
  
  res <- data %>%
    rename(x = !!rlang::sym(x),
           segment = !!rlang::sym(segment)) %>%
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
    geom_bar(stat = 'identity', position = 'stack') +
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
    tidyr::pivot_wider(!!rlang::sym(x), 
                       names_from = segment, 
                       values_from = value)
  
  list(data = data, plot = plot)
  
}

util.profile.fnames <- function(data, fnames){
  
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
      res <- util.profile.num(data, fname)
      profiles.data[[fname]] <- res$data
      profiles.plot[[fname]] <- res$plot
    }
    
    if(is.character(data[[fname]])){
      res <- util.profile.cat(data, fname)
      profiles.data[[fname]] <- res$data
      profiles.plot[[fname]] <- res$plot
    }
  }
  
  return(list(data = profiles.data, plot = profiles.plot))
}

util.segment.summize <- function(df.profile, segment.summ.func, 
                            highlight.tol = 0.2, kable = T, caption = NULL, 
                            ...){
  
  # highlight.tol: 
  # beyond the percentage of difference from the global will be highlighted
  # segment.summ.func:
  # custom function used for summarization
  
  # summarize segments
  stat.segment <- df.profile %>%
    group_by(segment = as.character(segment)) %>%
    segment.summ.func() %>%
    ungroup()
  
  # summarize global
  stat.global <- df.profile %>%
    segment.summ.func() %>%
    mutate(segment = 'Global')
  
  # right align columns except for the first column
  align <- glue('l{strrep("r", ncol(stat.segment))}')
  
  # bind rows of two stat. tables
  res <- stat.segment %>%
    mutate(segment = as.character(glue('Segment-{segment}'))) %>%
    bind_rows(stat.global)
  
  if(kable){
    # output kable table
    
    if('n' %in% colnames(res)){
      res <- res %>%
        mutate(n = format(n, big.mark = ',')) %>%
        rename(`# of user` = n) 
    }
    
    res %>%
      mutate_if(is.numeric, function(i){
        cell_spec(
          x = case_when(
            all(between(i, 0, 1)) ~ scales::percent(i, 1),
            i >= 1000 ~ format(as.integer(i), big.mark = ","),
            TRUE ~ as.character(round(i,0))
          ),
          color = case_when(
            i > tail(i, 1) * (1 + highlight.tol) ~ 'red',
            i < tail(i, 1) * (1 - highlight.tol) ~ 'blue',
            i == tail(i, 1) ~ 'black',
            TRUE ~ 'gray'
          ))
      }) %>%
      kable(escape = F, align = align, caption = caption) %>%
      kable_styling(c('hover', 'striped', 'condensed'), full_width = F, ...)%>%
      row_spec(0, bold = T, color = "white", background = util.lh2.fill()) %>%
      row_spec(nrow(stat.segment) + 1, bold = T, color = "black")
  } else{
    res
  }

}

