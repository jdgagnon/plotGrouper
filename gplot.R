# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License


function(dataset = NULL, # Define your data set which should be a gathered tibble
                  comparison = NULL, # Specify the comparison you would like to make (e.g., Genotype)
                  group.by = NULL, # Specify the variable to group by (e.g., Tissue)
                  levs = T,
                  val = "value", # If your tibble is not 'tidy', and there are multiple value columns, specify the one you want to plot
                  geom = c('bar','errorbar','dot','stat','seg'), # Define the list of geoms you want to plot
                  p = "p.signif", # Specify representation of pvalue ('p.signif' = *; 'p.format' = 'p = 0.05')
                  ref.group = NULL, comparisons = NULL, method = "t.test", paired = F, errortype = "mean_se",
                  y.lim = NULL, y.lab = NULL, trans.y = 'identity', x.lim = NULL, x.lab = NULL, trans.x = 'identity', sci = F,
                  file = NULL, save.it = F, aspect.ratio = 1, angle.x = F, levs.comps = T,
                  group.labs = NULL, stats = F, split = T, split_str = NULL, trim = 'el zilcho', leg.pos = "top",
                  stroke = 0.5, font_size = 9, size = 1, width = 0.75, dodge = 0.75,
                  shape.groups = c(19,21), color.groups = c("black",'black'), fill.groups = c('black',NA))
{
  
  df <- droplevels(dataset)

  # Assign labels to the groups
  suppressWarnings(if(is.null(group.labs) & split == F){
    group.labs <- function(x) x
  }else if(is.null(group.labs) & split == T){
    group.labs <- function(x) sapply(str_remove(word(str_remove(x, trim),-1,sep = '/'), ' %| #|% |# '), '[',1)
  }else if(is.null(group.labs) & split == T & !is.null(split.str)){
    group.labs <- function(x) sapply(strsplit(str_remove(x, trim), split = split.str, fixed = TRUE), "[", 2)
  }else {
    group.labs <- group.labs
  })

  # If the column of values has a different column name than 'value', assign it 'value'
  colnames(df)[colnames(df) == val] <- "value"
  df$value <- as.numeric(df$value)
  
  # If comparison variable is not a factor, coerce it to one
  if (!is.factor(df[[comparison]])){
    df[,comparison] <- factor(df[[comparison]], levels = unique(df[[comparison]])[levs.comps])
  }
  
  # If grouping variable is not numeric, scale x discretely, and assign levels. If it is numeric, scale x continuously
  if (is.factor(df[[group.by]])){
    scale.x <- scale_x_discrete(labels = group.labs, breaks = unique(df[[group.by]]))
  } else if (!is.numeric(df[[group.by]]) & !is.factor(df[[group.by]])){
    df[,group.by] <- factor(df[[group.by]], levels = unique(df[[group.by]])[levs])
    scale.x <- scale_x_discrete(labels = group.labs, breaks = unique(df[[group.by]]))
  }else{
    scale.x <- scale_x_continuous(breaks = df[[group.by]], labels = formatC(df[[group.by]], drop0trailing = T), trans = trans.x)
    # message('Grouping variable is numeric and will be scaled continuously')
  }
  
  df <- arrange_(df, comparison)  # Arrange tibble according to levels of comparison
  
  # Create a tibble of max values by group for assigning height of p values
  dmax <- droplevels(df %>% 
                       group_by_(group.by, comparison) %>%
                       mutate(error_max = max(get(errortype)(value, mult = 1), na.rm = T)) %>%
                       ungroup() %>% 
                       group_by_(group.by) %>%
                       slice(which.max(value)) %>%
                       select_(group.by, 'value', 'error_max') %>%
                       ungroup() %>%
                       arrange_(group.by))
  
  if(is.null(y.lim)){
    # y.lim <- c(0,NA)
    y.lim <- c(0, max(dmax[,c('value','error_max')], na.rm = T)*1.08)
    expand.y <- c(0,0)
  }
  
  # If no comparisons are specified, perform all comparisons
  if (is.null(comparisons)) { 
    comparisons <- df[[comparison]]
  }
  
  symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", NA))
  
  # Calculate p values for all comparisons being made
  statOut <- try(ggpubr::compare_means(formula = formula(paste('value', '~', comparison)), 
                                          data = df, method = method, group.by = group.by, ref.group = ref.group, 
                                          paired = paired, symnum.args = symnum.args))
  
  if (method %in% c('t.test', 'wilcox.test')){
    statistics <- try(left_join(statOut, dmax, by = group.by) %>%
      filter(group1 %in% comparisons | group2 %in% comparisons) %>%
      mutate(max = max(c(value, error_max), na.rm = T)*0.05) %>%
      group_by_(group.by) %>%
      mutate(n = row_number(),
             group1 = factor(group1, levels = levels(df[[comparison]])),
             group2 = factor(group2, levels = levels(df[[comparison]])),
             r = if_else(p == 'p.signif', max*0.1, max*0.2),
             e = max*n,
             n_comps = max(n),
             group.by_n = as.numeric(get(group.by)), # number of grouping variables
             group1_n = as.integer(group1),
             group2_n = as.integer(group2),
             unit = (width/max(group2_n))*dodge/width, # relative width of 1 bar
             start = group.by_n - unit*max(group2_n)/2, # center position of the first bar
             w.start = start + (group1_n - 1)*unit + unit/2, # center position of group1
             w.stop = start + (group2_n - 1)*unit + unit/2) %>% # center position of group2
      ungroup() %>%
      mutate(x.pos = rowMeans(.[,c('w.start','w.stop')]), # center of line segment
             w.start = ifelse(!is.na(p.signif), w.start, NA),
             w.stop = ifelse(!is.na(p.signif), w.stop, NA),
             h.p = value + e,
             h.s = value + e - r))

  } else {
    statistics <- try(left_join(statOut, dmax, by = group.by) %>%
      mutate(max = max(c(value, error_max), na.rm = T)*0.05) %>%
      group_by_(group.by) %>%
      mutate(n = row_number(),
             r = if_else(p == 'p.signif', max*0.1, max*0.2),
             e = max*n,
             n_comps = max(n),
             group.by_n = as.numeric(get(group.by)),
             w.start = group.by_n - width/length(unique(df[[comparison]])),
             w.stop = group.by_n + width/length(unique(df[[comparison]]))) %>%
      ungroup() %>%
      mutate(x.pos = rowMeans(.[,c('w.start','w.stop')]), # center of line segment
             w.start = ifelse(!is.na(p.signif), w.start, NA),
             w.stop = ifelse(!is.na(p.signif), w.stop, NA),
             h.p = value + e,
             h.s = value + e - r))
  }
                      

  if(("try-error" %in% class(statOut))){
    statistics <- tibble('p.signif' = NA)
  }
  
  if (stats == T) {
    return(statOut)
  }
  
  if (trans.y != 'identity' & !all(is.na(statistics$p.signif))) {
    logBase <- parse_number(trans.y)
    statistics <- statistics %>%
      mutate(h.p = h.p^logBase,
             h.s = h.s^logBase)
  }
  
  if(is.null(y.lab) | y.lab == ''){
    y.lab <- df$variable
  } # If not specified by user, set y axis label to the variable being plotted.
  
  # Make pretty scientific notation 
  max_e <- as.numeric(str_split(string = format(max(c(dmax$value, dmax$error_max)), scientific = T), pattern = 'e\\+')[[1]][2])
  fancy_scientific <- function(l) {
    l <- format(l, scientific = TRUE)
    e <- as.numeric(sapply(l, function(a) str_split(a, pattern = 'e\\+')[[1]][2]))
    e_dif <- as.numeric(sapply(e, function(x) (max_e - x)))
    l <- as.numeric(sapply(l, function(x) str_split(string = x, pattern = 'e\\+')[[1]][1]))
    l2 <- c()
    for(i in 1:length(e)) {
      l2[i] <- ifelse(e[i] == 0, l[i], 
                      ifelse(e[i] < max_e, (10^-e_dif[i])*l[i], l[i]))
    }
    
    format(l2, trim = F)
  }
  
  if (angle.x == T) {
    angle <- 45
    vjust <- 1
    hjust <- 1
  } else {
    angle <- 0
    vjust <- 0
    hjust <- 0.5
  }
  
  if (isTRUE(sci)) {
    labs.y <- fancy_scientific
    y.lab <- bquote(.(paste0(gsub('\\s*#\\s*', '', y.lab),' ')) (10^.(max_e)))
  } else { labs.y <- waiver()}
  
  if (trans.y != 'identity') {
    # logBase <- parse_number(trans.y)
    # log_trans <- function(x) {
    #   x <- sapply(x, function(a) log(a,logBase))
    #   format(x)
    # }
    if (trans.y == 'log2') {
      labs.y <- scales::trans_format(trans.y, scales::math_format(2^.x))
    } else {
      labs.y <- scales::trans_format(trans.y, scales::math_format(10^.x))
    }
    
    y.lim <- c(NA,NA)
    expand.y <- c(0.05,0.05)
  }
  
  # Assign names to the shape, fill, color, and alpha arguments
  for (x in c('shape.groups', 'fill.groups', 'color.groups')) {
    assign(x, setNames(object = get(x), levels(df[[comparison]])))
  }
  
  # Create geoms
  crossbar <- stat_summary(aes(group = get(comparison), color = get(comparison)), fun.y=mean, fun.ymax = mean, fun.ymin = mean, size = stroke/3,
                           geom="crossbar", width=width, position = position_dodge(dodge))
  point <- geom_point(aes(shape = get(comparison),  color = get(comparison)), stroke = stroke, size = size, position = position_jitterdodge(jitter.width = 0.25, dodge.width = dodge))
  errorbar <- stat_summary(aes(group = get(comparison)), fun.data = errortype, fun.args = list(mult=1),
                           geom = "errorbar", color = "black", width = 0.25*width, position = position_dodge(dodge), size = stroke)
  bar <- stat_summary(aes(fill = get(comparison)), color = 'black', fun.y=mean,
                      size = stroke, geom="bar", width = width, position = position_dodge(dodge), show.legend = T)
  violin <- geom_violin(aes(fill = get(comparison), color = get(comparison)), show.legend = T, position = position_dodge(dodge))
  box <- geom_boxplot(aes(color = get(comparison), fill = get(comparison)), show.legend = T, position = position_dodge(dodge))
  line <- stat_summary(aes(group = get(comparison), color = get(comparison)), fun.y = mean, geom = "line", size = stroke)
  line_error <- stat_summary(aes(group = get(comparison)), fun.data=errortype, fun.args = list(mult=1), 
                             geom="errorbar", color="black", width = width, size = stroke)
  line_point <- stat_summary(aes(fill = get(comparison), shape = get(comparison),  color = get(comparison)), stroke = stroke, size = size,
                             fun.y = mean, geom = "point")
  dot <- geom_dotplot(aes(color = get(comparison)), binaxis = "y", stackdir = "center", method = 'histodot', position = position_dodge(dodge))
  density <- geom_density(aes(color = get(comparison), fill = get(comparison), x = value), inherit.aes = F)
  
  if(all(is.na(statOut$p.signif))) {
    geom <- geom[which(!geom %in% c('stat', 'seg'))] 
  }else {
    stat <- geom_text(data = statistics, aes(x.pos, na.omit(h.p)), label = statistics[[p]], 
                      family = 'Helvetica', size = font_size/(1/0.35), inherit.aes = F, na.rm = T)
    seg <- geom_segment(data = statistics, aes(x = w.start, xend = w.stop,
                                               y = h.s, yend = h.s), size = stroke, inherit.aes=F, na.rm = T)
  }
  
  suppressWarnings(if(geom == 'line_point_stat'){
    geom <- c('line','line_error','line_point','stat')
    })
  
  suppressWarnings(if(geom %in% 'density') {
    scale.x <- scale_x_continuous()
    y.lim <- c(0,NA)
    })
  
  # Create ggplot object and plot
  g <- ggplot(data = df, aes(x = get(group.by), y = value)) +
    labs(x = x.lab, y = y.lab, color = comparison, shape = comparison, fill = comparison, alpha = comparison, hjust = 0.5) +
    scale_y_continuous(limits = y.lim, expand = expand.y, labels = labs.y, trans = trans.y, oob = function(x, ...) x) +
    scale.x +
    scale_shape_manual(values = shape.groups) +
    scale_fill_manual(values = fill.groups) +
    scale_alpha_manual(values = alpha.groups) +
    scale_color_manual(values = color.groups) +
    coord_cartesian(clip = 'off') +
    lapply(geom, function(x) get(x)) +
    theme(line = element_line(colour = "black", size = stroke),
          text = element_text(family = 'Helvetica', size = font_size, colour = "black"),
          rect = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = aspect.ratio,
          legend.position = leg.pos,
          legend.title = element_text(family = 'Helvetica', size = font_size, colour = "black"),
          legend.text = element_text(family = 'Helvetica', size = font_size, colour = "black"),
          # axis.line = element_line(colour = "black", size = stroke),
          axis.line.x = element_line(colour = "black", size = stroke),
          axis.line.y = element_line(colour = "black", size = stroke),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(family = 'Helvetica', size = font_size, colour = "black", angle = angle, vjust = vjust, hjust = hjust),
          axis.text = element_text(family = 'Helvetica', size = font_size, colour = "black"))
          # plot.margin = margin(1,1,1,1,'mm'))
  
  if (stats == F) {
    g
  }

}