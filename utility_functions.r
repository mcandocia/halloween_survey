library(qdapTools)
library(tidyr)
library(scales)

#clustering
calculate_distance_matrix <- function(data, formula, value.var, sim.function=jaccard_similarity_){
  wide_matrix = acast(data, formula, value.var = value.var)
  distmat = 1 - outer(1:nrow(wide_matrix), 1:nrow(wide_matrix), 
                      FUN=Vectorize(cosine_similarity_, vectorize.args=c('i','j')), 
                      m=wide_matrix)
  rownames(distmat) = colnames(distmat) = rownames(wide_matrix)
  return(as.dist(distmat))
}

split_columns <- function(data, column, sep=';'){
  new_columns = mtabulate(lapply( strsplit(data[,column], sep),
                                  function(x) paste0(column,'_',x))
  )
  return(cbind(data, new_columns))
}

bold_percent <- function(x) sprintf('bold(%s)', percent(x))

relevel_var <- function(data, var, lvls){
  data[,var]=factor(as.character(data[,var]),levels=lvls)
  return(data)
}

fix_factors <- function(data){
  data %>% relevel_var('region', 
                       c('Midwest', 'Northeast', 'Southeast', 
                         'Southwest', 'West', 
                         'I do not live in the United States')) %>%
    relevel_var('celebrates_christmas', c('Yes','No'))  %>%
    mutate(region=mapvalues(region,from='I do not live in the United States', to='Non-US')) %>%
    relevel_var('age_group', c('Under 18','18-24','25-34','35-44','45-54','55+')) %>% 
    relevel_var('parent_gifts', c('Yes, and they claimed they were from Santa','Yes, but they didn\'t say they were from Santa','No'))
  
}

#replaces each space in a factor's levels with a newline
split_factor_levels_with_newlines <- function(data, variable){
  levels = levels(data[,variable])
  target_levels = gsub(' ', '\n', levels)
  data[,variable] = mapvalues(data[,variable], from=levels, to=target_levels)
  return(data)
}

#uses SE on middle two columns and NSE on the pattern
smart_gather <- function(data, keycol, valcol, pattern, factor_key){
  command = sprintf('gather(data, %s, %s, pattern, factor_key=factor_key)', keycol, valcol)
  eval(parse(text=command))
}

#regroup columns into molten frame
gather_category <- function(data, prefix, sort_by_count=TRUE, deperiodize=TRUE) {
  new_column = gsub('_','', prefix)
  val_column =  paste0(prefix, 'value')
  #in case it hasn't been removed
  data[,new_column] = NULL
  newcol_e = new_column
  newval_e = val_column
  new_data = smart_gather(data, newcol_e, newval_e, starts_with(prefix), 
                          factor_key=TRUE)
  if (sort_by_count){
    counts = table(new_data[new_data[,val_column]==1, new_column])
    count_names = names(counts)
    new_data[,new_column] = factor(new_data[,new_column], levels = count_names[order(counts)])
  }
  else{
    new_data[,new_column] = factor(new_data[,new_column])
  }
  #remove periods for values.that.look.like.this
  deperiodize_ <- function(x) gsub('[.]+',' ',x)
  if (deperiodize)
    trans=deperiodize_
  else
    trans=identity
  new_data[,new_column] = mapvalues(new_data[,new_column], 
                                    from = levels(new_data[,new_column,]),
                                    to = trans(gsub(prefix,'', levels(new_data[,new_column]))))
  return(new_data)
}

# similarity functions

cosine_similarity <- function(x, y){
  as.numeric(x %*% y /sqrt(sum(x^2)*sum(y^2)))
}

cosine_similarity_ <- function(i, j, m){
  cosine_similarity(m[i,], m[j,])
}

jaccard_similarity <- function(x, y){
  sum(x & y)/sum(x | y)
}

jaccard_similarity_ <- function(i, j, m){
  jaccard_similarity(m[i,],m[j,])
}

refactor_by_cluster <- function(data, cluster){
  data %>% mutate(Var1 = factor(Var1, levels = levels(Var1)[cluster$order]),
                  Var2 = factor(Var2, levels = levels(Var2)[cluster$order])
  )
}


calculate_group_stats <- function(data, variable, value_variable, group_variables = NULL){
  if (missing(value_variable))
    value_variable = paste0(variable, '_value')
  x = data
  if (!is.null(group_variables))
    x  = x %>% group_by_at(vars(one_of(c(group_variables, variable))))
  else
    x = x %>% group_by_at(vars(variable))
  x %>% summarise_at(.vars=value_variable, .funs=list(count=sum, prop=mean))
}

#plotting theme changes for text size
better_text_size <- theme(axis.text=element_text(size=rel(2)),
                          axis.title=element_text(size=rel(2)),
                          plot.title=element_text(size=rel(2)),
                          plot.subtitle=element_text(size=rel(2)),
                          legend.title=element_text(size=rel(2)),
                          legend.text=element_text(size=rel(2)))

better_text_size_manylabs <- theme(axis.text=element_text(size=rel(1.3)),
                                   axis.title=element_text(size=rel(1.5)),
                                   plot.title=element_text(size=rel(1.5)),
                                   plot.subtitle=element_text(size=rel(1.3)),
                                   legend.title=element_text(size=rel(1.5)),
                                   legend.text=element_text(size=rel(1.1)))

better_text_size_tiled <- theme(axis.text=element_text(size=rel(1.15)),
                                axis.title=element_text(size=rel(1.5)),
                                plot.title=element_text(size=rel(1.5)),
                                plot.subtitle=element_text(size=rel(1.3)),
                                legend.title=element_text(size=rel(1.5)),
                                legend.text=element_text(size=rel(1.1)))
