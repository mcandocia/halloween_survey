source('cleaning.r')
source('utility_functions.r')

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(cetcolor)
library(tm)
library(wordcloud)
#source('plotting_functions.r')

halloween = read.csv('halloween.csv') 

#functions to transform words
map_words <- function(text){
  text %>% 
    VectorSource %>%
    Corpus %>%
    tm_map(tolower) %>%
    tm_map(removeWords, stopwords('english') )
}

comp.wordcloud <- function(data, scale=c(10,1), ...){
  wordcloud(data %>% map_words, colors=c(cet_pal(10, 'fire')[1:8], '#F00101'),
            scale=scale, random.order=FALSE, ...)
}

ntitle <- function(text, cex=1.5, n_newline=4){
  title(paste0(paste(rep('\n', n_newline), collapse=''), text), cex.main=cex)
}

comp.wordcloud(halloween$three_words, scale=c(8,1.1))
ntitle('How People Describe Halloween')

# this method doesn't work out too good...
comp.wordcloud( gsub('â€™|\'','', halloween$favorite_candy), scale=c(5,1))
ntitle('Favorite Halloween candies')

comp.wordcloud(gsub('â€™|\'','',halloween$worst_candy), scale=c(4.3,1))
ntitle('Least-favorite candies')

# just use regex to aggregate the good ones...
worst_candies = halloween %>% 
  mutate(wc=gsub('â€™','\'', tolower(worst_candy))) %>%
  summarize(
  `candy corn`=sum(grepl('corn', wc)),
  `licorice`=sum(grepl('licor|liqu', wc)),
  `Almond Joy`=sum(grepl('almondjoy|almond joy', wc)),
  `Snickers`=sum(grepl('snicker\'?s', wc)),
  `Dots`=sum(grepl('dots', wc)),
  `Milk Duds`=sum(grepl('milk duds|milkduds', wc)),
  `taffy`=sum(grepl('taffy', wc)),
  `Tootsie Rolls`=sum(grepl('tootsie', wc)),
  `jelly beans`=sum(grepl('jelly beans', wc)),
  `Smarties`=sum(grepl('smarties',wc)),
  `Twizzlers`=sum(grepl('twizzler', wc))
)

favorite_candies = halloween %>% 
  mutate(fc=gsub('â€™','\'', tolower(favorite_candy))) %>%
  summarize(
    `Reese's`=sum(grepl('reese|reece',fc)),
    `Twix`=sum(grepl('twix',fc)),
    `Chocolate`=sum(grepl('^chocolates?$|^anythin.{1,5}chocolate', fc)),
    `Kit-Kat bars`=sum(grepl('kit', fc)),
    `Snickers`=sum(grepl('snickers', fc)),
    `Butterfingers`=sum(grepl('butterfinger', fc)),
    `100 Grand`=sum(grepl('100.*grand',fc)),
    `Smarties`=sum(grepl('smarties', fc)),
    `Skittles`=sum(grepl('skittles', fc)),
    `M&Ms`=sum(grepl('m&m|(^| )m and m', fc)),
    `3 Musketeers`=sum(grepl('musket',fc)),
    `Hershey's chocolate`=sum(grepl('hersh?ey', fc)),
    `Milky Way`=sum(grepl('milky way', fc)),
    `Nerds`=sum(grepl('nerds', fc))
    
  )

n_responses = nrow(halloween)

transpose_cat_frame <- function(data, trans=identity){
  data %>% melt() %>%
    mutate(variable=factor(as.character(variable), levels = levels(variable)[order(value)])) %>%
    mutate(barcolor=trans(c('#222222','#EE5501'))[1 + (variable %in% levels(variable)[c(T,F)])]) %>%
    mutate(value=value/n_responses, label=percent(value))
  
}

ggplot(favorite_candies %>% transpose_cat_frame()) + 
  geom_bar(aes(x=variable, y=value, fill=barcolor), stat='identity') +
  coord_flip() + scale_fill_identity() + 
  scale_y_continuous(label=percent) + 
  xlab('Percent responses as favorite candy') + 
  ylab('Candy Type or Brand') + 
  ggtitle('Favorite candies among responses to Halloween survey', 
          subtitle='Source: maxcandocia.com/link/halloween-words') +
  theme_bw() + geom_text(aes(x=variable, y=value, label=label), color='#888888', hjust='inward', size=8)


ggplot(worst_candies %>% transpose_cat_frame(trans=rev)) + 
  geom_bar(aes(x=variable, y=value, fill=barcolor), stat='identity') +
  coord_flip() + scale_fill_identity() + 
  scale_y_continuous(label=percent) + 
  xlab('Percent responses as least-favorite candy') + 
  ylab('Candy Type or Brand') + 
  ggtitle('Least-favorite candies among responses to Halloween survey', 
          subtitle='Source: maxcandocia.com/link/halloween-words') +
  theme_bw() + geom_text(aes(x=variable, y=value, label=label), color='#888888', hjust='inward', size=8)



