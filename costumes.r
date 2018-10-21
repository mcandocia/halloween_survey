source('cleaning.r')
source('utility_functions.r')
library(scales)
library(ape)
library(gridExtra)
library(cetcolor)

# some spell fixes
costume_factor_remapping = list(
  'A pun based costume'='A pun-based costume',
  'Musician Actor Artist'='Musician/Actor/Artist',
  'Prince Princess'='Prince/Princess',
  'Mermaid merman'='Mermaid/Merman',
  'Witch not from movie TV show'='Witch not from movie/TV show',
  'Wizard not from movie TV show or Washington Wizards'='Wizard not from movie/TV show'
)

costumes_from = names(costume_factor_remapping)
costumes_to = unlist(costume_factor_remapping)

# costumes is a very tall data frame with info on costumes each person used
costumes = read.csv('costumes.csv')
# halloween is the default data frame with cleaned column names and survey weights
halloween = read.csv('halloween.csv') 

costumes$costume_type = costumes$costume_type %>% mapvalues(from=costumes_from, to=costumes_to)

costumes_anyage = costumes %>%
  group_by(costume_type, id) %>%
  summarize(
    costume_value=max(costume_value)
  ) %>% ungroup() %>% 
  left_join(halloween[, !grepl('costume_.*',names(halloween))], by='id')


costumes_summary = costumes %>%
  group_by(costume) %>%
  summarize(
    average = mean(costume_value),
    weighted_average=mean(costume_value*weight),
    average_sigma = weighted.var.sigma(costume_value, rep(1, n())),
    weighted_sigma=weighted.var.sigma(costume_value, weight)
  ) %>%
  ungroup()

costumes_subdivided_summary = costumes %>%
  group_by(costume_type, costume_time) %>%
  summarize(
    average = mean(costume_value),
    weighted_average=mean(costume_value*weight),
    average_sigma = weighted.var.sigma(costume_value, rep(1, n())),
    weighted_sigma=weighted.var.sigma(costume_value, weight)
  ) %>%
  ungroup()

costumes_gender_summary = costumes %>% filter(gender != "Other") %>%
  group_by(gender, costume_type) %>%
  summarize(
    average = mean(costume_value),
    weighted_average=mean(costume_value*weight),
    average_sigma = weighted.var.sigma(costume_value, rep(1, n())),
    weighted_sigma=weighted.var.sigma(costume_value, weight)
  ) %>%
  ungroup()

costumes_anyage_summary = costumes_anyage %>%
  group_by(costume_type) %>%
  summarize(
    average = mean(costume_value),
    weighted_average=mean(costume_value*weight),
    average_sigma = weighted.var.sigma(costume_value, rep(1, n())),
    weighted_sigma=weighted.var.sigma(costume_value, weight)    
  ) %>% ungroup()

taller_costumes_anyage = rbind(
  costumes_anyage_summary %>% transmute(costume=costume_type, average=average, sigma=average_sigma, weight='unweighted'),
  costumes_anyage_summary %>% transmute(costume=costume_type, average=weighted_average, sigma=weighted_sigma, weight='weighted')
)

# plot the costume weights (for verification)
ggplot(taller_costumes_anyage) + geom_bar(aes(x=costume, y=average, fill=weight), stat='identity', position='dodge') + 
  geom_errorbar(aes(x=costume, ymin=average-1.96*sigma, ymax=average+1.96*sigma, group=weight), position=position_dodge(width=0.75)) + 
  coord_flip() + scale_y_continuous(label=percent)

# order by weighted average, then send other categories to the back
costumes_level_ordered = costumes_anyage_summary$costume_type[order(costumes_anyage_summary$weighted_average)] %>% as.character()
end_levels = c('Something else not listed')
costumes_level_ordered=c(end_levels, costumes_level_ordered[!costumes_level_ordered %in% end_levels])
costumes_anyage_summary$costume_type = factor(as.character(costumes_anyage_summary$costume_type), levels=costumes_level_ordered)

costumes_anyage_summary$barcolor = ifelse(costumes_anyage_summary$costume_type %in% costumes_level_ordered[c(T,F)], '#222222','#EE5501')


costumes_subdivided_summary$costume_type = factor(as.character(costumes_subdivided_summary$costume_type), levels=costumes_level_ordered)
costumes_subdivided_summary$barcolor = ifelse(costumes_subdivided_summary$costume_type %in% costumes_level_ordered[c(T,F)], '#222222','#EE5501')

costumes_gender_summary$costume_type = factor(as.character(costumes_gender_summary$costume_type), levels=costumes_level_ordered)
costumes_gender_summary$barcolor = ifelse(costumes_gender_summary$costume_type %in% costumes_level_ordered[c(T,F)], '#222222','#EE5501')



# make it look pretty
ggplot(costumes_anyage_summary) + geom_bar(aes(x=costume_type, y=weighted_average, fill=barcolor), stat='identity') +
  scale_fill_identity() +
  coord_flip() + 
  scale_y_continuous(label=percent) +
  xlab('Costume Type') + ylab('Percent of US population who has worn costume') + 
  ggtitle('Most common costume types historically worn by Americans', 
          subtitle='Red error bars indicate 95% confidence intervals\nSource:maxcandocia.com 2018 Halloween survey') + 
  geom_errorbar(aes(x=costume_type, ymin=weighted_average-1.96*weighted_sigma, ymax=weighted_average+1.96*weighted_sigma), color='red')

# divided by age of costume
costumes_subdivided_summary = costumes %>%
  group_by(costume_type, costume_time) %>%
  summarize(
    average = mean(costume_value),
    weighted_average=mean(costume_value*weight),
    average_sigma = weighted.var.sigma(costume_value, rep(1, n())),
    weighted_sigma=weighted.var.sigma(costume_value, weight)
  ) %>%
  ungroup() %>%
  mutate(weighted_average=ifelse(costume_time=='As an adult', weighted_average/adult_proportion(),weighted_average),
         weighted_sigma=ifelse(costume_time=='As an adult', weighted_sigma/adult_proportion(),weighted_sigma)
  )

# divided by gender
ggplot(costumes_gender_summary) + 
  geom_bar(aes(x=costume_type, y=weighted_average, fill=barcolor), stat='identity') +
  scale_fill_identity() +
  coord_flip() + 
  scale_y_continuous(label=percent) +
  xlab('Costume Type') + ylab('Percent of US population who has worn costume') + 
  ggtitle('Most common costume types historically worn by Americans ,grouped by gender', 
          subtitle='Red error bars indicate 95% confidence intervals\nSource:maxcandocia.com 2018 Halloween survey') + 
  geom_errorbar(aes(x=costume_type, ymin=weighted_average-1.96*weighted_sigma, ymax=weighted_average+1.96*weighted_sigma), color='red') + 
  facet_wrap(~gender)

gender_props = costumes_gender_summary %>% 
  group_by(costume_type) %>%
  summarize(gender_diff = sum(weighted_average[gender=='Male']/(weighted_average[gender=='Female']+0.0000001))) %>%
  ungroup() 

costume_prevalence_order = as.character(gender_props$costume_type)[order(gender_props$gender_diff)]

costumes_gender_summary2 = costumes_gender_summary %>% mutate(
  costume_type = factor(as.character(costume_type), levels=costume_prevalence_order)
)

ggplot(costumes_gender_summary2) + 
  geom_bar(aes(x=costume_type, y=weighted_average, fill=gender), position='fill', stat='identity') + 
  coord_flip() + scale_fill_manual(values=c('Male'='#EE5501','Female'='#222222')) + 
  scale_y_continuous(label=percent) + ylab('Proportion of Costumes') + xlab('Costume Type') +
  ggtitle('Relative proportions of Halloween costumes worn by gender',
          subtitle='Source: maxcandocia.com 2018 Halloween survey')


# this way isn't working out...
"
male_plot = ggplot(costumes_gender_summary %>% filter(gender=='Male')) + 
  geom_bar(aes(x=costume_type, y=weighted_average, fill=barcolor), stat='identity') +
  scale_fill_identity() +
  coord_flip() + 
  #scale_y_continuous(label=percent) +
  xlab('Costume Type') + ylab('% has worn costume') + 
  ggtitle('Most common costume types historically worn by Americans, grouped by gender', 
          subtitle='Red error bars indicate 95% confidence intervals\nSource:maxcandocia.com 2018 Halloween survey') + 
  geom_errorbar(aes(x=costume_type, ymin=weighted_average-1.96*weighted_sigma, ymax=weighted_average+1.96*weighted_sigma), color='red') + 
  facet_wrap(~gender) + scale_y_reverse(label=percent)


female_plot = ggplot(costumes_gender_summary %>% filter(gender=='Female')) + 
  geom_bar(aes(x=costume_type, y=weighted_average, fill=barcolor), stat='identity') +
  scale_fill_identity() +
  coord_flip() + 
  scale_y_continuous(label=percent) +
  xlab('') + ylab('% has worn costume') + 
  ggtitle('Most common costume types historically worn by Americans, grouped by gender', 
          subtitle='Red error bars indicate 95% confidence intervals\nSource:maxcandocia.com 2018 Halloween survey') + 
  geom_errorbar(aes(x=costume_type, ymin=weighted_average-1.96*weighted_sigma, ymax=weighted_average+1.96*weighted_sigma), color='red') + 
  facet_wrap(~gender) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.title=element_text(color='white'),
        plot.subtitle=element_text(color='white'))

grid.arrange(male_plot, female_plot, nrow=1, widths=c(18,8))
"

#okay I want to see clusters!!!

# use anyage right now

general_costume_stats = calculate_group_stats(
  costumes_anyage, 
  'costume_type',
  'costume_value',
  group_variables='id'
)

costume_distance_matrix = calculate_distance_matrix(
  general_costume_stats,
  costume_type ~ id,
  value.var='prop',
  jaccard_similarity
)

costume_clusters = hclust(costume_distance_matrix, method='ward.D2')

costume_cluster_labels = cutree(costume_clusters, 3)

plot(as.phylo(costume_clusters), main='Halloween costumes clustered by who wears them',
     tip.color =c('#222222','#EE5501','#BB2244')[costume_cluster_labels]
)

# make tile plot of similarity matrix
ggplot(melt(as.matrix(1-as.matrix(costume_distance_matrix))) %>% 
         refactor_by_cluster(costume_clusters)) + 
  geom_tile(aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradientn('similarity\n', colors=cet_pal(9, 'fire'),
                       limits = 0:1) + 
  xlab('') + ylab('') + 
  ggtitle('Similarity of Halloween Costumes by Usage Among Individuals') + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

 
#  geom_text(aes(x=Var1, y=Var2, label=round(value,2)))



# maybe don't worry about these
"
# YOUNG ONLY

young_costume_stats = calculate_group_stats(
  costumes %>% filter(costume_time=='When you were 9 or younger'), 
  'costume_type',
  'costume_value',
  group_variables='id'
)

young_costume_distance_matrix = calculate_distance_matrix(
  young_costume_stats,
  costume_type ~ id,
  value.var='prop',
  jaccard_similarity
)

young_costume_clusters = hclust(young_costume_distance_matrix, method='ward.D2')

young_costume_cluster_labels = cutree(young_costume_clusters, 5)

plot(as.phylo(young_costume_clusters))



# TEENAGE ONLY


# ADULT ONLY
"
