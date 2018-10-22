source('cleaning.r')
source('utility_functions.r')
library(scales)
library(ape)
library(survival)

halloween = read.csv('halloween.csv') 

# note that a very large number of these answers are uncertain in time range
# table(halloween$trick_or_treat_age_start)
# table(halloween$trick_or_treat_age_end)

# basic transformations
halloween$right_censored = halloween$still_trick_or_treats=='Yes'
treats = halloween %>% filter(Did.you.ever.go.trick.or.treating.=='Yes')

# https://pdfs.semanticscholar.org/7f14/de9d83684d50ec0ae2691929d98b8f34f69d.pdf


# test with weights

is_int <- function(x){grepl('^\\d+$',x)}

treats_simple = treats %>% filter(is_int(trick_or_treat_age_end))

simple_survival_obj=Surv(time=as.integer(as.character(treats_simple$trick_or_treat_age_end)), 
                  event=1-treats_simple$right_censored
)

simple_fit = survfit(simple_survival_obj ~ 1, data=treats_simple, weights=treats_simple$weight, error='greenwood')

simple_fit2 = survfit(simple_survival_obj ~ 1, data=treats_simple, weights=treats_simple$weight/2, error='greenwood')

summary(simple_fit)
summary(simple_fit2)

# try resampling method

modify_interval <- function(x){
  if (x %in% c('', NA)){
    return(-999)
  }
  if (is_int(x)){
    v = as.integer(as.character(x))
    return(max(1, v))
  }
  else{
    #print(x)
    bounds= as.integer(as.character(c( gsub('-.*','', x), gsub('.*-','', x))))
    val = sample.int(bounds[2]-bounds[1]+1, 1)+bounds[1]-1
    return(val)
  }
}

resample_data <- function(data=treats, size=nrow(data), interval_column='trick_or_treat_age_end') {
  data = sample_n(data, size=size, weight=data$weights, replace=TRUE)
  if (!is.null(interval_column)){
    data[,interval_column] = sapply(data[,interval_column],modify_interval)
  }
  return(data)
}


make_fit_frame <- function(data=halloween, variable='trick_or_treat_age_end', scale=FALSE){

  for (interval_column in c('trick_or_treat_age_start','trick_or_treat_age_end'))
    data=resample_data(data=data, interval_column=interval_column)
  
  n_initial = nrow(data)
  
  data = data %>% filter(trick_or_treat_age_start != -999)
  
  n_final = nrow(data)
  proportion_celebrated = n_final/n_initial
  
  survival_obj = Surv(time=as.integer(as.character(data[,variable])),
                      event=1-data$right_censored
  )
  
  
  #print(min(data$trick_or_treat_age_start))
  fit = survfit(survival_obj ~ 1, data=data)
  #print(summary(fit))
  fit_data = with(summary(fit),data.frame(survival=surv, age=time, upper=upper, lower=lower, std.err=std.err))
  
  #fill until first year
  first_age = fit_data[1, 'age']
  #print(first_age)
  t = first_age-1
  while (t < first_age && t > 0){
    fit_data = rbind(
      data.frame(survival=1, age=t, upper=1, lower=1, std.err=0), 
      fit_data
    )
    t = t -1
  }
  
  #fill in missing gaps
  i=2
  while (i <= nrow(fit_data)){
    if (fit_data[i,'age'] != fit_data[i-1, 'age'] + 1){
      fit_data = rbind(fit_data[1:(i-1),], fit_data[i-1,], fit_data[i:nrow(fit_data),])
      fit_data[i, 'age'] = fit_data[i-1, 'age'] + 1
      #print(nrow(fit_data))
      if (nrow(fit_data)  > 50){
        print('ERR')
        return(fit_data)
      }
    }
    else{
      i = i + 1
    }
  }
  
  # extend to maximum
  while (i < 47){
    fit_data = rbind(fit_data, tail(fit_data, 1))
    fit_data[i, 'age'] = i
    i=i+1
  }
  
  #rownames are stupid
  rownames(fit_data)=NULL
  
  # scales the survival up 
  if (scale){
    fit_data = fit_data %>% mutate(survival=(1-proportion_celebrated) + proportion_celebrated*survival)
    
  }
  
  # calculate hazard function
  fit_data$diff = c(0, diff(fit_data$survival))
  # fix hazard function
  fit_data$hazard = -fit_data$diff/c(1, fit_data$survival[1:(nrow(fit_data)-1)])
  
  
  return(fit_data)
  
}

# start/stop distribution function


#trials = replicate(2000, make_fit_frame(data=treats), simplify=FALSE)
#save(trials,file='trials.RData')
load('trials.RData')
trial_matrices = list()

for (i in 1:2000){
  rownames(trials[[i]])=NULL
  trial_matrices[[i]] = as.matrix(trials[[i]])
}

inf.rm <- function(x, ceiling=1) ifelse(is.infinite(x), ceiling, x)

# these calculate the mean and standard deviations of the 
# above matrices, which are decent approximations of actual standard error
meanmat = as.data.frame(apply(simplify2array(trial_matrices), 1:2, median, na.rm=TRUE))
sdmat = as.data.frame(apply(simplify2array(trial_matrices), 1:2, sd))

qLmat =  as.data.frame(apply(simplify2array(trial_matrices), 1:2, quantile, probs=0.025, na.rm=TRUE))
qUmat = as.data.frame(apply(simplify2array(trial_matrices), 1:2, quantile,probs= 0.975, na.rm=TRUE))
qMaxmat = as.data.frame(apply(simplify2array(trial_matrices), 1:2, max, na.rm=TRUE))

meanmat$survival.se = sdmat$survival
meanmat$diff = c(0, diff(meanmat$survival))
meanmat$hazard.se = sdmat$hazard
meanmat$survival.upper = inf.rm(qUmat$survival)
meanmat$survival.lower = qLmat$survival
meanmat$hazard.upper = inf.rm(qUmat$hazard)
meanmat$hazard.lower = qLmat$hazard

# note peaks at age 8, age 12, age 15, age 19, 

ggplot(meanmat %>% mutate(age=age+1)) + 
  geom_bar(aes(x=age,y=survival), width=1, stat='identity', fill='#222222') +
  geom_rect(aes(xmin=age-0.5,xmax=age+0.5,ymin=survival.lower, ymax=survival.upper),
            fill='#EE881166') + 
  scale_y_continuous(label=percent) + 
  xlab('Age') + 
  ylab('Probability of age being last time one goes trick-or-treating\n (survival function)') + 
  ggtitle('"What ages do people stop going trick or treating?" survival curve',
          subtitle='transparent orange bars=95% confidence interval\nsource: maxcandocia.com/link/trick-or-treating') + 
  geom_label(data=meanmat %>% filter(age %in% c(8, 12, 15, 19))  %>% mutate(age=age+1), 
            aes(x=age, y=survival, 
            label=sprintf('Age %s \n %d%%', age, round(survival*100)
             )
            ),
            color='#EE3344',
            size=5,
            alpha=0.5
  ) + better_text_size_manylabs


ggplot(meanmat %>% filter(age < 25) %>% mutate(age=age+1)) + 
  geom_bar(aes(x=age, y=hazard), width=1, stat='identity') + 
  ggtitle('Probability of someone stopping going trick-or-treating that year if they went previous year\n(trick-or-treating hazard curve)',
          subtitle='transparent orange bars=95% confidence interval\nsource: maxcandocia.com/link/trick-or-treating') + 
  scale_y_continuous(label=percent) +
  geom_rect(aes(xmin=age-0.5,xmax=age+0.5,ymin=hazard.lower, ymax=hazard.upper),
            fill='#EE881166') + 
  geom_label(data = meanmat %>% filter(age %in% c(8,12,15,16,17,18, 19, 22)) %>% mutate(age=age+1),
             aes(x=age,y=hazard, 
                 label=sprintf('Age %d \n %d%%', age, round(100*hazard))),
             color='#EE3344',
             size=5,
             alpha=0.5
             ) +
  ylab('Probability of not going trick or treating anymore if went previous year\n (hazard function)') +
  better_text_size_manylabs



## NOW FOR HALLOWEEN START TIME


#trials_start = replicate(2000, make_fit_frame(variable='trick_or_treat_age_start', scale=TRUE), simplify=FALSE)
trial_start_matrices = list()
#save(trials_start, file='trials_start.RData')
load('trials_start.RData')

for (i in 1:2000){
  rownames(trials_start[[i]])=NULL
  trial_start_matrices[[i]] = as.matrix(trials_start[[i]])
}



# these calculate the mean and standard deviations of the 
# above matrices, which are decent approximations of actual standard error
meanmat_start = as.data.frame(apply(simplify2array(trial_start_matrices), 1:2, median, na.rm=TRUE))
sdmat_start = as.data.frame(apply(simplify2array(trial_start_matrices), 1:2, sd))

qLmat_start =  as.data.frame(apply(simplify2array(trial_start_matrices), 1:2, quantile, probs=0.025, na.rm=TRUE))
qUmat_start = as.data.frame(apply(simplify2array(trial_start_matrices), 1:2, quantile, probs=0.975, na.rm=TRUE))

meanmat_start$survival.se = sdmat_start$survival
meanmat_start$diff = c(0, diff(meanmat_start$survival))
meanmat_start$survival.upper = inf.rm(qUmat_start$survival)
meanmat_start$survival.lower = qLmat_start$survival
meanmat_start$hazard.upper = inf.rm(qUmat_start$hazard)
meanmat_start$hazard.lower = qLmat_start$hazard

# note peaks at age 8, age 12, age 15, age 19, 

ggplot(meanmat_start %>% mutate(age=age) %>% filter(age < 16)) + 
  geom_bar(aes(x=age,y=1-survival), width=1, stat='identity', fill='#222222') +
  geom_rect(aes(xmin=age-0.5,xmax=age+0.5,ymax=1-survival.lower, ymin=1-survival.upper),
            fill='#EE881166') + 
  scale_y_continuous(label=percent) + 
  xlab('Age') + 
  ylab('Probability of trick or treating \n (1-survival function)') + 
  ggtitle('By what ages have kids tarted going trick or treating?',
          subtitle='transparent orange bars=95% confidence interval\nsource: maxcandocia.com/link/trick-or-treating') + 
  geom_label(data=meanmat_start %>% filter(age %in% c(3,4,5,6,7,8,10))  %>% mutate(age=age), 
             aes(x=age, y=1-survival, 
                 label=sprintf('Age %s \n %d%%', age, round(100-survival*100)
                 )
             ),
             color='#EE3344',
             size=5,
             alpha=0.5
  ) + better_text_size_manylabs


ggplot(meanmat_start %>% filter(age < 23) %>% mutate(age=age)) + 
  geom_bar(aes(x=age, y=hazard), width=1, stat='identity') + 
  ggtitle('Probability of someone starting going trick-or-treating that year if they didn\'t go previous year\n(trick-or-treating "hazard curve")',
          subtitle='transparent orange bars=95% confidence interval\nsource: maxcandocia.com/link/trick-or-treating') + 
          scale_y_continuous(label=percent) + 
  geom_rect(aes(xmin=age-0.5,xmax=age+0.5,ymin=hazard.lower, ymax=hazard.upper),
            fill='#EE881166') + 
          geom_label(data = meanmat_start %>% filter(age %in% c(4,6,8,10,12)) %>% mutate(age=age),
          aes(x=age,y=hazard, 
          label=sprintf('Age %d \n %d%%', age, round(100*hazard))),
          color='#EE3344',
          size=5,
          alpha=0.5
          ) +
  ylab('Probability of going trick or treating if didn\'t go previous year\n (hazard function)') 


### MAKE FINAL DISTRIBUTION



simulate_age_frequency_curve <- function(data=halloween){
  # columns = age, count, proportion
  age_matrix = matrix(0, nrow=46, ncol=3)
  
  
  
  for (interval_column in c('trick_or_treat_age_start','trick_or_treat_age_end'))
    data=resample_data(data=data, interval_column=interval_column)
  
  # -999 is the NA substitute value
  filtered_data = data %>% filter(trick_or_treat_age_start != -999)
  
  # used to get accurate idea of proportion of population celebrating
  proportion=nrow(filtered_data)/nrow(data)
  print(sprintf('%s %s', proportion, nrow(filtered_data)))
  
  # remove results of oddly sampled data (overlapping intervals) and make age end true failure time
  # also use t1 and t2 for easier coding
  filtered_data = filtered_data %>% filter(trick_or_treat_age_start <= trick_or_treat_age_end) %>%
    mutate(trick_or_treat_age_end=trick_or_treat_age_end+1, 
           t1=trick_or_treat_age_start, t2=trick_or_treat_age_end)
  
  filtered_data$will_be_censored = filtered_data$still_trick_or_treats=='Yes'
  filtered_data$censored=FALSE
  filtered_data$active=TRUE
  current_popsize = nrow(filtered_data)
  censored_popsize= 0
  for (age in 1:35){
    filtered_data$active = with(filtered_data, (!will_be_censored) | (t2 > age))
    pos_count = with(filtered_data %>% filter(active), sum((t1 <= age) & (t2 > age)))
    tot_count = nrow(filtered_data %>% filter(active))
    age_matrix[age,] = c(age, pos_count, pos_count/tot_count * proportion)
  }
  return(age_matrix)
}

age_frequency_matrices = replicate(2000, simulate_age_frequency_curve(), simplify=FALSE)
# save bc it takes forever to churn out simulations
save(age_frequency_matrices, file='age_frequency_matrices.RData')
load('age_frequency_matrices.RData')
       
age_medians = as.data.frame(apply(simplify2array(age_frequency_matrices), 1:2, median, na.rm=TRUE))
age_lower_mat =  as.data.frame(apply(simplify2array(age_frequency_matrices), 1:2, quantile, probs=0.025, na.rm=TRUE))
age_upper_mat = as.data.frame(apply(simplify2array(age_frequency_matrices), 1:2, quantile,probs= 0.975, na.rm=TRUE))

age_medians = age_medians %>% transmute(age=V1, count=V2, prop=V3) %>%
  mutate(prop_upper=age_upper_mat$V3, prop_lower=age_lower_mat$V3)


ggplot(age_medians) +
  geom_bar(aes(x=age,y=prop), stat='identity', width=1, fill='#222222') +
  geom_rect(aes(xmin=age-0.5, xmax=age+0.5, ymin=prop_lower, ymax=prop_upper), 
            fill='#EE881166') + 
  xlab('Age') +ylab('Probability of going trick-or-treating (for yourself/with friends)') + 
  scale_y_continuous(label=percent) + 
  ggtitle('Trick-or-treating frequency by age',
          subtitle='transparent orange bars=95% confidence interval\nsource: maxcandocia.com/link/trick-or-treating') + 
  better_text_size_manylabs + 
  geom_label(data = age_medians %>% filter(age %in% c(2,4,6,10,14,16,18)),
             aes(x=age,y=prop, 
                 label=sprintf('Age %d \n %d%%', age, round(100*prop))),
             color='#EE3344',
             size=5,
             alpha=0.5
  )
       

  