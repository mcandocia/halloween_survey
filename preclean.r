# this is the file that does the most preliminary of cleaning
# data processed BY file will not be publicly available, apart from final output
library(plyr)
library(dplyr)

data_list = list()

# load
directories = paste0('Halloween Questionnaire - ', c('EM','FB','LI','RD'),'.csv')

for (directory in directories){
  data_list[[directory]] = read.csv(paste0(directory,'/',directory), colClasses='character')
}

# combine
combined_raw = bind_rows(data_list)
combined_raw = combined_raw[(!duplicated(combined_raw$What.is.your.email.address.) & 
                              !duplicated(combined_raw$What.is.your.email.address., fromLast=TRUE) ) |
                              combined_raw$What.is.your.email.address.=='',]

# select winner on HALLOWEEN
contestants = combined_raw %>% filter(What.is.your.email.address. != '' & What.is.your.age.group. != "Under 18")

# No. 1 gets $13, No. 2 gets $6.66
WINNERS = sample(contestants$What.is.your.email.address., 2)



# randomize and remove unneeded columns
filtered_raw = combined_raw
filtered_raw$Timestamp = NULL
filtered_raw$What.is.your.email.address. = NULL

randomized_raw = filtered_raw[sample(nrow(combined_raw)),]

write.csv(randomized_raw, 'halloween_raw.csv', row.names=FALSE)
