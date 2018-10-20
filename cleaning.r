library(plyr)
library(dplyr)
library(qdapTools)
source('raking.r')
source('population_constants.r')

survey = read.csv('halloween_raw.csv', colClasses='character')


duplicate_columns = names(survey)[names(survey) %in% paste0(names(survey),'.1')]

is_na_or_blank <- function(x){
  is.na(x) | x==''
}

#give each row in the survey a unique ID
survey$id = 1:nrow(survey)

survey = survey %>% rename(
  id=id,
  celebrated=Have.you.ever.celebrated.Halloween.,
  haunted_houses=Did.you.ever.go.to.haunted.houses...,
  reason_no_trick_or_treat=Why.didn.t.you.go.trick.or.treating.,
  wish_trick_or_treat=Do.you.wish.you.had.gone.trick.or.treating.,
  trick_or_treat_age_start=How.old.were.you.when.you.started.going.trick.or.treating.,
  trick_or_treat_age_end=How.old.were.you.during.your.last.year.of.going.trick.or.treating.,
  still_trick_or_treats=Do.you.still.go.trick.or.treating.for.yourself.or.with.friends.,
  still_trick_or_treats2=Do.you.still.go.trick.or.treating.for.yourself.with.friends., # need to merge this with row above
  favorite_candy=What.was.your.favorite.candy.,
  worst_candy=What.was.your.least.favorite.candy.,
  three_words=Describe.Halloween.in.three.words,
  gender=What.is.your.gender.,
  age_group=What.is.your.age.group.,
  why_no_childhood_halloween=Why.didn.t.you.celebrate.Halloween.as.a.kid.,
  halloween_party=Each.year..do.you.usually.attend.a.Halloween.party.,
  race_multi=What.is.your.race.,
  hispanic=Are.you.Hispanic.and.or.Latino.a,
  region=Which.region.of.the.US.do.you.live.in.,
  birthday_relativity=As.a.child..was.your.birthday.between.the.start..of.school.and.Halloween..or.after.Halloween.
)

# rename costume columns
survey_colnames = names(survey)
costume_colnames = survey_colnames[grepl('Which.of.the.following.costumes.', survey_colnames)]
new_costume_colnames = gsub('Which.*\\.{3}','costume_', costume_colnames)

# not sure exactly how this works, but it sure is succint
survey = survey %>%  rename_at(vars(costume_colnames), ~new_costume_colnames)


# fix race classes
survey$race = ifelse(
  grepl(';', survey$race_multi), 'Multiple Races', survey$race_multi
)

# merge the two trick or treating columsn back
# note "" = question wasn't added yet, so it's unknown
survey$still_trick_or_treats = ifelse(
  !is.na(survey$still_trick_or_treats),
  survey$still_trick_or_treats,
  survey$still_trick_or_treats2
)
survey$still_trick_or_treats2 = NULL


# weight survey
survey_weighted = survey %>%
  mutate(
    weight=rake_data(
      survey,
      names(general_demographic_weights),
      general_demographic_weights,
      max_iter=25
    )
  )



# split survey columns into groups
split_columns <- function(data, column, sep=';'){
  new_columns = mtabulate(lapply( strsplit(data[,column], sep),
                                  function(x) paste0(column,'_',x))
  )
  return(cbind(data, new_columns))
}

columns_to_split = c(new_costume_colnames)

split_survey = survey_weighted
for (survey_colname in columns_to_split){
  split_survey = split_columns(split_survey, survey_colname)
}

# these represent any matches
split_column_names = names(split_survey)[grepl('costume_.+_.+', names(split_survey))]

# these represent non-matches
questionable_split_column_names = names(split_survey)[grepl('costume_.+_$', names(split_survey))]
non_split_costume_names = names(split_survey)[grepl('costume_[^_]+$', names(split_survey))]

costume_frame_proto = split_survey[,!names(split_survey) %in% c(non_split_costume_names, questionable_split_column_names)] %>% gather_category(
  'costume_'
)

costume_frame = costume_frame_proto

# extract both the time and type of costume so that they can be used to summarize if needed
costume_frame$costume_type = gsub(' ?_.+$','', costume_frame$costume)
costume_frame$costume_time = gsub('.+_','', costume_frame$costume)

write.csv(costume_frame, 'costumes.csv', row.names=FALSE)
write.csv(survey_weighted, 'halloween.csv', row.names=FALSE)

