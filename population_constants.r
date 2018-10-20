## contains constants for raking

total_population = 325.7e6

race_population = list(
  'White'=0.766*total_population,
  'Black/African American'=0.134*total_population,
  'American Indian/Alaska Native'=0.013*total_population,
  'Asian'=0.058*total_population,
  'Native Hawaiian/Other Pacific Islander'=0.002*total_population,
  'Multiple Races'=0.027*total_population,
  'Other'=0.062*total_population # from 2010 census (others are current estimates)
)

hispanic_population = list(
  'Yes'=total_population*0.171,
  'No'=total_population*(1-0.171)
)

gender_population = list(Female=1, Male=0.97, Other=0.014)

trump_support_population = list()

# https://www.census.gov/prod/cen2010/briefs/c2010br-03.pdf
age_group_population = list(
  '18-24'=30.67e6,
  '25-34'=42.9e6,
  '35-44'=41.06e6,
  '45-54'=45e6,
  '55+'=76.7e6
)

# regions using https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population
southwest = list(
  'Texas'=28.3e6,
  'Arizona'=7.02e6,
  'Oklaholma'=3.93e6
)

southeast = list(
  'Florida'=20.96e6,
  'Georgia'=10.4e6,
  'North Carolina'=10.27e6,
  'Virginia'=8.47e6,
  'Tennessee'=6.72e6,
  'Missouri'=6.11e6,
  'South Carolina'=5.02e6,
  'Alabama'=4.87e6,
  'Louisiana'=4.68e6,
  'Kentucky'=4.45e6,
  'Arkansas'=3.00e6,
  'Mississippi'=3.98e6,
  'Kansas'=2.91e6,
  'West Virginia'=1.82e6,
  'District of Columbia'=693e5
  
)

west = list(
  'California'=39.5e6,
  'Washington'=7.41e6,
  'Colorado'=5.61e6,
  'Oregon'=4.14e6,
  'Utah'=3.10e6,
  'Nevada'=3.00e6,
  'New Mexico'=2.09e6,
  'Idaho'=1.72e6,
  'Hawaii'=1.43e6,
  'Montana'=1.05e6,
  'Alaska'=740e5,
  'Wyoming'=579e5
  
)

midwest = list(
  'Illinois'=12.8e6,
  'Ohio'=11.66e6,
  'Michigan'=9.96e6,
  'Indiana'=6.67e6,
  'Wisconsin'=5.80e6,
  'Minnesota'=5.58e6,
  'Iowa'=3.15e6,
  'Nebraska'=1.92e6,
  'South Dakota'=870e5,
  'North Dakota'=755e5
)

northeast = list(
  'New York'=19.85e6,
  'Pennsylvania' = 12.8e6,
  'New Jersey'=9e6,
  'Massachusetts'=6.86e6,
  'Maryland'=6.05e6,
  'Connecticut'=3.59e6,
  'New Hampshire'=1.34e6,
  'Maine'=1.34e6,
  'Rhode Island'=1.06e6,
  'Delaware'=962e5,
  'Vermont'=623e5
)



#unused for this data
territory=list(
  'Puerto Rico'=3.337e6, 
  'Guam'=160e5, 
  'Virgin Islands'=106e5, 
  'American Samoa'=56e4, 
  'Northern Mariana Islands'=54e4
)

region_population = list(
  'West'=sum(unlist(west)),
  'Southwest'=sum(unlist(southwest)),
  'Southeast'=sum(unlist(southeast)),
  'Midwest'=sum(unlist(midwest)),
  'Northeast'=sum(unlist(northeast)),
  'Outside US'=9e6 # state department estimate
)

# we'll see how useful this is
# source: Gallup Polls
trump_approval_weights = list(
  'Approve'=0.41*total_population, # approve/strongly approve
  'Disapprove'=0.55*total_population, # disapprove/strongly disapprove
  'Neutral'=0.03*total_population # neutral (i.e., no opinion)
)


## these are what you want to use for raking
general_demographic_weights=list(
  gender=gender_population,
  region=region_population,
  hispanic=hispanic_population,
  race=race_population,
  age_group=age_group_population
)

political_demographic_weights=general_demographic_weights

political_demographic_weights$trump_support = trump_approval_weights