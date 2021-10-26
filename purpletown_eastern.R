setwd("/users/caity/Desktop/Research Assistant Files - Purpletown")
eleceastern <- read.csv("election_eastern.csv")
library(stringr)
state_columns <- read.csv("state columns.csv")

##To create a substring of the GEOID, use these two lines:
eleceastern$GEOSTATE <- substr(eleceastern$GEOID,1,2)
eleceastern$GEOCOUNTY <- substr(eleceastern$GEOID,3,5)
##All of these codes will remove trailing letters, periods, commas, “ward”, “voting precinct”, etc.
eleceastern$name <- gsub("[0-9]|[-)(]","",eleceastern$GEOID)
eleceastern$name <- str_trim(eleceastern$name,"both")
eleceastern$name <- str_to_title(eleceastern$name)
eleceastern$name <- gsub("\\bWard\\b","",eleceastern$name) 
eleceastern$name <- gsub("Voting District","",eleceastern$name,ignore.case=T) 
eleceastern$name <- gsub("Precinct","",eleceastern$name,ignore.case=T)
eleceastern$name <- gsub("\\b[A-Z]\\b","",eleceastern$name)
eleceastern$name <- gsub("\\b[a-z]\\b", "", eleceastern$name)
eleceastern$name <- gsub("\\b[a-z]", "", eleceastern$name)
eleceastern$name <- gsub("\\b[A-Z][A-Z]\\b","",eleceastern$name)
eleceastern$name <- gsub(",","",eleceastern$name)
eleceastern$name <- gsub("\\b[.]","",eleceastern$name)
eleceastern$name <- str_trim(eleceastern$name,"both")
##This will make it a title, so instead of MINNEAPOLIS it would say Minneapolis.
eleceastern$name <- str_to_title(eleceastern$name)
##This creates the final column which has the two state digits, three middle digits, and end name if it has one. 
eleceastern$final <- paste(eleceastern$GEOSTATE,eleceastern$GEOCOUNTY,eleceastern$name,sep=" ")

##This for loop goes through and makes a new column that says the name of the state.
eleceastern$GEOSTATE <- as.numeric(eleceastern$GEOSTATE)
eleceastern$stateName<- rep(NA, nrow(eleceastern))
for(i in 1:nrow(eleceastern)){
  for(state in 1:nrow(state_columns)){
    if(eleceastern$GEOSTATE[i]== state_columns$state_num[state]){
      eleceastern$stateName[i]<- state_columns$state_name[state]
    }
  }
}
##This code allows for us to sum together the votes of democratic votes, republican votes, and total votes.

eleceastern<-(eleceastern %>% group_by(final) 
               %>% mutate(mun_dem_votes = sum(votes_dem)))
eleceastern<-(eleceastern %>% group_by(eleceastern$final) 
               %>% mutate(mun_rep_votes = sum(votes_rep)))
eleceastern<-(eleceastern %>% group_by(eleceastern$final) 
               %>% mutate(mun_total_votes = sum(votes_total)))
##To get the percentages

##This is how I can see what states are included. 
table(eleceastern$stateName)
##This creates a subset of the data for me, that removes the states that have little to no names to identify .
eleceastern_subset<-subset(eleceastern, stateName==c('Massachusetts','New Hampshire', 'New Jersey', 
                                                      'South Carolina', 'Pennsylvania', 
                                                      'Rhode Island', 'Vermont', 'Ohio'))
##Then, this creates a csv file of the new table we have made through the manipulations above. 
write.csv(eleceastern, "election_eastern_purpletown.csv")
