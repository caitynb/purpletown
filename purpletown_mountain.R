setwd("/users/caity/Desktop/Research Assistant Files - Purpletown")
elecmountain <- read.csv("election_mountain.csv")
library(stringr)
state_columns <- read.csv("state columns.csv")



elecmountain$GEOSTATE <- substr(elecmountain$GEOID,1,2)
elecmountain$GEOCOUNTY <- substr(elecmountain$GEOID,3,5)


elecmountain$name <- gsub("[0-9]|[-)(]","",elecmountain$GEOID)
elecmountain$name <- str_trim(elecmountain$name,"both")
elecmountain$name <- str_to_title(elecmountain$name)
elecmountain$name <- gsub("\\bWard\\b","",elecmountain$name) 
elecmountain$name <- gsub("Voting District","",elecmountain$name,ignore.case=T) 
elecmountain$name <- gsub("Precinct","",elecmountain$name,ignore.case=T)
elecmountain$name <- gsub("\\b[A-Z]\\b","",elecmountain$name)
elecmountain$name <- gsub("\\b[a-z]\\b", "", elecmountain$name)
elecmountain$name <- gsub("\\b[a-z]", "", elecmountain$name)
elecmountain$name <- gsub("\\b[A-Z][A-Z]\\b","",elecmountain$name)
elecmountain$name <- gsub(",","",elecmountain$name)
elecmountain$name <- gsub("\\b[.]","",elecmountain$name)
elecmountain$name <- str_trim(elecmountain$name,"both")
elecmountain$name <- str_to_title(elecmountain$name)

elecmountain$final <- paste(elecmountain$GEOSTATE,elecmountain$GEOCOUNTY,elecmountain$name,sep=" ")


elecmountain$GEOSTATE <- as.numeric(elecmountain$GEOSTATE)
elecmountain$stateName<- rep(NA, nrow(elecmountain))
for(i in 1:nrow(elecmountain)){
  for(state in 1:nrow(state_columns)){
    if(elecmountain$GEOSTATE[i]== state_columns$state_num[state]){
      elecmountain$stateName[i]<- state_columns$state_name[state]
    }
  }
}
##check names to be same, do another for loop to go through the levels of the final column,
  ##make it a factor if levels are same, sum votes_dem, votes_rep, votes_tota 
levels(elecmountain$final)
class(elecmountain$final)
elecmountain$final <- as.factor(elecmountain$final)
elecmountain<-(elecmountain %>% group_by(elecmountain$final) 
                            %>% mutate(mun_dem_votes = sum(votes_dem)))
elecmountain<-(elecmountain %>% group_by(elecmountain$final) 
               %>% mutate(mun_rep_votes = sum(votes_rep)))
elecmountain<-(elecmountain %>% group_by(elecmountain$final) 
               %>% mutate(mun_total_votes = sum(votes_total)))
table(elecmountain$stateName)
elecmountain_subset<-subset(elecmountain, stateName== c('Utah', 'North Dakota', 'South Dakota', 'Arizona'))
write.csv(elecmountain_subset, "election_mountain_purpletown.csv")
