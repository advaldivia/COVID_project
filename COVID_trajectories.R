library(tidyverse)

##download data
NYT_county_data<-read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
NYT_state_data<-read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))

##cut out florida counties
states<-split(NYT_state_data,NYT_state_data$state)
mark<-c()
FL<-NYT_county_data[NYT_county_data$state=="Florida",]
FL_counties<-split(FL,FL$county)


##Calculate state new cases and date conversion


for(i in 1:55){
  raw_cases_new<-c(1:length(states[[i]]$cases))
  raw_cases_new[1]<-states[[i]]$cases[1]
  for(j in 2:length(states[[i]]$cases)){
    raw_cases_new[j]<-(states[[i]]$cases[j]-states[[i]]$cases[j-1])
  }
  states[[i]]<-mutate(states[[i]],newcases=raw_cases_new)}

for(i in 1:55){
  raw_deaths_new<-c(1:length(states[[i]]$deaths))
  raw_deaths_new[1]<-states[[i]]$deaths[1]
  for(j in 2:length(states[[i]]$deaths)){
    raw_deaths_new[j]<-(states[[i]]$deaths[j]-states[[i]]$deaths[j-1])
  }
  states[[i]]<-mutate(states[[i]],newdeaths=raw_deaths_new)}


for(i in 1:length(states)){
  states[[i]]$date <- as.Date(states[[i]]$date, format= "%Y-%m-%d")
}


##Bind into a single data frame
Statesinone<-states[[1]]
for(i in 2:length(states)){
  sec<-states[[i]] 
  Statesinone<-rbind(Statesinone,sec)
}

##First download state population data and clean
library(rvest)
h<-read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population")
tabs<-html_nodes(h,"table")
states_pop<-html_table(h, fill=TRUE)[[1]][,3:4]
states_pop<-states_pop[2:53,] 
names(states_pop)[2]<-"population"
states_pop$population<-states_pop$population %>% parse_number()

##Probably not the best way but it works.  Adds cases and deaths normed to per 10,000 in population.
Statesinone %>% mutate(pop=0)
for(i in 1:nrow(Statesinone)){Statesinone$pop[i]<-filter(states_pop,states_pop$State==Statesinone$state[i])[1,2]}

Statesinone<-Statesinone %>% mutate(cases_normed=cases/pop*10000)
Statesinone<-Statesinone %>% mutate(newcases_normed=newcases/pop*10000)

Statesinone<-Statesinone %>% mutate(deaths_normed=deaths/pop*10000)
Statesinone<-Statesinone %>% mutate(newdeaths_normed=newdeaths/pop*10000)
## clean out unneeded stuff
Statesinone<-Statesinone[,-c(3)]

## Clean up NA and negative values
for(i in 1:nrow(Statesinone)){if(Statesinone$newcases_normed[i]<=0 | is.na(Statesinone$newcases_normed[i])){
  Statesinone$newcases_normed[i]<-0}
}
for(i in 1:nrow(Statesinone)){if(Statesinone$newdeaths_normed[i]<=0 | is.na(Statesinone$newdeaths_normed[i])){
  Statesinone$newdeahs_normed[i]<-0}
}

##rename new and improved + format dates
state_data<-Statesinone%>% filter(date>="2020-03-15")

##To compare similar virus trajectories we consider the cross correlation 
##The maximum up to a lag of 20 is recorded.
cor_matrix<-matrix(0,55,55)

for(i in 1:55){
  for(j in 1:55){
    c<-ccf(filter(state_data,as.numeric(state)==i)$newcases_normed,filter(state_data,as.numeric(state)==j)$newcases_normed,pl=FALSE)
    cor_matrix[j,i]<-max(c[[1]][,,1])
  }
}
##clean
for(i in 1:55){
  for(j in 1:55){
    if(is.na(cor_matrix[i,j])){cor_matrix[i,j]<-0}
  }
}
##High correlation should mean low distance
cor_matrix<-matrix(1,55,55)-cor_matrix

##cluster based on ccf max values

clust<-kmeans(cor_matrix,6)

##There seem to be clear similarities and some geographic information.
for(i in 1:6){
  print(state_data %>% filter(as.numeric(state) %in% which(clust$cluster==i)) %>% ggplot(aes(date,newcases_normed,color=state))+geom_point())
}


