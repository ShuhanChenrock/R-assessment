library(dplyr)
library(tidyverse)
library(psych)
library(ggplot2)
urlfile="https://raw.githubusercontent.com/ShuhanChenrock/R-assessment/master/test_data.csv"
team_data<-read_csv(url(urlfile))
print(team_data)

#Q1
recoded_data <-team_data %>%
  select(id,team_cohesion_1,team.cohesion_2,team.cohesion_3_r,team.cohesion_4,team_performance,task_interdepdence_1,task_interdepdence_2_r,task_interdepdence_3) %>%
  mutate(team.cohesion_3_r =recode(team.cohesion_3_r, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1)) %>%
  mutate(task_interdepdence_2_r =recode(task_interdepdence_2_r,'1'=5,'2'=4,'3'=3,'4'=2,'5'=1 )) %>%
  na.omit() 
print(recoded_data)
#Q2
composite_scale_cohesion <-aggregate(
  cbind(team_cohesion_1,team.cohesion_2,team.cohesion_3_r,team.cohesion_4)~id,
  data = recoded_data, FUN=mean
)
composite_scale_task <-aggregate(
  cbind(task_interdepdence_1,task_interdepdence_2_r,task_interdepdence_3)~id,
  data=recoded_data, FUN=mean
)

#Q3
team_cohesion<-
  rowSums(
  select(recoded_data,team_cohesion_1,team.cohesion_2,team.cohesion_3_r,team.cohesion_4)
  )
task_interdepdence<-
  rowSums(
    select(recoded_data,task_interdepdence_1,task_interdepdence_2_r,task_interdepdence_3)
  )
team_performance<-select(recoded_data,team_performance)
df <-tibble(team_cohesion,task_interdepdence,team_performance)
  
describe(df)
correlation <- cor(df)
#Q4
model<- lm(
  team_performance~team_cohesion+task_interdepdence+team_cohesion*task_interdepdence,
  data=df)
#Q5
ggplot(
  data=recoded_data,aes(x=team_cohesion,y=task_interdepdence)
  )+geom_point(size=2,shape=23)+geom_smooth(method=lm)
