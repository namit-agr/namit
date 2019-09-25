library(ggplot2)
library(dplyr)
library(corrplot)

load("C:/easy_r/brfss.gz")

#PART2 : Reserch questions
## Research quesion 1: First of all I am interested in overall health conditions of the U.S.

## Research quesion 2: How is sickness affected by healthcare access?

## Research quesion 3: I am quite interested in heart strokes

#PART3 : EDA

# ggplot2 function for heatmapping health status 
all_states = map_data("state") 
health_states = brfss2013 %>%
  filter(!is.na(genhlth)) %>%  # omit on NA values!
  select(genhlth, X_state, poorhlth)

health_states_poorhlth = health_states %>%
  group_by(genhlth, X_state) %>% 
  summarise(mean(poorhlth),n=n()) %>%
  mutate(pct = (n/sum(n))*100) 

health_states_map = health_states_poorhlth %>%
  mutate(region = tolower(X_state))

states_map = merge(all_states, health_states_map, by="region") 

ggplot() + geom_polygon(data = states_map, aes(x= long, y = lat, group = group, fill = pct), color = "white") + ggtitle("Heat map of poor health conditions in the US") + scale_fill_gradient2(low = "blue", mid = "grey", high = "darkred") + theme(legend.position = c(1, 0), legend.justification = c(1, 0)) 

# Research quesion 1 : I am using poorhlth variable to plot this heat map, which means that there is higher number of people with poor health in Florida.

# Research quesion 2 : Is it because residents of Florida do not go to medical exams? Is there healthcare accessible in Florida? Or it is expensive? 

# Are medical cost too high? - No, medical costs are not high.
plot(brfss2013 %>% 
       filter(!is.na(medcost), X_state == "Florida") %>% 
       select(medcost), main = "Medical costs too high?") 

# Are they regular smokers? - No, they are not heavy smokers.
plot(brfss2013 %>% 
       filter(X_state == "Florida") %>% 
       select(smokday2), main = "Frequency days now smoking")  

# Do they drink daily? - No, they do not drink too often.
# alcday5 : Days In Past 30 Had Alcoholic Beverage 
summary(brfss2013 %>% 
          filter(!is.na(alcday5), X_state=="Florida") %>% 
          select(alcday5)) 

# Do they have health plan? - Yes, they do have health plan.
plot(brfss2013 %>% 
       filter(X_state == "Florida") %>% 
       select(hlthpln1), main = "Does have health plan?") 

# Aren¢¥t they poor and therefore sick? - No, they are not poor.
plot(brfss2013 %>% 
       filter(X_state == "Florida") %>% 
       select(income2), main = "What are levels of income?") 

# Do they go to medical exams? - Yes, they visit doctor regularly.
plot(brfss2013 %>% 
       filter(X_state == "Florida") %>% 
       select(checkup1), main = "Do they regularly go to medical exams?") 

# Aren¢¥t they just old? 
plot(brfss2013 %>% 
       filter(X_state == "Florida") %>% 
       select(employ1), main = "Aren¢¥t they just old?") 

# what about join pain? 
summary(brfss2013 %>% 
          filter(!is.na(joinpain), X_state == "Florida") %>%
          select(joinpain)) 

# and what about cholesterol? - Lot of blood cholesterol
plot(brfss2013 %>% 
       filter(X_state == "Florida") %>% 
       select(toldhi2), main = "Blood cholesterol") 

# Are retired people having bigger issue with blood cholesterol? 

plot(brfss2013 %>% 
       filter(X_state == "Florida", employ1=="Retired") %>% 
       select(toldhi2), main = "Blood cholesterol in retired group" ) 

# In summary: There are lot of elderly with blood cholesterol problems. That could be the issue with poor health condition in Florida.

# Research quesion 3: Does high blood cholesterol results in more hearthstrokes? Are heartstrokes correlated to obesity, smoking, drinking and high blood cholesterol?

heart = brfss2013 %>% 
  filter(!is.na(toldhi2), !is.na(cvdstrk3), !is.na(X_bmi5), !is.na(maxdrnks), X_state=="Florida") %>% 
  select(toldhi2,cvdstrk3,maxdrnks, X_bmi5) 

heart$toldhi2 = ifelse(heart$toldhi2=="Yes",1,0)
heart$cvdstrk3 = ifelse(heart$cvdstrk3=="Yes",1,0)
korelace = cor(heart) 
corrplot(korelace, method="ellipse") 

# This looks like there is weak correlation between high blood cholesterol and diagnosed stroke. Also it looks like there is weak correlation between high blood cholesterol and high BMI (obesity), but no correlation between obesity and diagnosed stroke. Even more! There is weak negative correlation between max drinks in past 30 days and high blood cholesterol, but very weak correlation between max drinks and obesity.

# How is sickness affected by healthcare access? Does Florida has good healthcare access or is health care too expensive? Does patients go to medical checks?

sickness = brfss2013 %>%
  filter(!is.na(hlthpln1), !is.na(medcost), !is.na(income2), !is.na(checkup1), !is.na(medicare), !is.na(poorhlth)) %>% 
  select(hlthpln1, medcost, income2, checkup1, medicare, poorhlth)

plot(sickness$income2, sickness$hlthpln1, xlab="income", ylab="healthplan") 

plot(sickness$income2, sickness$medcost, xlab="income", ylab="medcost") 

plot(sickness$income2, sickness$medicare, xlab="income", ylab="medicare") 

# It is obvious that with there is bigger probability of having health plan if one belongs to higher income level.
# Unsurprisingly the less income, the stronger feeling of expensive medical costs.
# medicare is more often in lower income levels.