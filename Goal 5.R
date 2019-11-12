library(dplyr)
library(readr)
library(ggplot2)
options(scipen = 99)
dataset=read.csv("dataset.csv")
datasetc=dataset%>%mutate(ratioC=votes_dem_2016/(votes_dem_2016 + votes_gop_2016))
datasetct=datasetc %>%mutate(ratioT=votes_gop_2016/(votes_dem_2016 + votes_gop_2016))
head(datasetct)


One=datasetct%>%select(state, state_abbr, hate_crimes_per_100k_splc, `Rates.of.2016`, `Rates.of.2015`, `Rates.of.2014`, `Clinton..2016.`, `Trump..2016.` )
library(reshape2)                    
One_y <- One %>% melt(id = c("state", "state_abbr","hate_crimes_per_100k_splc", 'Rates.of.2016', 'Rates.of.2015', 'Rates.of.2014'))

Two = datasetct %>% select(state, ratioC, ratioT)
Two_y <- Two %>% melt(id = c("state"))

Final=cbind(One_y, Two_y)
colnames(Final)= c("State", "Abbr","Hate_Crimes", "Unemp2016", "Unemp2015", "Unemp2014", "Candidate", "Publicity",
                   "State2","Ratio", "Votes" )
Final=Final%>%mutate(Win = Votes > 0.5)
Final_Win=Final%>%select(State, Abbr, Candidate, Publicity, Ratio, Votes, Win)
Final_Win=na.omit(Final_Win)
head(Final_Win)

Final1= Final%>%group_by(Candidate, Win)%>%summarise(n())
colnames(Final1)=c("Candidate", "Win", "No_of_states")
Final1= na.omit(Final1)
Final1=subset(Final1, Win==TRUE)
ggplot(Final1, aes(x= Candidate,y= No_of_states, fill= Candidate))+geom_bar(stat="identity")+ggtitle("The number of states each candidate won")

ggplot(Final,aes(x=Publicity, y=Votes, color=Candidate, label=Abbr))+geom_point()+geom_text(hjust=0, vjust=0)+ylab("Votes_Ratio")+ggtitle("Relationship between publicity and votes that each candidate received in each state")
#Relationship between publicity and votes received.

Final2 <- Final %>% mutate(mean_unemp = (Unemp2016 + Unemp2015 + Unemp2014) / 3) %>% select(State,Abbr, Unemp2016, Unemp2015, Unemp2014,mean_unemp,Candidate, Votes)
Final2=na.omit(Final2)
ggplot(Final2, aes(x=reorder(State,mean_unemp), y=Votes, fill= Candidate))+geom_bar(stat="identity",position = "dodge")+coord_flip()+xlab("Mean Unemployment(from highest to lowest)")+ylab("Votes Ratio")+ggtitle("Barplot to represent  how unemployment affects votes")



Hate=Final%>%select(State, Abbr, Candidate, Hate_Crimes, Votes)
Hate=Hate%>%filter(State!="District of Columbia")
ggplot(Hate, aes(x=Hate_Crimes, y=Votes, color=Candidate, label=Abbr))+geom_point()+geom_text(hjust=0,vjust=0)+geom_smooth(method = lm, se=FALSE)+ylab("Votes_Ratio")+ggtitle("Relationship between hate crimes and the voting ratio each candidate received in each state")
