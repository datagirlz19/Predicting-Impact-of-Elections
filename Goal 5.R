#Eyoas,Chin Chin, Sarah 
#Goal 5 

#loading the readr dataset so that the files can be read into the program
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
options(scipen = 99)
dataset=read.csv("dataset.csv")

#datasetc is a collection of data with all of its factors. RatioC is for Clinton RatioT is for Trump to calculate an accurate vote count 
#ktable is just to show the table in the knitted document 
datasetc=dataset%>%mutate(ratioC=votes_dem_2016/(votes_dem_2016 + votes_gop_2016))
datasetct=datasetc %>%mutate(ratioT=votes_gop_2016/(votes_dem_2016 + votes_gop_2016))
datasetct=na.omit(datasetct)
kable(datasetct [1:5, ])

#One is a dataset with informtion about the canidates, qty of hatecrimes, and he publicity each canidate recieved. 
#using the melt function we are trying to concatinate them into one colum (from "wide" to "long")
One=datasetct%>%select(state, state_abbr, hate_crimes_per_100k_splc, `Rates.of.2016`, `Rates.of.2015`, `Rates.of.2014`, `Clinton..2016.`, `Trump..2016.` )
library(reshape2)                    
One_y <- One %>% melt(id = c("state", "state_abbr","hate_crimes_per_100k_splc", 'Rates.of.2016', 'Rates.of.2015', 'Rates.of.2014'))
#Two is a dataset with information about the total votes for each party.
#using melt again we are trying to concatinate the data into one colum (from "wide" to "long")
Two = datasetct %>% select(state, ratioC, ratioT)
Two_y <- Two %>% melt(id = c("state"))
#Final is a dataset with both One and Two altertered contents with the names adjusted to what the data is counting
Final=cbind(One_y, Two_y)
colnames(Final)= c("State", "Abbr","Hate_Crimes", "Unemp2016", "Unemp2015", "Unemp2014", "Candidate", "Publicity",
                   "State2","Ratio", "Votes" )

#Final_win is a table with info about the states each canidate won/lost TRUE = Win 
#the 0.5 is refering to the ratioC/T 0.5 is when there is a 50/50 split in votes. 
Final=Final%>%mutate(Win = Votes > 0.5)
Final_Win=Final %>% mutate(mean_unemp = (Unemp2016 + Unemp2015 + Unemp2014) / 3)  %>%select(State, Abbr, Hate_Crimes, mean_unemp, Publicity, Candidate, Ratio, Votes, Win) %>% arrange(State) 
Final_Win=na.omit(Final_Win)
kable(Final_Win [1:5, ])

#Graph is a bar plot to show if the canidate won the state vote or not. 
Final1= Final%>%group_by(Candidate, Win)%>%summarise(n())
colnames(Final1)=c("Candidate", "Win", "No_of_states")
Final1= na.omit(Final1)
Final1=subset(Final1, Win==TRUE)
ggplot(Final1, aes(x= Candidate,y= No_of_states, fill= Candidate))+geom_bar(stat="identity")+ggtitle("States each candidate won") + ylab("Number of States")

#graph is a scatter plot to show if the Relationship between publicity and votes received.
ggplot(Final,aes(x=Publicity, y=Votes, color=Candidate, label=Abbr))+geom_point()+geom_text(hjust=0, vjust=0)+ylab("Votes Ratio")+ggtitle("Relationship between Publicity V. Votes")

#another bar graph showing the unemployemt rates to votes from Highest at the top to lowest at the bottom. 
#to see clearly the relatonship, you have to look at frequency 
Final2 <- Final %>% mutate(mean_unemp = (Unemp2016 + Unemp2015 + Unemp2014) / 3)%>% select(State,Abbr, Unemp2016, Unemp2015, Unemp2014,mean_unemp,Candidate, Votes)
Final2=na.omit(Final2)
ggplot(Final2, aes(x=reorder(State,mean_unemp), y=Votes, fill= Candidate))+geom_bar(stat="identity",position = "dodge")+coord_flip()+xlab("Average Unemployment (from Highest to Lowest)")+ylab("Votes Ratio")+ggtitle("Unemployment Impact on Votes") +ylab("Hate Crimes")

#final graph is a scatter plot lising the ratio of Hatecrimes to votes with a line of regression. There 
#removed District of Columbia bc it is an outlier skewing the data. 
Hate=Final%>%select(State, Abbr, Candidate, Hate_Crimes, Votes)
Hate=Hate%>%filter(State!="District of Columbia")
ggplot(Hate, aes(x=Hate_Crimes, y=Votes, color=Candidate, label=Abbr))+geom_point()+geom_text(hjust=0,vjust=0)+geom_smooth(method = lm, se=FALSE)+ylab("Votes Ratio")+ggtitle("Relationship between Hate Crimes V. Votes") + xlab("Hate Crimes")



#____________________________________
library(caret)
#__________With Professor Morrison 

#Isolating Trump Speific Data, it wont have much of an impact on the data results, 
  #Trump is just our base case
  Final2 <- Final %>% filter( Candidate == "Trump..2016.")%>% mutate(MeanUnemp = (Unemp2016 + Unemp2015 + Unemp2014)/3) %>% select(Votes, Publicity, MeanUnemp, Hate_Crimes)


    #looking at the Publicity 
    ggplot(Final2, aes(x=MeanUnemp, y=Votes))+geom_point()+geom_smooth(method = "lm", se=FALSE) + ggtitle("Average Unemployment 2014-2016") + ylab("Voting Ratio") +xlab(" Average Unemployment")
    model4=lm(Votes~MeanUnemp, data=Final2)
    summary(model4)

    #Looking at Hate Crimes 
    ## Remove that outlier ONLY FOR THIS MODEL andrefit to see if there is a difference (there is)
    
    #With Outlier 
      #ggplot(Final2, aes(x=Hate_Crimes, y=Votes))+geom_point()+geom_smooth(method = "lm", se=FALSE)
      #model5=lm(Votes~Hate_Crimes, data=Final2)
      #summary(model5)
  
    #without outlier
    Final4=Final2%>%filter(Hate_Crimes<1.2)
    ggplot(Final4, aes(x=Hate_Crimes, y=Votes))+geom_point()+geom_smooth(method = "lm", se=FALSE) + ggtitle("Hate Crimes") + ylab("Voting Ratio") +xlab('Hate Crimes')
    model6=lm(Votes~Hate_Crimes, data=Final4)
    summary(model6)

    #effects of publicity on Canidates voting Ratio
    ggplot(Final2, aes(x=Publicity, y=Votes))+geom_point()+geom_smooth(method = "lm", se=FALSE) + ggtitle("Trumps's Publicity") + ylab("Voting Ratio")
    model1=lm(Votes~Publicity, data=Final2) 
    summary(model1)

#for Clinton, had to filter out her speific data since data ontains both canidates
Final3 <- Final %>% filter( Candidate == "Clinton..2016.")%>%select(Votes, Publicity)
    #looking at the effects on publicity to see if ther is any compararive diffference from Trumps
    ggplot(Final3, aes(x=Publicity, y=Votes))+geom_point()+geom_smooth(method = "lm", se=FALSE) + ggtitle("Clinton's Publicity") + ylab("Voting Ratio")
    model4=lm(Votes~Publicity, data=Final3)
    summary(model4)

#Using the Leave One Out Cross Validation to test predictions 
data_crtl<-trainControl(method = 'LOOCV')
model_caret<-train(Votes~Publicity, data=Final, trControl=data_crtl, method="lm", na.action = na.pass)
    
