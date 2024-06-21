#This file is used to construct a data set usable for regressions
#Takes output from 42RidingAggregator, as well as past election results to generate a data set
#usable for statistical analysis
library(dplyr)

#First, read in CSV that filters ridings eligible for analysis (possible incumbents)
#These are ridings that were identified by name to match with ridings from the previous election
#As of now, these are the only ridings considered in this analysis
df_filter <- read.csv("RidingFilter.csv")


#Now, read in the aggregated data set from 42nd election
df_42Data <- read.csv("42Results.csv")


#Now, add a  new column to df_42Data that takes the riding number from the 41st election that it matches to
#First initialize new column
df_42Data$Riding41 <- 0

for (i in 1:nrow(df_42Data)) {
  #Find which index corresponds to each polling place
    row_to_find = which(df_filter$ED_CODE == df_42Data$Riding[i])

    #Assign the value of Direct.Match to the corresponding row in df_42Data
    df_42Data$Riding41[i] = df_filter$Direct.Match[row_to_find]
}

#Now, remove any entries from a riding that doesn't have a match (not considered)
df_42Data<- df_42Data %>% filter(Riding41 != "#N/A")


#Now, pair with data on elected candidates in 41st election
#This data is contained in table-11 of official election data
df_table11_41 <- read.csv("41ResultsRAW/table_tableau11.csv")

parties_listed <-c("Liberal","Conservative","NDP","Green","Bloc")
parties_tag <- c("LIB","CON","NDP","GREEN","BLOC")

#Initialise new column for elected party
df_table11_41$Elected = 0

library(stringr) #For searching strings
for (i in 1:nrow(df_table11_41)) { #Iterate across every row in this table
    for (j in 1:6) { #Check to see which party is listed in the 
        contains_test <- str_detect(df_table11_41$Elected.Candidate.Candidat.elu[i], parties_listed[j])
            if (contains_test) { #If test is true, write the party name in this column
                df_table11_41$Elected[i] = parties_tag[j]
                break #break once a match has been found
            }
    }

}
sum(df_table11_41 == 0) #Check if any were unassigned - means they were independent, or fringe party

#Append the elected party to the results of the 42 election data frame

#Initialize incumbent column in results dataframe
df_42Data$Incumbent <- 0


for (i in 1:nrow(df_table11_41)) {
  #Find which index corresponds to each polling place
    row_to_find = which(df_table11_41$Electoral.District.Number.Numero.de.circonscription == df_42Data$Riding[i])

    #Assign the value of Direct.Match to the corresponding row in df_42Data
    df_42Data$Incumbent[i] = df_table11_41$Elected[row_to_find]
}

#Now, find vote share of incumbent party
#Vote share is defined as vote for party/total turnout

#Initialize column for incumbent share
df_42Data$IncumbentShare <- 0

#Iterate across each row and assign vote share for each party
for (i in 1:nrow(df_42Data)){
    IncumbentParty = df_42Data$Incumbent[i] #Find incumbent party for each polling place
    VoteShare = df_42Data[[IncumbentParty]][i]/df_42Data$TurnoutAbs[i] #Find vote share of incumbent party at polling place
    df_42Data$IncumbentShare[i] = VoteShare #Write vote share to the resepective row
}

#Create filtered Df to run first regression - only include located polling places
df_42Data_Filtered = df_42Data %>% filter(Located != 0)

#Model 0 - basic without any additional fixed effects
model0 = lm(IncumbentShare ~ IsNew, data = df_42Data_Filtered)
summary(model0)

#Model 1 - basic without riding level fixed effects, but including party effects
model1 = lm(IncumbentShare ~ Incumbent + IsNew, data = df_42Data_Filtered)
summary(model1)

#Model 2 - includes riding level fixed effects
model2 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew, data = df_42Data_Filtered)
summary(model2)






#Filter to only include PRECISE locations
#Create filtered Df to run first regression - only include located polling places
df_42Data_Filtered_Precise = df_42Data_Filtered %>% filter(Precise == 1)

#Model 0 - basic without any additional fixed effects
model3 = lm(IncumbentShare ~ IsNew, data = df_42Data_Filtered_Precise)
summary(model3)

#Model 1 - basic without riding level fixed effects, but including party effects
model4 = lm(IncumbentShare ~ Incumbent + IsNew, data = df_42Data_Filtered_Precise)
summary(model4)

#Model 2 - includes riding level fixed effects
model5 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew, data = df_42Data_Filtered_Precise)
summary(model5)


#Need to add additional models which include vote preferences in previous elections