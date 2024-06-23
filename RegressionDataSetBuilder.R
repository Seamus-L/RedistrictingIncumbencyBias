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


for (i in 1:nrow(df_42Data)) {
  #Find which index corresponds to each polling place
    row_to_find = which(df_table11_41$Electoral.District.Number.Numero.de.circonscription == df_42Data$Riding41[i])

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


#######################################################
#Now include previous poll results to see if they predict turnout now

#First, import results 41
df_41Data <- read.csv("41Results.csv")

#Now, remove any entries that have NA entries in their turnout
df_41Data<- df_41Data %>% filter(TurnoutRel != "#N/A")


#Now need to map polling places from 41 to 42 by name of polling place
#Import polling station list 41 and 42
df_41Loc <- read.csv("41ResultsRAW/41PollLocations.csv", header = TRUE, sep = ",")

df_42Loc <- read.csv("42ResultsRAW/42PollLocations.csv", header = TRUE, sep = ",")


#For matching purposes, use site name, concatinated with province abbreviation
df_41Loc$Address = paste(df_41Loc$Site.name..EN...Nom.du.site..AN.,df_41Loc$Municipality..EN.,df_41Loc$Province)

df_42Loc$Address = paste(df_42Loc$Site.name..EN...Nom.du.site..AN.,df_42Loc$Municipality..EN.,df_42Loc$Province)

#Now load dictionary files that associate stations with places
df_41Dict = read.csv("41Dictionary.csv", header = TRUE, sep = ",")

df_42Dict = read.csv("42Dictionary.csv", header = TRUE, sep = ",")

#Reformat station number column to align formatting with poll result files
Reformat_PDSV <- function(x) {
  # Remove leading zeros before hyphen
  x <- gsub("^(0+)(\\d+-)", "\\2", x)
  # Remove "-0"
  x <- gsub("-0", "", x)
  # Remove hyphen and 0 when middle digit is 0
  x <- gsub("-(0)(-)", "\\2", x)
  # Remove hyphen before letters (A, B, ...)
  x <- gsub("-([A-Za-z])$", "\\1", x)
  return(x)
}


#Reformat poll stations for matching
df_41Loc <- df_41Loc %>%
  mutate(PD.SV = Reformat_PDSV(PD.SV))

df_42Loc <- df_42Loc %>%
mutate(PD.SV = Reformat_PDSV(PD.SV))

#Finally, concatinate riding no. and PD.SV for matching
#Adjust electoral district column to just contain district number
df_42Loc <- df_42Loc %>%
  mutate(Riding = gsub("-.*", "", Riding)) %>% mutate(Riding = trimws(Riding))

df_42Loc$Match <- paste(df_42Loc$Riding,df_42Loc$PD.SV)

df_41Loc$Match <- paste(df_41Loc$Riding,df_41Loc$PD.SV)


#Drop duplicate places, as we only need 1 station to map addresses
df_41Dict <- df_41Dict %>% distinct(Place, .keep_all = TRUE)

df_42Dict <- df_42Dict %>% distinct(Place, .keep_all = TRUE)

#Initialise new column that contains address
df_41Dict$Address = 0
df_42Dict$Address = 0

for (i in 1:nrow(df_41Dict)){ #Iterate across dictionary to assign address to each polling place
    row_to_find = which(df_41Loc$Match == df_41Dict$Station[i])
    df_41Dict$Address[i] = df_41Loc$Address[row_to_find]
}

for (i in 1:nrow(df_42Dict)){ #Iterate across dictionary to assign address to each polling place
    row_to_find = which(df_42Loc$Match == df_42Dict$Station[i])
    df_42Dict$Address[i] = df_42Loc$Address[row_to_find]
}


#Now, find which place from 41 election matches to place from 42 election
df_42Dict$Match41 = 0 #First, initialize column

for (i in 1:nrow(df_42Dict)){ #Iterate across dictionary to assign address to each polling place
    print(i)
    row_to_find = which(df_41Dict$Address == df_42Dict$Address[i])
      if (length(row_to_find) == 0){
      } else {
      df_42Dict$Match41[i] = df_41Dict$Place[row_to_find]
      }
}

#Now drop all rows with 0 in Match41
df_42Dict = df_42Dict %>% filter(Match41 != 0)

#Now, find the incumbent vote share FUCK I can't do that with my data because it only captures the vote share for the person who won the previous election, so if someone was moved to a riding where the incumbent party changed then it won't work.