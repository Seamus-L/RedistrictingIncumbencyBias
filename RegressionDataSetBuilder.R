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

#This column is the riding when matching was only approximate
df_42Data$Riding41Approx <- 0

for (i in 1:nrow(df_42Data)) {
  #Find which index corresponds to each polling place
    row_to_find = which(df_filter$ED_CODE == df_42Data$Riding[i])

    #Assign the value of Direct.Match to the corresponding row in df_42Data
    df_42Data$Riding41[i] = df_filter$Direct.Match[row_to_find]

    #Now repeat for the approx match as well
    df_42Data$Riding41Approx[i] = df_filter$Approx.Match[row_to_find]
}


# #Now, remove any entries from a riding that doesn't have a match (not considered)
# df_42Data_All <- df_42Data %>% filter(Riding41Approx != "#N/A") #This one considers approx locations as well

# df_42Data <- df_42Data %>% filter(Riding41 != "#N/A")


#Now, pair with data on elected candidates in 41st election
#This data is contained in table-11 of official election data
df_table11_41 <- read.csv("41ResultsRAW/table_tableau11.csv", encoding = "latin1", stringsAsFactors = FALSE)



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
df_42Data$IncumbentCand <- 0
df_42Data$PrevIncumbent <- 0
df_42Data$PrevIncumbentCand <- 0

for (i in 1:nrow(df_42Data)) {
  #Find which index corresponds to each polling place
  row_to_find = which(df_table11_41$Electoral.District.Number.Numero.de.circonscription == df_42Data$Riding41Approx[i])

  if (length(row_to_find) != 0) { 
  #Assign the value of Direct.Match to the corresponding row in df_42Data
  df_42Data$Incumbent[i] = df_table11_41$Elected[row_to_find]

  df_42Data$IncumbentCand[i] <- df_table11_41$Elected.Candidate.Candidat.elu[row_to_find]
  }

  #Find which index corresponds to each polling place for PREVIOUS election (will only be different form above for IsNew = 1)
  row_to_find = which(df_table11_41$Electoral.District.Number.Numero.de.circonscription == df_42Data$PrevRidingNo[i])

  if (length(row_to_find) != 0) { 
  #Assign the incumbent and incumbnet cand for OLD riding (only will be different from above if IsNew = 1)
  df_42Data$PrevIncumbent[i] = df_table11_41$Elected[row_to_find]

  df_42Data$PrevIncumbentCand[i] <- df_table11_41$Elected.Candidate.Candidat.elu[row_to_find]
  }
  
}


#Now, find vote share of incumbent party
#Vote share is defined as vote for party/total turnout

#Initialize column for incumbent share
df_42Data$IncumbentShare <- 0

#Iterate across each row and assign vote share for each party
for (i in 1:nrow(df_42Data)){
    IncumbentParty = df_42Data$Incumbent[i] #Find incumbent party for each polling place
    VoteShare = df_42Data[[IncumbentParty]][i]/df_42Data$TurnoutAbs[i] #Find vote share of incumbent party at polling place
      
    if (length(VoteShare) != 0) { 
    #Assign the value of Direct.Match to the corresponding row in df_42Data
    df_42Data$IncumbentShare[i] = VoteShare #Write vote share to the resepective row
    } 
}


#Finally, drop all NA observations (only include those at least located approximately)
df_42Data <- df_42Data %>% filter(Riding41Approx != "#N/A")



#Truncate the IncumbCand to remove party affiliation

#Function for this:
truncate_string <- function(input_string, keyword) {
  pos <- regexpr(keyword, input_string)
  if (pos != -1) {
    return(substr(input_string, 1, pos - 2))
  } else {
    return(input_string)
  }
}

#Now iterate across data frame
#Named vector for association
party_reference <- c("LIB" = "Liberal", "NDP" = "NDP", "CON" = "Conservative", "BLOC" = "Bloc", "GREEN" = "Green")

for (i in seq_len(nrow(df_42Data))) {
  # Find respective party given value in Incumbent column
  incumbent <- df_42Data$Incumbent[i]
  
  if (incumbent %in% names(party_reference)) {
    party <- party_reference[incumbent]
  } else {
    party <- NA  # Handle cases where the incumbent is not in the party reference
  }
  
  # Truncate incumbent candidate if party is found
  if (!is.na(party)) {
    df_42Data$IncumbentCand[i] <- truncate_string(df_42Data$IncumbentCand[i], party)
  }

  #Repeat for previous incumbent
  incumbent <- df_42Data$PrevIncumbent[i]
  
  if (incumbent %in% names(party_reference)) {
    party <- party_reference[incumbent]
  } else {
    party <- NA  # Handle cases where the incumbent is not in the party reference
  }
  
  # Truncate incumbent candidate if party is found
  if (!is.na(party)) {
    df_42Data$PrevIncumbentCand[i] <- truncate_string(df_42Data$PrevIncumbentCand[i], party)
  }



}

#Create filtered Df - only include located polling places
df_42Data = df_42Data %>% filter(Located != 0)


#Now iterate through the data frame and see which should be included or not
#Included entries are either: Old polling places that had both the same party and candidate
#AND New polling places that have a new candidate, but the same party
#This allows for the isolation of Personal incumbency bias

df_42Data$ToInclude = 0 #Initialise include dummy at 0


for (i in 1:nrow(df_42Data)) {

  party =  df_42Data$Incumbent[i]
  candidate = paste0(party,"_CAND")

  if (df_42Data$IsNew[i] == 0 & df_42Data$IncumbentCand[i] == df_42Data[[candidate]][i]
    |  df_42Data$IsNew[i] == 1 & df_42Data$Incumbent[i] == df_42Data$PrevIncumbent[i]){
    df_42Data$ToInclude[i] = 1
  }

}

#Now iterate through the data frame and see which should be included or not
#Only need to consider New polling places that have a new candidate, but the same party
#This allows for the measure of personal + party incumbency advantage, because it candidates that aren't personal incumbents, but are party incumbents

df_42Data$ToIncludeTotalEffect = 0 #Initialise include dummy at 0


for (i in 1:nrow(df_42Data)) {

  party =  df_42Data$Incumbent[i]
  candidate = paste0(party,"_CAND")

  if (df_42Data$IsNew[i] == 0
    |  df_42Data$IsNew[i] == 1 & df_42Data$Incumbent[i] == df_42Data$PrevIncumbent[i]){
    df_42Data$ToIncludeTotalEffect[i] = 1
  }

}



#######################################################

#######################################################

#######################################################

#Now include previous poll results to see if they predict turnout now

#First, import results 41
df_41Data <- read.csv("41Results.csv")

#Now, remove any entries that have NA entries in their turnout
df_41Data<- df_41Data %>% filter(TurnoutAbs != "Invalid Number")


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
    row_to_find = which(df_41Dict$Address == df_42Dict$Address[i])
      if (length(row_to_find) == 0){
      } else {
      df_42Dict$Match41[i] = df_41Dict$Place[row_to_find]
      }
}



#Now drop all rows with 0 in Match41
df_42Dict = df_42Dict %>% filter(Match41 != 0)

#### Now need to add incumbent party to to the table (incumbent in 42nd election riding)
df_42Dict$Incumbent <- 0
for (i in 1:nrow(df_42Dict)){ #Iterate across dictionary to assign address to each polling place
    row_to_find = which(df_42Data$Place == df_42Dict$Place[i])

      if (length(row_to_find) != 0){
      df_42Dict$Incumbent[i] = df_42Data$Incumbent[row_to_find]
      }
}


#Drop all where incumbent is 0
df_42Dict <- df_42Dict %>% filter(Incumbent != 0)



#Now add new column for incumbent vote share in prev. election
df_42Data$PrevIncVoteShare = 0

  for (i in 1:nrow(df_42Dict)){ #Iterate across dictionary to assign address to each polling place
        #Find row for place from 41st election
        row_to_find = which(df_41Data$Place == df_42Dict$Match[i])

        if (length(row_to_find) != 0){
        #Find incumbent party
        Incumbnet = df_42Dict$Incumbent[i]

        #Compute vote share of incumbent party
        VoteShare = df_41Data[[Incumbnet]][row_to_find]/df_41Data$TurnoutAbs[row_to_find]

        #Now find relevant row in df_42Data
        row_to_find = which(df_42Data$Place == df_42Dict$Place[i])

        #Write vote share to df_42Data
        df_42Data$PrevIncVoteShare[row_to_find] = VoteShare
        }
  }

df_42Data$PrevIncVoteShare[df_42Data$PrevIncVoteShare == 0] <- NA #Change all 0s to NA




##################################

#Filter further so that only directly matched ridings are included and they have ToInclude = 1
df_42Data_Filtered = df_42Data %>% filter(Riding41 != '#N/A') %>% filter(ToInclude == 1)


#Model 0 - basic without any additional fixed effects
model0 = lm(IncumbentShare ~ IsNew, data = df_42Data_Filtered)
# summary(model0)

#Model 1 - basic without riding level fixed effects, but including party effects
model1 = lm(IncumbentShare ~ Incumbent + IsNew, data = df_42Data_Filtered)
# summary(model1)

#Model 2 - includes riding level fixed effects
model2 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew, data = df_42Data_Filtered)
# summary(model2)

#Model 3 - includes riding level fixed effects + Past vote share
model3 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_Filtered)
# summary(model3)

#Model 4 - includes riding level fixed effects + Past vote share
model4 = lm(IncumbentShare ~ IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_Filtered)
# summary(model4)


library(stargazer)


stargazer(model0, model1, model2, model4, model3,
          keep = c('IsNew', 'PrevIncVoteShare','PrevIncVoteShare:IsNew'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "Linear Regression Model Results",
          align = TRUE)  # Align coefficients



# temp  <- df_42Data_Filtered[!is.na(df_42Data_Filtered$PrevIncVoteShare), ]


#Filter to only include PRECISE locations
#Create filtered Df to run first regression - only include located polling places
df_42Data_Filtered_Precise = df_42Data_Filtered %>% filter(Precise == 1)

#Model 0 - basic without any additional fixed effects
model5 = lm(IncumbentShare ~ IsNew, data = df_42Data_Filtered_Precise)
# summary(model3)

#Model 1 - basic without riding level fixed effects, but including party effects
model6 = lm(IncumbentShare ~ Incumbent + IsNew, data = df_42Data_Filtered_Precise)
# summary(model4)

#Model 2 - includes riding level fixed effects
model7 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew, data = df_42Data_Filtered_Precise)
# summary(model5)

#Model 3 - includes riding level fixed effects + Past vote share
model8 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_Filtered_Precise)
# summary(model7)

#Model 4 - includes riding level fixed effects + Past vote share
model9 = lm(IncumbentShare ~ IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_Filtered_Precise)
# summary(model7)

stargazer(model5, model6, model7, model9, model8,
          keep = c('IsNew', 'PrevIncVoteShare','PrevIncVoteShare:IsNew'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "Linear Regression Model Results - Precise Locations",
          align = TRUE)  # Align coefficients



##############################
#REPEAT ABOVE TO GET PARTY + PERSONAL INCUMBENCY ADVANTAGE


df_42Data_PartyPers <- df_42Data %>% filter(ToIncludeTotalEffect == 1)


#Model 0 - basic without any additional fixed effects
model10 = lm(IncumbentShare ~ IsNew, data = df_42Data_PartyPers)
# summary(model0)

#Model 1 - basic without riding level fixed effects, but including party effects
model11 = lm(IncumbentShare ~ Incumbent + IsNew, data = df_42Data_PartyPers)
# summary(model1)

#Model 2 - includes riding level fixed effects
model12 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew, data = df_42Data_PartyPers)
# summary(model2)

#Model 3 - includes riding level fixed effects + Past vote share
model13 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_PartyPers)
# summary(model3)

#Model 4 - includes riding level fixed effects + Past vote share
model14 = lm(IncumbentShare ~ IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_PartyPers)
# summary(model4)


library(stargazer)


stargazer(model10, model11, model12, model14, model13,
          keep = c('IsNew', 'PrevIncVoteShare','PrevIncVoteShare:IsNew'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "Party + Personal Linear Regression Model Results",
          align = TRUE)  # Align coefficients




df_42Data_Precise = df_42Data_PartyPers %>% filter(Precise == 1)


#Filter to only include PRECISE locations
#Create filtered Df to run first regression - only include located polling places
df_42Data_Filtered_Precise = df_42Data_Filtered %>% filter(Precise == 1)

#Model 0 - basic without any additional fixed effects
model15 = lm(IncumbentShare ~ IsNew, data = df_42Data_Precise)
# summary(model3)

#Model 1 - basic without riding level fixed effects, but including party effects
model16 = lm(IncumbentShare ~ Incumbent + IsNew, data = df_42Data_Precise)
# summary(model4)

#Model 2 - includes riding level fixed effects
model17 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew, data = df_42Data_Precise)
# summary(model5)

#Model 3 - includes riding level fixed effects + Past vote share
model18 = lm(IncumbentShare ~ Incumbent + factor(Riding) + IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_Precise)
# summary(model7)

#Model 4 - includes riding level fixed effects + Past vote share
model19 = lm(IncumbentShare ~ IsNew + PrevIncVoteShare + PrevIncVoteShare*IsNew, data = df_42Data_Precise)
# summary(model7)

stargazer(model15, model16, model17, model19, model18,
          keep = c('IsNew', 'PrevIncVoteShare','PrevIncVoteShare:IsNew'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "Linear Regression Model Results - Precise Locations",
          align = TRUE)  # Align coefficients

###############################################
#Robustness checker

#Compute vote share of winning party by riding within my data and compare to actual vote share in winning election
#Help identify bias in data
#Should colour code winner by party (red, orange, blue, light blue, green)


#First, use table 12 to find winning party for each riding
df_42Winners <- read.csv("42ResultsRAW/table_tableau12.csv")
#Now, because the table records the results for each person running, need to truncate so only the first row is retained for each riding
df_42Winners <- df_42Winners %>% distinct(Electoral.District.Number.Numéro.de.circonscription, .keep_all = TRUE)



#Now make a column that labels the winning party
df_42Winners$WinningParty = 0


for (i in 1:nrow(df_42Winners)) { #Iterate across every row in this table
    for (j in 1:6) { #Check to see which party is listed in the 
        contains_test <- str_detect(df_42Winners$Candidate.Candidat[i], parties_listed[j])

            if (contains_test) { #If test is true, write the party name in this column
                df_42Winners$WinningParty[i] = parties_tag[j]
                break #break once a match has been found
            }
    }
}


#Now need to aggregate results in df_42_Filtered to be at riding level
#Must also do this for the precise data frame afterward

#Initialise data frame for winners
df_42Data_Filtered_Winner = as.data.frame(unique(df_42Data$Riding))
colnames(df_42Data_Filtered_Winner) <- c("Riding")

#Add columns for winner, vote share, then computed total votes and computed vote share
df_42Data_Filtered_Winner$Winner = 0 #Actual results from table 12
df_42Data_Filtered_Winner$VoteShare = 0 #Actual results from table 12
df_42Data_Filtered_Winner$CompCandidateVote = 0 #For located stations only
df_42Data_Filtered_Winner$CompTurnout = 0 #For located stations only
df_42Data_Filtered_Winner$CompVoteShare = 0 #For located stations only
df_42Data_Filtered_Winner$PreciseCompCandidateVote = 0 #For precise filter
df_42Data_Filtered_Winner$PreciseCompTurnout = 0 #For located stations only
df_42Data_Filtered_Winner$PreciseCompVoteShare = 0  #For precise filter

#Now, iterate through ridings and assign winner and vote share from df_42Winners
for (i in 1:nrow(df_42Data_Filtered_Winner)) {
  #Find which index corresponds to each polling place
  row_to_find = which(df_42Winners$Electoral.District.Number.Numéro.de.circonscription == df_42Data_Filtered_Winner$Riding[i])
  df_42Data_Filtered_Winner$Winner[i] = df_42Winners$WinningParty[row_to_find]
  df_42Data_Filtered_Winner$VoteShare[i] = df_42Winners$Percentage.of.Votes.Obtained..Pourcentage.des.votes.obtenus[row_to_find]
}

#Now, aggregate vote results in df_42Data_Filtered to riding level

for (i in 1:nrow(df_42Data_Filtered)){ #Iterate across every row in the data table
  row_to_find = which(df_42Data_Filtered_Winner$Riding == df_42Data_Filtered$Riding[i]) #Find which row each observation corresponds to in the aggregation table
  party = df_42Data_Filtered_Winner$Winner[row_to_find] #Find correct party

  df_42Data_Filtered_Winner$CompCandidateVote[row_to_find] = df_42Data_Filtered_Winner$CompCandidateVote[row_to_find] + df_42Data_Filtered[[party]][i] #Add party specific vote
  
  df_42Data_Filtered_Winner$CompTurnout[row_to_find] = df_42Data_Filtered_Winner$CompTurnout[row_to_find] + df_42Data_Filtered$TurnoutAbs[i] #Add absolute turnout
}


#Repeat for precise locations

for (i in 1:nrow(df_42Data_Filtered_Precise)){ #Iterate across every row in the data table
  row_to_find = which(df_42Data_Filtered_Winner$Riding == df_42Data_Filtered_Precise$Riding[i]) #Find which row each observation corresponds to in the aggregation table
  party = df_42Data_Filtered_Winner$Winner[row_to_find] #Find correct party

  df_42Data_Filtered_Winner$PreciseCompCandidateVote[row_to_find] = df_42Data_Filtered_Winner$PreciseCompCandidateVote[row_to_find] + df_42Data_Filtered_Precise[[party]][i] #Add party specific vote
  
  df_42Data_Filtered_Winner$PreciseCompTurnout[row_to_find] = df_42Data_Filtered_Winner$PreciseCompTurnout[row_to_find] + df_42Data_Filtered_Precise$TurnoutAbs[i] #Add absolute turnout
}

#Compute turnout now as fraction of vote share
df_42Data_Filtered_Winner$CompVoteShare = (df_42Data_Filtered_Winner$CompCandidateVote/df_42Data_Filtered_Winner$CompTurnout)*100 #For all locations

df_42Data_Filtered_Winner$PreciseCompVoteShare = (df_42Data_Filtered_Winner$PreciseCompCandidateVote/df_42Data_Filtered_Winner$PreciseCompTurnout)*100 #For precise locations




library(ggplot2)
# library(ggthemes)

#Assign party colours to each row for party elected
parties_colours = c("#d71920","#003F72","#F58220","#3D9B35","#33B2CC")


df_42Data_Filtered_Winner$Colour = 0 #Initialize colour column

for (i in 1:nrow(df_42Data_Filtered_Winner)) { #Iterate across every row in this table
    for (j in 1:5) { #Check to see which party is listed in the 
        contains_test <- (df_42Data_Filtered_Winner$Winner[i] == parties_tag[j])
            if (contains_test) { #If test is true, write the party name in this column
                df_42Data_Filtered_Winner$Colour[i] = parties_colours[j]
                break #break once a match has been found
            }
    }
}

unique(df_42Data_Filtered_Winner$Winner)

parties_colours2 = c("LIB" = "#d71920","CON" = "#003F72","NDP" = "#F58220","GREEN" = "#3D9B35","BLOC" = "#33B2CC")


plot1 <- ggplot(df_42Data_Filtered_Winner, aes(x = VoteShare, y = CompVoteShare, colour = Winner)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(title = "",
       x = "Actual Vote Share of Winner",
       y = "Computed Vote Share of Winner") +
  xlim(30, 80) +
  ylim(30, 80) +
  scale_color_manual(values = parties_colours2)+
  labs(colour = "Party") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),    
    legend.position = c(0.98, .5),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) 
 




plot2 <- ggplot(df_42Data_Filtered_Winner, aes(x = VoteShare, y = PreciseCompVoteShare, colour = Winner)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(title = "",
       x = "Actual Vote Share of Winner",
       y = "Computed Vote Share of Winner") +
  xlim(30, 80) +
  ylim(30, 80) +
  scale_color_manual(values = parties_colours2)+
  labs(colour = "Party") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(0.98, .5),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    )




#Export plots
ggsave("plot1.png", plot = plot1, width = 6, height = 4, dpi = 300)
ggsave("plot2.png", plot = plot2, width = 6, height = 4, dpi = 300)

cat(" Attrition from matching to 41 =",1-sum(df_42Data_Filtered$Place41!=0)/nrow(df_42Data_Filtered))

cat(" Attrition from matching to 41 with precise data =",1-sum(df_42Data_Filtered_Precise$Place41!=0)/nrow(df_42Data_Filtered_Precise))



stargazer(model0, model1, model2,
          keep = c('Constant','IsNew'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "Linear Regression Model Results",
          align = TRUE)  # Align coefficients



stargazer(model3, model4, model5,
          keep = c('Constant','IsNew'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "Linear Regression Model Results",
          align = TRUE)  # Align coefficients




#Create data frame to store data on distribution of polling places and new-places by riding

RidingData = as.data.frame(unique(df_42Data$Riding))
colnames(RidingData) <- c("Riding")

territories = c(60001, 61001, 62001)

RidingData <- RidingData %>%
  filter(!Riding %in% territories)

#Initialize columns for other metrics
RidingData$FullSample = 0
RidingData$LocatedAll = 0
RidingData$LocatedNew = 0
RidingData$PreciseAll = 0
RidingData$PreciseNew = 0

for (i in 1:nrow(df_42Data)){ #Iterate across every row in the data table
  row_to_find = which(RidingData$Riding == df_42Data$Riding[i]) #Find which row each observation corresponds to in the aggregation table
  RidingData$FullSample[row_to_find] = RidingData$FullSample[row_to_find] + 1
}

for (i in 1:nrow(df_42Data_Filtered)){ #Iterate across every row in the data table
  row_to_find = which(RidingData$Riding == df_42Data_Filtered$Riding[i]) #Find which row each observation corresponds to in the aggregation table
  RidingData$LocatedAll[row_to_find] = RidingData$LocatedAll[row_to_find] + 1
  RidingData$LocatedNew[row_to_find] = RidingData$LocatedNew[row_to_find] + df_42Data_Filtered$IsNew[i]
}

for (i in 1:nrow(df_42Data_Filtered_Precise)){ #Iterate across every row in the data table
  row_to_find = which(RidingData$Riding == df_42Data_Filtered_Precise$Riding[i]) #Find which row each observation corresponds to in the aggregation table
  RidingData$PreciseAll[row_to_find] = RidingData$PreciseAll[row_to_find] + 1
  RidingData$PreciseNew[row_to_find] = RidingData$PreciseNew[row_to_find] + df_42Data_Filtered_Precise$IsNew[i]
}


stargazer(RidingData)
