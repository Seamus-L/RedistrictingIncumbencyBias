#This file produces a regression discontinuity estimate 
#Can conveniently use table-12 files provided by statistics  canada to construct the data set
library(dplyr)
#First, create index to relate ridings across redistricting period (41 - 42)
dictionary <- read.csv("RidingFilter.csv")


#Change the names of the dictionary's columns to better reflect usage, then remove all unecessary columns, then mutate all #N/A to NA
dictionary <- dictionary %>%
    rename(NewNo = ED_CODE, OldNo = Direct.Match) %>%
  select(1, 3) %>%
  mutate(OldNo = na_if(OldNo, "#N/A"))


#View(dictionary)

#Initialise dataframe to write results to

rows = 5*338 #Basic setup - 338 ridings * 5 possible parties to be included


df <- data.frame(
    Riding = numeric(rows), #Riding Number (from dictionary, not actual number)
    Party = character(rows), #Party that candidate belongs to
    Cand = character(rows), #Candidate Name
    VS = numeric(rows), #Vote share in this year's election
    Win = numeric(rows), #Takes 1 if this candidate won this election (for computing MV)
    MV = numeric(rows), #Margin of victory 
    PersInc = numeric(rows), #Is candidate an incumbent (same person ran)
    Elec41 = numeric(rows),  #Dummy for part of 41st election
    Elec42 = numeric(rows),  #Same as above
    Elec43 = numeric(rows), 
    Elec44 = numeric(rows), 
    MVPrev = numeric(rows), #Margin of victory in previous election
    DPrev = numeric(rows) #Dummy for positive MV in previous election for this party
)

View(df)

#This function is used to truncate party from 
truncate_string <- function(input_string, keyword) {
  pos <- regexpr(keyword, input_string)
  if (pos != -1) {
    return(substr(input_string, 1, pos - 2))
  } else {
    return(input_string)
  }
}

party_reference <- c("LIB" = "Liberal", "NDP" = "NDP", "CON" = "Conservative", "BLOC" = "Bloc", "GREEN" = "Green")

parties_listed <-c("Liberal","Conservative","NDP","Green","Bloc")
parties_tag <- c("LIB","CON","NDP","GREEN","BLOC")


#Need to nest this in another loop that iterates across all tables
i = 41
data <- read.csv("RDData/table_tableau12_41.csv", fileEncoding = "latin1")

View(data)

k = 1 #initialise iterator for writing to final data frame



#Initialise new column for elected party
data$Party = NA

#First, iterate through and add new column with party name
library(stringr) #For searching strings
for (j in 1:nrow(data)) { #Iterate across every row in this table
    for (l in 1:5) { #Check to see which party is listed

        contains_test <- str_detect(data$Candidate.Candidat[j], parties_listed[l])
            if (contains_test==TRUE) { #If test is true, write the party name in this column
                data$Party[j] = parties_tag[l]
                break #break once a match has been found
            } 
        # print(j)
        # print(contains_test)
    }

}

View(data)

# colnames(data)

for (j in 1:nrow(data)){
    print(data$Electoral.District.Number.Numéro.de.circonscription[j])
    #First find riding number from table
    row_to_find = which(dictionary$OldNo == data$Electoral.District.Number.Numéro.de.circonscription[j]) #Must add if statement for old vs new
    if (length(row_to_find) != 0){
            print(row_to_find)
            df$Riding[k] = row_to_find #Indexed based on row of entry

        # Find respective party given value in candidate column
        candidate <- data$Party[j]
  
        if (candidate %in% names(party_reference)) {
            party <- party_reference[candidate]
        } else {
            party <- NA  # Handle cases where the incumbent is not in the party reference
        }
        print(party)
        # Truncate incumbent candidate if party is found
        if (!is.na(party)) {
            df$Party[k] <- data$Party[j] #Write the party
            df$Cand[k] <- truncate_string(data$Candidate.Candidat[j], party) #Write candidate name
            df$VS[k] <- data$Percentage.of.Votes.Obtained..Pourcentage.des.votes.obtenus[j]
            df$Win[k] <- ifelse(!is.na(data$Majority.Percentage.Pourcentage.de.majorité[j]), 1, 0)
            if (!is.na(data$Majority.Percentage.Pourcentage.de.majorité[j])){
                df$MV[k] <- data$Majority.Percentage.Pourcentage.de.majorité[j]
                VS = df$VS[k] #Temporarily store VS of winning candidate
            } else {
                df$MV[k] <- data$Percentage.of.Votes.Obtained..Pourcentage.des.votes.obtenus[j] - VS
            }
            df[[paste0("Elec",i)]][k] <- 1

            k = k+1 #iterate k by 1 only if we find a candidate that should be written
        }

    }    
    print(paste("K = ",k))
    
}



View(df)

#When computing MVPrev, must remember to exclude ridings where same candidate ran, but for a different party


#Now that df is fully computed, iterate across to remove ** and populate the pers.inc column.
for (i in 1:nrow(df)) { #Iterate across every row in this table
        contains_test <- str_detect(df$Cand[i], "\\*\\*")
            if (contains_test==TRUE) { #If test is true, write the party name in this column
                df$PersInc[i] = 1
                df$Cand[i] = truncate_string(df$Cand[i],"\\*\\*")
            } 

}


View(df)
