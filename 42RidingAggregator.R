#This script aggregates polling station level results to the building level for use in geo-coded resutls # nolint: line_length_linter.

library(dplyr)

# data <- read.csv("42ResultsRAW/pollresults_resultatsbureau10001.csv")

# #Remove any stations with null-results
# cleaned_data <- data %>% filter(Void.Poll.Indicator.Indicateur.de.bureau.supprimé == 'N')


#First, because polling stations/places do not have unique names, I must create a library
#that distinguishes unique polling places using their address 
#This dictionary will relate polling station numbers to a polling place - ie, address, that will be used to aggregate results


# data <- read.csv("42ResultsRAW/42PollLocations.csv")

data <- read.csv("42ResultsRAW/42PollLocations.csv", header = TRUE, sep = ",")


#First, exclude all results that are not ordinary polls (that is advanced, mobile etc...)
values_to_include <- c("Ordinary/Ordinaire", "Ordinaire/Ordinary") #QC ridings have the order reversed, so must consider both possibilities
data <- data %>% filter(Type %in% values_to_include) %>% filter(Site.name..EN...Nom.du.site..AN. != 'VOID')

#Reformat PD.SV column to align formatting with poll result files
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

# Apply the function to the PD.SV column
data <- data %>%
  mutate(PD.SV = Reformat_PDSV(PD.SV))

#Concatinate english address for matching stations to a polling place
data <- data %>%
  mutate(AddressFull = paste(Riding,Site.name..EN...Nom.du.site..AN., Address..EN..Adresse..AN., Municipality..EN., sep = ","))

#Adjust electora district column to just contain district number
data <- data %>%
  mutate(Riding = gsub("-.*", "", Riding)) %>% mutate(Riding = trimws(Riding))


#Generate a list of all unique entries in Riding column for use later
Ridings <- unique(data$Riding)


#Now create dictionary that maps polling station numbers (PD.SV) to a polling place
Places <- unique(data$AddressFull)


#For dictionary, because polling station IDs indexed to zero for each riding
#Need to add the riding number to the dictionary
#IE: 10001,1 is a dictionary entry that will map to polling place 1
 
data$Place <- match(data$AddressFull, Places) #Assign polling place ID's for 42nd election


Dictionary42 <- data.frame(Station = paste(data$Riding,data$PD.SV),Place = data$Place)

#Initialise data frame that will store results by polling place
Rows = length(unique(Dictionary42$Place)) #Define data frame size

#Creating list of ridings that correspond to each polling place
temp <- Dictionary42 %>% mutate(Station = gsub("^(\\d{5}).*", "\\1", Station))

Results42 <- data.frame(Riding = temp$Station[match(unique(Dictionary42$Place),Dictionary42$Place)], Place = unique(Dictionary42$Place), Eligible = numeric(Rows), TurnoutAbs = numeric(Rows), TurnoutRel = numeric(Rows), 
LIB = numeric(Rows), CON = numeric(Rows), NDP = numeric(Rows), GREEN = numeric(Rows), BLOC = numeric(Rows),
LIB_CAND = numeric(Rows), CON_CAND = numeric(Rows),NDP_CAND = numeric(Rows),GREEN_CAND = numeric(Rows),BLOC_CAND = numeric(Rows))

#Start polling aggregation
for (i in Ridings){
print(i)
# #place holder to work on single entry while retaining loop structure
#     if (i == 10004) {
#     break
#     }

ReadAddress <- paste0("42ResultsRAW/pollresults_resultatsbureau",i,'.csv') #Generates string that iterates through CSV list
print(ReadAddress)
poll <- read.csv(ReadAddress)

poll$dictionary <- paste0(poll$Electoral.District.Number.Numéro.de.circonscription,poll$Polling.Station.Number.Numéro.du.bureau.de.scrutin) #concatinating station and riding number for matching

poll$place <- Dictionary42$Place[match(poll$dictionary,Dictionary42$Station)] #Matching polling place id's to stations




poll <- poll[!(is.na(poll$place)), ] #Drop any NA obserations (avanced, mobile etc..)


#In cases where all merged stations are removed by the NA observation drop, the column becomes NA resulting in isses
#This code chunk sets the value to an empty string so it is compatible with the rest of the code
poll$Merge.With.Fusionné.avec <- lapply(poll$Merge.With.Fusionné.avec, function(x) {
  x[is.na(x)] <- ""
  return(x)
})

poll <- subset(poll, Merge.With.Fusionné.avec == "") #Drop any polling stations that were merged as they do not have results

#Because candidates are the same for the entire riding, can take candidates list from first station
#First find index of first entry of LIB, CON, NDP, GREEN, and BLOC
parties <- c("LIB","CON","NDP","GREEN","BLOC")

indices <- c(Index_LIB <- which(poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais == 'Liberal')[1],
Index_CON <- which(poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais == 'Conservative')[1],
Index_NDP <- which(poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais == 'NDP-New Democratic Party')[1],
Index_GREEN <- which(poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais == 'Green Party')[1],
Index_BLOC <- which(poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais == 'Bloc Québécois')[1])

#Define min and max polling places in this riding 
range_max = max(poll$place) 
range_min = min(poll$place)

#Change values in relevant columns of the results sheet
for (j in 1:5){
  cand = paste0(parties[j],"_CAND")
  if (is.na(indices[j])) { #Check for NA indices in cases where party did not run in this riding, ex: bloc
    name = "NA"
  } else {
    name = paste0(poll$Candidate.s.Family.Name.Nom.de.famille.du.candidat[indices[j]],", ",poll$Candidate.s.First.Name.Prénom.du.candidat[indices[j]])
  }
  Results42[[cand]][range_min:range_max] <- name #name assigned as "last name, first name"
}

#Iterate across data frame to find total vote count for each candidate, as well as the total
for (j in 1:nrow(poll)) {
  if (poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais[j] == 'Liberal') { #Liberal sum
    Results42$LIB[poll$place[j]] = Results42$LIB[poll$place[j]] + poll$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau[j]
    
    #Because every riding has a liberal candidate, I can use their value for eligiblity to back out the total amount
    Results42$Eligible[poll$place[j]] = poll$Electors.for.Polling.Station.Électeurs.du.bureau[j] + Results42$Eligible[poll$place[j]]
  }
 
  else if  (poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais[j] == 'Conservative') { #Conservative sum
    Results42$CON[poll$place[j]] = Results42$CON[poll$place[j]] + poll$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau[j]
  }

  else if  (poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais[j] == 'NDP-New Democratic Party') { #Conservative sum
    Results42$NDP[poll$place[j]] = Results42$NDP[poll$place[j]] + poll$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau[j]
  }

  else if  (poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais[j] == 'Green Party') { #Conservative sum
    Results42$GREEN[poll$place[j]] = Results42$GREEN[poll$place[j]] + poll$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau[j]
  }

  else if  (poll$Political.Affiliation.Name_English.Appartenance.politique_Anglais[j] == 'Bloc Québécois') { #Conservative sum
    Results42$BLOC[poll$place[j]] = Results42$BLOC[poll$place[j]] + poll$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau[j]
  }

Results42$TurnoutAbs[poll$place[j]] = Results42$TurnoutAbs[poll$place[j]] + poll$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau[j]

}

}

#Drop any rows with 0 in TurnoutABS

Results42 <- Results42[Results42$TurnoutAbs != 0, ]


Results42$TurnoutRel <- Results42_filtered$TurnoutAbs/Results42_filtered$Eligible


write.csv(Results42, file = "42Results.csv", row.names = FALSE)

# #Number of polling places that had >100% turnout (can only be due to people voting)
# sum(Results42_filtered$TurnoutRel > 1)


# #Remove territories from list
# territories = c(60001,61001,62001)
# rows_to_drop <- Results42_filtered$Riding %in% territories

# Results42_filtered_territories <- Results42_filtered[!rows_to_drop, ]
# sum(Results42_filtered_territories$TurnoutRel > 1)
