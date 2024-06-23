#This script aggregates polling station level results to the building level for use in geo-coded resutls 
#This document, as the name suggests, aggregats the results of the 42nd national election. It also aggregates 41st election results in the 2nd half


library(dplyr)

# data <- read.csv("42ResultsRAW/pollresults_resultatsbureau10001.csv")

# #Remove any stations with null-results
# cleaned_data <- data %>% filter(Void.Poll.Indicator.Indicateur.de.bureau.supprimé == 'N')


#First, because polling stations/places do not have unique names, I must create a library
#that distinguishes unique polling places using their address 
#This dictionary will relate polling station numbers to a polling place - ie, address, that will be used to aggregate results

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

#Adjust electoral district column to just contain district number
data <- data %>%
  mutate(Riding = gsub("-.*", "", Riding)) %>% mutate(Riding = trimws(Riding))


#Generate a list of all unique entries in Riding column for use later
Ridings <- unique(data$Riding)


#Now create dictionary that maps polling station numbers (PD.SV) to a polling place
Places <- unique(data$AddressFull)


#For dictionary, because polling station IDs indexed to zero for each riding
#Need to add the riding number to the dictionary
#IE: 10001 1 is a dictionary entry that will map to polling place 1
 
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





#Now incorporate spatial data derived from QGIS geolocation process

df_located <- read.csv("CAN42Tag.csv")

#Now use similar processing as above to pair electoral district/polling station pairs
df_located <- df_located %>%
  mutate(PD.SV = Reformat_PDSV(PD.SV))

#Adjust electoral district column to retain just the district number
df_located <- df_located %>%
  mutate(Electoral = gsub("-.*", "", Electoral)) %>% mutate(Electoral = trimws(Electoral))


#Create dictionary column that containates the electoral and Polling station columns for matching
df_located$dictionary <- paste(df_located$Electoral,df_located$PD.SV) #concatinating station and riding number for matching

df_located$place <- Dictionary42$Place[match(df_located$dictionary,Dictionary42$Station)] #Matching polling place id's to stations


#list of locations captures by geo-processing that are deemed to be "precise" - capture a specific building, instead of a geographic feature
precise = c("place","building","amenity","leisure","man_made","tourism","club","office","historic","shop","aeroway","healthcare","emergency","military")


#Add a column indicating precise locate
df_located <- df_located %>%
  mutate(Precise = ifelse(category %in% precise, 1, 0))

#Clean IsNew column to take 0 instead of NA
df_located <- df_located %>%
    mutate(IsNew = ifelse(is.na(IsNew), 0, IsNew))

#Because of mistakes in data processing, the geolocate was done at the polling station level. Will truncate data to include only first entry from each polling place to make matching simpler
df_located <- df_located %>%
  distinct(place, .keep_all = TRUE)


#Initialize new columns in Results42 to for gelocation data
Results42$Located <- 0
Results42$LatLong <- NA
Results42$IsNew <- NA
Results42$Precise <- NA


#Iterate across df_located to add location-based data to the main dataframe
for (i in 1:nrow(df_located)) {
  #Find which index corresponds to each polling place
  row_to_find = which(Results42$Place == df_located$place[i])
  Results42$Located[row_to_find] = 1
  Results42$LatLong[row_to_find] = df_located$latlong[i]
  Results42$IsNew[row_to_find] = df_located$IsNew[i]
  Results42$Precise[row_to_find] = df_located$Precise[i]
}






write.csv(Results42, file = "42Results.csv", row.names = FALSE)


# sum(Results42$Located == 1)

# sum(Results42$IsNew == 1, na.rm = TRUE)





############################################################################
#Repeat for 41st election polling stations








data <- read.csv("41ResultsRAW/41PollLocations.csv", header = TRUE, sep = ",")




#First, exclude all results that are not ordinary polls (that is advanced, mobile etc...)
data <- data %>% filter(Type %in% "ORD")


# Apply the function to the PD.SV column
data <- data %>%
  mutate(PD.SV = Reformat_PDSV(PD.SV))

#Concatinate english address for matching stations to a polling place
data <- data %>%
  mutate(AddressFull = paste(Riding,Site.name..EN...Nom.du.site..AN., Address..EN..Adresse..AN., Municipality..EN., sep = ","))

#Generate a list of all unique entries in Riding column for use later
Ridings <- unique(data$Riding)


#Now create dictionary that maps polling station numbers (PD.SV) to a polling place
Places <- unique(data$AddressFull)


#For dictionary, because polling station IDs indexed to zero for each riding
#Need to add the riding number to the dictionary
#IE: 10001 1 is a dictionary entry that will map to polling place 1
 
data$Place <- match(data$AddressFull, Places) #Assign polling place ID's for 41st election


Dictionary41 <- data.frame(Station = paste(data$Riding,data$PD.SV),Place = data$Place)

#Initialise data frame that will store results by polling place
Rows = length(unique(Dictionary41$Place)) #Define data frame size

#Creating list of ridings that correspond to each polling place
temp <- Dictionary41 %>% mutate(Station = gsub("^(\\d{5}).*", "\\1", Station))

Results41 <- data.frame(Riding = temp$Station[match(unique(Dictionary41$Place),Dictionary41$Place)], Place = unique(Dictionary41$Place), Eligible = numeric(Rows), TurnoutAbs = numeric(Rows), TurnoutRel = numeric(Rows), 
Winner = numeric(Rows))


#Load readr to import csv's with french characters
library(readr)

association <- read_csv("41ResultsRAW/table_tableau11.csv", locale = locale(encoding = "ISO-8859-1"))



# Create list of parties to remove from their names
parties_to_remove = c(" Liberal/Libéral", " Conservative/Conservateur", " NDP-New Democratic Party/NPD-Nouveau Parti démocratique", " Bloc Québécois/Bloc Québécois", " Green Party/Parti Vert")

# Create function to remove party from name
remove_political_party <- function(candidate_info, parties) {
  pattern <- paste(parties, collapse = "|") # Create a regex pattern
  gsub(paste0(",?\\s*(", pattern, ")$"), "", candidate_info) # Remove the party info
}

# Function to reorder names to "first name last name"
reorder_name <- function(name) {
  name_parts <- strsplit(name, ",\\s*")[[1]]  # Split by comma and optional whitespace
  if (length(name_parts) == 2) {
    paste(trimws(name_parts[2]), trimws(name_parts[1]))  # Trim whitespace and reorder parts
  } else {
    name  # Return original if not in expected format
  }
}

# Adjust data frame using the functions
association <- association %>%
  rowwise() %>%
  mutate(CandidateName = reorder_name(remove_political_party(`Elected Candidate/Candidat elu`, parties_to_remove)))

#Iterating through riding data for 41st election now



for (i in Ridings){
print(i)

#Read in csv
ReadAddress <- paste0("41ResultsRAW/pollbypoll_bureauparbureau",i,'.csv') #Generates string that iterates through CSV list
print(ReadAddress)
poll <- read_csv(ReadAddress, locale = locale(encoding = "ISO-8859-1"))

#Find riding number, and associated election winner
row_to_find = which(association$'Electoral District Number/Numero de circonscription' == i)[1]
Candidate_to_match = association$CandidateName[row_to_find]

# Filter out rows where 'Scott Andrews' column contains "Void" or "merged"
poll <- poll %>%
  filter(!is.na(as.numeric(as.character(!!sym(Candidate_to_match))))) %>%
  mutate(!!sym(Candidate_to_match) := as.numeric(as.character(!!sym(Candidate_to_match))))


poll$dictionary <- paste(poll$'Electoral District Number/Numéro de circonscription',poll$'Polling Station Number/Numéro du bureau de scrutin') #concatinating station and riding number for matching

poll$place <- Dictionary41$Place[match(poll$dictionary,Dictionary41$Station)] #Matching polling place id's to stations


#Iterate across data frame to find total vote count and votes for elected candidate
for (j in 1:nrow(poll)) {
    #Write votes for winner
    Results41$Winner[poll$place[j]] = Results41$Winner[poll$place[j]] + poll[[Candidate_to_match]][j]
    
    #Write eligible voters
    Results41$Eligible[poll$place[j]] = Results41$Eligible[poll$place[j]] + poll$'Electors/Électeurs'[j]

    #Write total votes cast
    Results41$TurnoutAbs[poll$place[j]] = Results41$TurnoutAbs[poll$place[j]] + poll$'Total Votes/Total des votes'[j]
  }  

}

Results41$TurnoutRel <- Results41$TurnoutAbs/Results41$Eligible


write.csv(Results41, file = "41Results.csv", row.names = FALSE)
