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
remove_leading_zeros_and_hyphens <- function(x) {
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
  mutate(PD.SV = remove_leading_zeros_and_hyphens(PD.SV))

#Concatinate english address for matching stations to a polling place
data <- data %>%
  mutate(AddressFull = paste(Site.name..EN...Nom.du.site..AN., Address..EN..Adresse..AN., Municipality..EN., sep = ","))

#Adjust electora district column to just contain district number
data <- data %>%
  mutate(Riding = gsub("-.*", "", Riding))


#Generate a list of all unique entries in Riding column
Ridings <- unique(data$Riding)