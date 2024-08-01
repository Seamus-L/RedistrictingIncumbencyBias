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

rows = 4*4*338 #Basic setup - 338 ridings * 5 possible parties to be included


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
    MVPrev = rep(NA, rows),  # Margin of victory in previous election
    DPrev = rep(NA, rows),   # Dummy for positive MV in previous election for this party
    RidingPersInc = rep(NA, rows), #This row will contain 1 in PersInc is 1 for the entire riding, not just the elected candidate
    RepeatCand = rep(NA, rows) #Will contain 1 if candidate is a repeat, 0 otherwise
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


elections = c(41,42,43,44) #List of general elections for iteration
k = 1 #initialise iterator for writing to final data frame



for (i in elections){
    address <- paste0("RDData/table_tableau12_",i,".csv")
    if (i == 41){
        data <- read.csv(address, fileEncoding = "latin1") #First table has different encoding
    } else {
        data <- read.csv(address, fileEncoding = "UTF8") #All other tables use UTF8
    }
    print(address)
    View(data)





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

    for (j in 1:nrow(data)){
        # print(data$Electoral.District.Number.Numéro.de.circonscription[j])
        #First find riding number from table
        if (i == 41){
            row_to_find = which(dictionary$OldNo == data$Electoral.District.Number.Numéro.de.circonscription[j])
        } else {
            row_to_find = which(dictionary$NewNo == data$Electoral.District.Number.Numéro.de.circonscription[j])
        }
         #Must add if statement for old vs new
        if (length(row_to_find) != 0){
                # print(row_to_find)
                df$Riding[k] = row_to_find #Indexed based on row of entry

            # Find respective party given value in candidate column
            candidate <- data$Party[j]
  
            if (candidate %in% names(party_reference)) {
                party <- party_reference[candidate]
            } else {
                party <- NA  # Handle cases where the incumbent is not in the party reference
            }
            # print(party)
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
    # print(paste("K = ",k))
    
    }

}


# View(df)

#When computing MVPrev, must remember to exclude ridings where same candidate ran, but for a different party


#Now that df is fully computed, iterate across to remove ** and populate the pers.inc column.
for (i in 1:nrow(df)) { #Iterate across every row in this table
        contains_test <- str_detect(df$Cand[i], "\\*\\*")
            if (contains_test==TRUE) { #If test is true, write the party name in this column
                df$PersInc[i] = 1
                df$Cand[i] = truncate_string(df$Cand[i],"\\*\\*")
            } 

}

#Need to assign this to all participants in the riding, not just the one person. Should be easy with an if statement though
#This entire thing will happen outside i loop


df <- df[1:k-1,] #Truncate df to only include rows that were written to

# View(df)


for (i in 1:nrow(df)){
    if (df$Elec41[i] == 1) {
     #Do nothing - no incumbency to evaluate since I don't have data from prior years
    } else if (df$Elec42[i] == 1) { #If the data is from 42 we can find an incumbent
                party <- df$Party[i]
        candidate <- df$Cand[i] #Not needed as of now, as PersInc captrues this already
        riding <- df$Riding[i]

        row_to_find <- which(df$Party == party & df$Elec41 == 1 & df$Riding == riding) #Find row where party and riding matches and the data is from the previous year

        # print(df$MV[row_to_find])
        # print(df$Win[row_to_find])   


        # print(i)
        # print(row_to_find)
        if (length(row_to_find) != 0) { #Only do something if it finds a row that matches
            df$MVPrev[i] = df$MV[row_to_find] #Set MV prev 
            df$DPrev[i] = df$Win[row_to_find] #Set DPrev (party won in the prev election)
                    
            if (candidate == df$Cand[row_to_find]){ #Check if candidate from this riding/party ran again
                df$RepeatCand[i] = 1
            } else {df$RepeatCand[i] = 0}
        
        }

    } else if (df$Elec43[i] == 1) { #If the data is from 43 we can find an incumbent
                party <- df$Party[i]
        candidate <- df$Cand[i] #Not needed as of now, as PersInc captrues this already
        riding <- df$Riding[i]

        row_to_find <- which(df$Party == party & df$Elec42 == 1 & df$Riding == riding) #Find row where party and riding matches and the data is from the previous year

        # print (i)
        # print(row_to_find)

        # print(df$MV[row_to_find])
        # print(df$Win[row_to_find])   

        if (length(row_to_find) != 0) { #Only do something if it finds a row that matches
            df$MVPrev[i] = df$MV[row_to_find] #Set MV prev 
            df$DPrev[i] = df$Win[row_to_find] #Set DPrev (party won in the prev election)
                    
            if (candidate == df$Cand[row_to_find]){ #Check if candidate from this riding/party ran again
                df$RepeatCand[i] = 1
            } else {df$RepeatCand[i] = 0}
        
        }
    
    } else if (df$Elec44[i] == 1) { #If the data is from 43 we can find an incumbent
        party <- df$Party[i]
        candidate <- df$Cand[i] #Not needed as of now, as PersInc captrues this already
        riding <- df$Riding[i]

        row_to_find <- which(df$Party == party & df$Elec43 == 1 & df$Riding == riding) #Find row where party and riding matches and the data is from the previous year

        # print (i)
        # print(row_to_find)

        # print(df$MV[row_to_find])
        # print(df$Win[row_to_find])   

        if (length(row_to_find) != 0) { #Only do something if it finds a row that matches
            df$MVPrev[i] = df$MV[row_to_find] #Set MV prev 
            df$DPrev[i] = df$Win[row_to_find] #Set DPrev (party won in the prev election)

            if (candidate == df$Cand[row_to_find]){ #Check if candidate from this riding/party ran again
                df$RepeatCand[i] = 1
            } else {df$RepeatCand[i] = 0}

        }

    }

}




#Populate - does riding have a repeat candidate for incumbent party
riding = df$Riding[1] #initialise riding as first riding
IsPersInc = df$PersInc[1]
df$RidingPersInc[1] = IsPersInc
riding = df$Riding[1]

for (i in 2:nrow(df)){ #Iterate through dataframe to populate whether or not the riding has a personal incumbent
    if (df$Riding[i] == riding) { #Check to see if still in same riding
        df$RidingPersInc[i] = IsPersInc
        
    } else {
       riding = df$Riding[i]
       IsPersInc = df$PersInc[i]
       df$RidingPersInc[i] = IsPersInc
    }

}

df_regression <- df %>% filter(MVPrev >= -15 & MVPrev <= 15) #Restricted to 15% bandwidth around MV = 0 following Rekkas
# %>% filter(RidingPersInc == 1) %>% filter(MVPrev >= -15 & MVPrev <= 15)



#Model 1 - uses full data set
model1 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev + RidingPersInc + DPrev*RidingPersInc + DPrev*MVPrev*RidingPersInc, data = df_regression)
summary(model1)



df_42 <- df %>% filter(Elec42 == 1) #Restrict to only include 42nd election to compare to redistricting results

df_regression_42 <- df_regression %>% filter(Elec42 == 1)

#Model 2 - restrict to 42nd election
model2 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev + RidingPersInc + DPrev*RidingPersInc + DPrev*MVPrev*RidingPersInc, data = df_regression_42)
summary(model2)


library(stargazer)

stargazer(model1, model2,
          keep = c('DPrev', 'MVPrev', 'RidingPersInc', 'Dprev:RidingPersInc'),
          omit.stat = c("rsq", "ser", "f"),  # Omit standard errors and F-statistic
          title = "RD Design - 15% Bandwidth",
          align = TRUE)  # Align coefficients




library(ggplot2)


plot_df <- na.omit(df) #omit NA for plotting



library(ggthemes)


# Define bin width
bin_width <- 1  # 1% bin width following Rekkas


# Define cutoff
cutoff <- 0


###################### USING BANDWIDTH REGRESSION

# Need to shift points based on party FE to draw graph properly


FE_Con = as.numeric(model1$coefficients[2])
FE_Green = as.numeric(model1$coefficients[3])
FE_Lib = as.numeric(model1$coefficients[4])
FE_NDP = as.numeric(model1$coefficients[5])

#Create list to associate fixed effects
FE = list(
    'CON' = FE_Con,
    'LIB' = FE_Lib,
    'GREEN' = FE_Green,
    'NDP' = FE_NDP,
    'BLOC' = 0
)



plot_df_1 <- plot_df %>%
  mutate(VS = VS - unlist(FE[Party]))


# Create bins and calculate midpoints
df_binned <- plot_df_1 %>% filter(RidingPersInc == 0) %>%
  mutate(MVPrev_bin = cut(MVPrev, 
                          breaks = seq(floor(min(MVPrev, na.rm = TRUE)), ceiling(max(MVPrev, na.rm = TRUE)), by = bin_width), 
                          include.lowest = TRUE, 
                          right = FALSE)) %>%
  filter(!is.na(MVPrev_bin)) %>%
  group_by(MVPrev_bin) %>%
  summarise(mean_VS = mean(VS, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(
    bin_range = as.character(MVPrev_bin),
    lower_bound = as.numeric(sub("\\[(-?[0-9.]+),.*\\)", "\\1", bin_range)),
    MVPrev_mid = lower_bound + bin_width / 2
  ) %>%
  filter(!is.na(mean_VS) & !is.na(MVPrev_mid))


df_binned2 <- plot_df_1 %>% filter(RidingPersInc == 1) %>%
  mutate(MVPrev_bin = cut(MVPrev, 
                          breaks = seq(floor(min(MVPrev, na.rm = TRUE)), ceiling(max(MVPrev, na.rm = TRUE)), by = bin_width), 
                          include.lowest = TRUE, 
                          right = FALSE)) %>%
  filter(!is.na(MVPrev_bin)) %>%
  group_by(MVPrev_bin) %>%
  summarise(mean_VS = mean(VS, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(
    bin_range = as.character(MVPrev_bin),
    lower_bound = as.numeric(sub("\\[(-?[0-9.]+),.*\\)", "\\1", bin_range)),
    MVPrev_mid = lower_bound + bin_width / 2
  ) %>%
  filter(!is.na(mean_VS) & !is.na(MVPrev_mid))


# Fit separate models for each side of the cutoff
model_leftN <- lm(VS ~ MVPrev, data = plot_df_1 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 0))
model_rightN <- lm(VS ~ MVPrev, data = plot_df_1 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 0))
model_leftY <- lm(VS ~ MVPrev, data = plot_df_1 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 1))
model_rightY <- lm(VS ~ MVPrev, data = plot_df_1 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 1))

# Create predictions for plotting
plot_df_1 <- plot_df_1 %>%
  mutate(fitted_leftN = ifelse(MVPrev <= cutoff, predict(model_leftN, newdata = plot_df_1), NA),
         fitted_rightN = ifelse(MVPrev > cutoff, predict(model_rightN, newdata = plot_df_1), NA),
         fitted_leftY = ifelse(MVPrev <= cutoff, predict(model_leftY, newdata = plot_df_1), NA),
         fitted_rightY = ifelse(MVPrev > cutoff, predict(model_rightY, newdata = plot_df_1), NA))

# Create the scatter plot with binned summary points and separate regression lines
plot1 <- ggplot() +
  geom_smooth(data = plot_df_1 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 0), aes(x = MVPrev, y = fitted_leftN), method = "lm", color = "black", se = FALSE, size = 0.5) +
  geom_smooth(data = plot_df_1 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 0), aes(x = MVPrev, y = fitted_rightN), method = "lm", color = "black", se = FALSE, size = 0.5) +
  geom_smooth(data = plot_df_1 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 1), aes(x = MVPrev, y = fitted_leftY), method = "lm", color = "red", se = FALSE, size = 0.5) +
  geom_smooth(data = plot_df_1 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 1), aes(x = MVPrev, y = fitted_rightY), method = "lm", color = "red", se = FALSE, size = 0.5) +
  geom_point(data = df_binned, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 1.2, alpha = 0.5, shape = 16) +
  geom_point(data = df_binned2, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 1, alpha = 0.8, shape = 0) +
  labs(x = "Margin of Victory (%), time t-1",
       y = "Vote Share (%), time t") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 20)) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20))

# Display the plot
print(plot1)






############# REPEAT FOR YEAR RESTRICTION

plot_df_42<- na.omit(df_42) #omit NA for plotting

FE_Con = as.numeric(model2$coefficients[2])
FE_Green = as.numeric(model2$coefficients[3])
FE_Lib = as.numeric(model2$coefficients[4])
FE_NDP = as.numeric(model2$coefficients[5])

#Create list to associate fixed effects
FE = list(
    'CON' = FE_Con,
    'LIB' = FE_Lib,
    'GREEN' = FE_Green,
    'NDP' = FE_NDP,
    'BLOC' = 0
)


plot_df_2 <- plot_df_42 %>%
  mutate(VS = VS - unlist(FE[Party]))




# Create bins and calculate midpoints
df_binned <- plot_df_2  %>% filter(RidingPersInc == 0)%>%
  mutate(MVPrev_bin = cut(MVPrev, 
                          breaks = seq(floor(min(MVPrev, na.rm = TRUE)), ceiling(max(MVPrev, na.rm = TRUE)), by = bin_width), 
                          include.lowest = TRUE, 
                          right = FALSE)) %>%
  filter(!is.na(MVPrev_bin)) %>%
  group_by(MVPrev_bin) %>%
  summarise(mean_VS = mean(VS, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(
    bin_range = as.character(MVPrev_bin),
    lower_bound = as.numeric(sub("\\[(-?[0-9.]+),.*\\)", "\\1", bin_range)),
    MVPrev_mid = lower_bound + bin_width / 2
  ) %>%
  filter(!is.na(mean_VS) & !is.na(MVPrev_mid))


df_binned2 <- plot_df_2 %>% filter(RidingPersInc == 1) %>%
  mutate(MVPrev_bin = cut(MVPrev, 
                          breaks = seq(floor(min(MVPrev, na.rm = TRUE)), ceiling(max(MVPrev, na.rm = TRUE)), by = bin_width), 
                          include.lowest = TRUE, 
                          right = FALSE)) %>%
  filter(!is.na(MVPrev_bin)) %>%
  group_by(MVPrev_bin) %>%
  summarise(mean_VS = mean(VS, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(
    bin_range = as.character(MVPrev_bin),
    lower_bound = as.numeric(sub("\\[(-?[0-9.]+),.*\\)", "\\1", bin_range)),
    MVPrev_mid = lower_bound + bin_width / 2
  ) %>%
  filter(!is.na(mean_VS) & !is.na(MVPrev_mid))



# Fit separate models for each side of the cutoff
model_leftN <- lm(VS ~ MVPrev, data = plot_df_2 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 0))
model_rightN <- lm(VS ~ MVPrev, data = plot_df_2 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 0))
model_leftY <- lm(VS ~ MVPrev, data = plot_df_2 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 1))
model_rightY <- lm(VS ~ MVPrev, data = plot_df_2 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 1))

# Create predictions for plotting
plot_df_2 <- plot_df_2 %>%
  mutate(fitted_leftN = ifelse(MVPrev <= cutoff, predict(model_leftN, newdata = plot_df_2), NA),
         fitted_rightN = ifelse(MVPrev > cutoff, predict(model_rightN, newdata = plot_df_2), NA),
         fitted_leftY = ifelse(MVPrev <= cutoff, predict(model_leftY, newdata = plot_df_2), NA),
         fitted_rightY = ifelse(MVPrev > cutoff, predict(model_rightY, newdata = plot_df_2), NA))

# Create the scatter plot with binned summary points and separate regression lines
plot2 <- ggplot() +
  geom_smooth(data = plot_df_2 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 0), aes(x = MVPrev, y = fitted_leftN), method = "lm", color = "black", se = FALSE, size = 0.5) +
  geom_smooth(data = plot_df_2 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 0), aes(x = MVPrev, y = fitted_rightN), method = "lm", color = "black", se = FALSE, size = 0.5) +
  geom_smooth(data = plot_df_2 %>% filter(MVPrev <= cutoff, MVPrev > -15, RidingPersInc == 1), aes(x = MVPrev, y = fitted_leftY), method = "lm", color = "red", se = FALSE, size = 0.5) +
  geom_smooth(data = plot_df_2 %>% filter(MVPrev > cutoff, MVPrev < 15, RidingPersInc == 1), aes(x = MVPrev, y = fitted_rightY), method = "lm", color = "red", se = FALSE, size = 0.5) +
  geom_point(data = df_binned, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 1.2, alpha = .5, shape = 16) +
  geom_point(data = df_binned2, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 1, alpha = .8, shape = 0) +
  labs(x = "Margin of Victory (%), time t-1",
       y = "Vote Share (%), time t") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 20)) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20))

# Display the plot
print(plot2)






combinedplot <- plot1 | plot2

ggsave("CombinedPlot1.png", plot = combinedplot, width = 6, height = 3, dpi = 300)


