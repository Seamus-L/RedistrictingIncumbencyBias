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
    DPrev = rep(NA, rows)    # Dummy for positive MV in previous election for this party
    RidingPersInc = rep(NA, rows) #This row will contain 1 in PersInc is 1 for the entire riding, not just the elected candidate
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
        #candidate <- df$Cand[i] #Not needed as of now, as PersInc captrues this already
        riding <- df$Riding[i]

        row_to_find <- which(df$Party == party & df$Elec41 == 1 & df$Riding == riding) #Find row where party and riding matches and the data is from the previous year

        print (i)
        print(row_to_find)

        # print(df$MV[row_to_find])
        # print(df$Win[row_to_find])   

        if (length(row_to_find) != 0) { #Only do something if it finds a row that matches
            df$MVPrev[i] = df$MV[row_to_find] #Set MV prev 
            df$DPrev[i] = df$Win[row_to_find] #Set DPrev (party won in the prev election)
        }

    } else if (df$Elec43[i] == 1) { #If the data is from 43 we can find an incumbent
        party <- df$Party[i]
        #candidate <- df$Cand[i] #Not needed as of now, as PersInc captrues this already
        riding <- df$Riding[i]

        row_to_find <- which(df$Party == party & df$Elec42 == 1 & df$Riding == riding) #Find row where party and riding matches and the data is from the previous year

        print (i)
        print(row_to_find)

        # print(df$MV[row_to_find])
        # print(df$Win[row_to_find])   

        if (length(row_to_find) != 0) { #Only do something if it finds a row that matches
            df$MVPrev[i] = df$MV[row_to_find] #Set MV prev 
            df$DPrev[i] = df$Win[row_to_find] #Set DPrev (party won in the prev election)
        }
    
    } else if (df$Elec44[i] == 1) { #If the data is from 43 we can find an incumbent
        party <- df$Party[i]
        #candidate <- df$Cand[i] #Not needed as of now, as PersInc captrues this already
        riding <- df$Riding[i]

        row_to_find <- which(df$Party == party & df$Elec43 == 1 & df$Riding == riding) #Find row where party and riding matches and the data is from the previous year

        print (i)
        print(row_to_find)

        # print(df$MV[row_to_find])
        # print(df$Win[row_to_find])   

        if (length(row_to_find) != 0) { #Only do something if it finds a row that matches
            df$MVPrev[i] = df$MV[row_to_find] #Set MV prev 
            df$DPrev[i] = df$Win[row_to_find] #Set DPrev (party won in the prev election)
        }

    }

}

# View(df)



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

View(df)

df_party <- df %>% filter(RidingPersInc == 0) #Restrict to only include entries where candidate did not seek re-election

#Model 0 - uses full data set
model0 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev, data = df)
summary(model0)


#Model 1 - uses dataset where same candidate did NOT run - measures party advantage
model1 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev, data = df_party)
summary(model1)



df_42 <- df %>% filter(Elec42 == 1) #Restrict to only include 42nd election to compare to redistricting results

df_42_party <- df_42 %>% filter(RidingPersInc == 0) #Further restrict to only include entries where candidate did not seek re-election


#Model 2 - uses all candidates, but only 42nd election
model2 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev, data = df_42)
summary(model2)


#Model 3 - uses dataset where same candidate did NOT run - measures party advantage
model3 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev, data = df_42_party)
summary(model3)


library(ggplot2)

plot_df <- na.omit(df) #omit NA for plotting

# plot1 <- ggplot(df, aes(x = MVPrev, y = VS)) +
#   geom_point(size = 3) +
#   labs(title = "Vote Share vs Margin of Victory in Previous Election",
#        x = "MV t-1",
#        y = "VS")


# print(plot1)



# Create the scatter plot with the fitted regression lines
plot <- ggplot(plot_df, aes(x = MVPrev, y = VS, color = factor(DPrev))) +
  geom_point(size = 3) +  # Scatter plot of data points
  geom_smooth(method = "lm", aes(group = DPrev), color = "black", se = FALSE) +  # Add regression lines
  labs(title = "Vote Share vs Margin of Victory in Previous Election",
       x = "MV t-1",
       y = "VS",
       color = "DPrev") +  # Add labels
  theme_minimal()  # Use a minimal theme for better readability

# Display the plot
print(plot)



# Create the scatter plot with alpha blending
plot <- ggplot(plot_df, aes(x = MVPrev, y = VS, color = factor(DPrev))) +
  geom_point(size = 3, alpha = 0.5) +  # Adjust alpha for transparency
  geom_smooth(method = "lm", aes(group = DPrev), color = "black", se = FALSE) +
  labs(title = "Vote Share vs Margin of Victory in Previous Election",
       x = "MV t-1",
       y = "VS",
       color = "DPrev") +
  xlim(-75, 75) +
  ylim(0, 80) +      
  theme_minimal()

# Display the plot
print(plot)




# Define bin width
bin_width <- 1  # Adjust bin width as needed


# Define cutoff
cutoff <- 0

# Create bins and calculate midpoints
df_binned <- plot_df %>%
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
model_left <- lm(VS ~ MVPrev, data = plot_df %>% filter(MVPrev <= cutoff))
model_right <- lm(VS ~ MVPrev, data = plot_df %>% filter(MVPrev > cutoff))

# Create predictions for plotting
plot_df <- plot_df %>%
  mutate(fitted_left = ifelse(MVPrev <= cutoff, predict(model_left, newdata = plot_df), NA),
         fitted_right = ifelse(MVPrev > cutoff, predict(model_right, newdata = plot_df), NA))

# Create the scatter plot with binned summary points and separate regression lines
plot <- ggplot() +
  geom_smooth(data = plot_df %>% filter(MVPrev <= cutoff), aes(x = MVPrev, y = fitted_left), method = "lm", color = "black", se = FALSE) +  # Regression line for left side
  geom_smooth(data = plot_df %>% filter(MVPrev > cutoff), aes(x = MVPrev, y = fitted_right), method = "lm", color = "black", se = FALSE) +  # Regression line for right side
  geom_point(data = df_binned, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 2, alpha = 0.5) +  # Binned summary points
  labs(title = "Vote Share vs Margin of Victory in Previous Election - All Elec - Party + Personal",
       x = "MV t-1",
       y = "VS") +
  theme_minimal()

# Display the plot
print(plot)


ggsave("RDPlot1.png", plot = plot, width = 6, height = 4, dpi = 300)






#######
#Repeat for party restriction

plot_df_party <- na.omit(df_party) #omit NA for plotting



# Create bins and calculate midpoints
df_binned <- plot_df_party %>%
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
model_left <- lm(VS ~ MVPrev, data = plot_df_party %>% filter(MVPrev <= cutoff))
model_right <- lm(VS ~ MVPrev, data = plot_df_party %>% filter(MVPrev > cutoff))

# Create predictions for plotting
plot_df_party <- plot_df_party %>%
  mutate(fitted_left = ifelse(MVPrev <= cutoff, predict(model_left, newdata = plot_df_party), NA),
         fitted_right = ifelse(MVPrev > cutoff, predict(model_right, newdata = plot_df_party), NA))

# Create the scatter plot with binned summary points and separate regression lines
plot <- ggplot() +
  geom_smooth(data = plot_df_party %>% filter(MVPrev <= cutoff), aes(x = MVPrev, y = fitted_left), method = "lm", color = "black", se = FALSE) +  # Regression line for left side
  geom_smooth(data = plot_df_party %>% filter(MVPrev > cutoff), aes(x = MVPrev, y = fitted_right), method = "lm", color = "black", se = FALSE) +  # Regression line for right side
  geom_point(data = df_binned, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 2, alpha = 0.5) +  # Binned summary points
  labs(title = "Vote Share vs Margin of Victory in Previous Election - All Elec - Party",
       x = "MV t-1",
       y = "VS") +
  theme_minimal()

# Display the plot
print(plot)


ggsave("RDPlot2.png", plot = plot, width = 6, height = 4, dpi = 300)

















#######
#Repeat for 42nd election

plot_df_42<- na.omit(df_42) #omit NA for plotting



# Create bins and calculate midpoints
df_binned <- plot_df_42 %>%
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
model_left <- lm(VS ~ MVPrev, data = plot_df_42 %>% filter(MVPrev <= cutoff))
model_right <- lm(VS ~ MVPrev, data = plot_df_42 %>% filter(MVPrev > cutoff))

# Create predictions for plotting
plot_df_42 <- plot_df_42 %>%
  mutate(fitted_left = ifelse(MVPrev <= cutoff, predict(model_left, newdata = plot_df_42), NA),
         fitted_right = ifelse(MVPrev > cutoff, predict(model_right, newdata = plot_df_42), NA))

# Create the scatter plot with binned summary points and separate regression lines
plot <- ggplot() +
  geom_smooth(data = plot_df_42 %>% filter(MVPrev <= cutoff), aes(x = MVPrev, y = fitted_left), method = "lm", color = "black", se = FALSE) +  # Regression line for left side
  geom_smooth(data = plot_df_42 %>% filter(MVPrev > cutoff), aes(x = MVPrev, y = fitted_right), method = "lm", color = "black", se = FALSE) +  # Regression line for right side
  geom_point(data = df_binned, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 2, alpha = 0.5) +  # Binned summary points
  labs(title = "Vote Share vs Margin of Victory in Previous Election - 42 Elec - Party + Pers",
       x = "MV t-1",
       y = "VS") +
  theme_minimal()

# Display the plot
print(plot)


ggsave("RDPlot3.png", plot = plot, width = 6, height = 4, dpi = 300)





#######
#Repeat for 42nd election - party restriction

plot_df_42_party <- na.omit(df_42_party) #omit NA for plotting



# Create bins and calculate midpoints
df_binned <- plot_df_42_party %>%
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
model_left <- lm(VS ~ MVPrev, data = plot_df_42_party %>% filter(MVPrev <= cutoff))
model_right <- lm(VS ~ MVPrev, data = plot_df_42_party %>% filter(MVPrev > cutoff))

# Create predictions for plotting
plot_df_42_party <- plot_df_42_party %>%
  mutate(fitted_left = ifelse(MVPrev <= cutoff, predict(model_left, newdata = plot_df_42_party), NA),
         fitted_right = ifelse(MVPrev > cutoff, predict(model_right, newdata = plot_df_42_party), NA))

# Create the scatter plot with binned summary points and separate regression lines
plot <- ggplot() +
  geom_smooth(data = plot_df_42_party %>% filter(MVPrev <= cutoff), aes(x = MVPrev, y = fitted_left), method = "lm", color = "black", se = FALSE) +  # Regression line for left side
  geom_smooth(data = plot_df_42_party %>% filter(MVPrev > cutoff), aes(x = MVPrev, y = fitted_right), method = "lm", color = "black", se = FALSE) +  # Regression line for right side
  geom_point(data = df_binned, aes(x = MVPrev_mid, y = mean_VS), color = "black", size = 2, alpha = 0.5) +  # Binned summary points
  labs(title = "Vote Share vs Margin of Victory in Previous Election - 42 Elec - Party",
       x = "MV t-1",
       y = "VS") +
  theme_minimal()

# Display the plot
print(plot)


ggsave("RDPlot4.png", plot = plot, width = 6, height = 4, dpi = 300)







#Model 4 - uses full data set - Directly estimate both party and individual incumbency effect
model4 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev + RidingPersInc + DPrev*RidingPersInc+ DPrev*MVPrev*RidingPersInc , data = df)
summary(model4)


#Model 5 - uses 42nd election - Directly estimate both party and individual incumbency effect
model5 = lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev + RidingPersInc + DPrev*RidingPersInc+ DPrev*MVPrev*RidingPersInc , data = df_42)
summary(model5)



# Assuming 'df' is your data frame and 'model4' is already fitted
# Fit separate models for each side of the cutoff at MVPrev = 0
model_left <- lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev + RidingPersInc + DPrev*RidingPersInc + DPrev*MVPrev*RidingPersInc, data = df %>% filter(MVPrev <= 0))
model_right <- lm(VS ~ factor(Party) + DPrev + MVPrev + DPrev*MVPrev + RidingPersInc + DPrev*RidingPersInc + DPrev*MVPrev*RidingPersInc, data = df %>% filter(MVPrev > 0))

# Generate predictions for specific ranges
prediction_left <- data.frame(
  MVPrev = seq(from = min(df$MVPrev, na.rm = TRUE), to = 0, length.out = 100),
  DPrev = unique(df$DPrev),  # Replace with appropriate values if necessary
  Party = unique(df$Party),  # Replace with appropriate values if necessary
  RidingPersInc = unique(df$RidingPersInc)  # Replace with appropriate values if necessary
) %>%
  mutate(predicted_VS = predict(model_left, newdata = .))

prediction_right <- data.frame(
  MVPrev = seq(from = 0, to = max(df$MVPrev, na.rm = TRUE), length.out = 100),
  DPrev = unique(df$DPrev),  # Replace with appropriate values if necessary
  Party = unique(df$Party),  # Replace with appropriate values if necessary
  RidingPersInc = unique(df$RidingPersInc)  # Replace with appropriate values if necessary
) %>%
  mutate(predicted_VS = predict(model_right, newdata = .))

# Create the scatter plot with the fitted lines and regression discontinuity
plot <- ggplot(df, aes(x = MVPrev, y = VS)) +
  geom_point(aes(color = factor(Party)), size = 2, alpha = 0.6) +  # Scatter plot of data points
  geom_line(data = prediction_left, aes(x = MVPrev, y = predicted_VS), color = "black") +  # Fitted line for MVPrev <= 0
  geom_line(data = prediction_right, aes(x = MVPrev, y = predicted_VS), color = "black") +  # Fitted line for MVPrev > 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at MVPrev = 0
  labs(title = "Vote Share vs Margin of Victory in Previous Election",
       x = "MV t-1",
       y = "VS") +
  theme_minimal()

# Display the plot
print(plot)

ggsave("Temp1.png", plot = plot, width = 6, height = 4, dpi = 300)




# Create the scatter plot with the fitted regression lines
plot <- ggplot(plot_df, aes(x = MVPrev, y = VS, color = factor(DPrev))) +
  geom_point(size = 3) +  # Scatter plot of data points
  geom_smooth(method = "lm", aes(group = DPrev), color = "black", se = FALSE) +  # Add regression lines
  labs(title = "Vote Share vs Margin of Victory in Previous Election",
       x = "MV t-1",
       y = "VS",
       color = "DPrev") +  # Add labels
  theme_minimal()  # Use a minimal theme for better readability

# Display the plot
print(plot)


ggsave("Temp2.png", plot = plot, width = 6, height = 4, dpi = 300)
