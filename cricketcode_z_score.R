
# Load necessary libraries
library(dplyr)

#Loading the dataframe
load("all_players.RData")

# Define the eras
eras <- data.frame(
  Era = c(
    "Pre-Ashes", "Bodyline Allowed", "New Handles and Rubber Grips", 
    "Pre-Five Day", "Pre-Helmets", "Post New Ball after 80 Overs", 
    "Reverse Swing Era", "Introduction of Neutral Empires", 
    "Introduction of DRS", "Standardised bat size"
  ),
  Start = c(1877, 1881, 1935, 1950, 1972, 1979, 1990, 1994, 2008, 2017),
  End = c(1881, 1935, 1950, 1972, 1979, 1990, 1994, 2008, 2017, 2025)
)

# Grouping players into eras
cricketers <- cricketers %>%
  mutate(
    Start_Year = as.numeric(sub("-.*", "", Span)),  # Extract start year
    End_Year = as.numeric(sub(".*-", "", Span)),    # Extract end year
    Midpoint = (Start_Year + End_Year) / 2          # Calculate midpoint
  ) %>%
  rowwise() %>%
  mutate(
    Era = eras %>% 
      filter(Midpoint >= Start & Midpoint <= End) %>% 
      pull(Era) %>% 
      first()  # Take the first matching era
  ) %>%
  ungroup()

# Preview the updated data
head(cricketers[, c("Player", "Span", "Midpoint", "Era")])


#View(cricketers)
colnames(cricketers)

# Convert columns to numeric (if they aren't already)
cricketers <- cricketers %>%
  mutate(
    Mat = as.numeric(Mat),
    Inns = as.numeric(Inns),
    NO = as.numeric(NO),
    Runs = as.numeric(Runs),
    HS = as.numeric(HS),
    Ave = as.numeric(Ave),
    SR = as.numeric(SR),
    `100` = as.numeric(`100`),
    `50` = as.numeric(`50`),
    `0` = as.numeric(`0`)
  )

# Replace null values (NA) with zero
cricketers <- cricketers %>%
  replace(is.na(.), 0)


#Find era wise mean and variance
era_summary <- cricketers %>%
  group_by(Era) %>%
  summarise(across(
    c(Mat, Inns, NO, Runs, HS, Ave, SR, `100`, `50`, `0`),
    list(mean = mean, std = sd),
    .names = "{.fn}_{.col}"
  ))


#Calculate Z_Score
normalized_cricketers <- cricketers %>%
  mutate(
    Z_Mat = (Mat - era_summary$mean_Mat)/ era_summary$std_Mat,
    Z_Inns = (Inns  - era_summary$mean_Inns)/ era_summary$std_Inns,
    Z_NO = (NO - era_summary$mean_NO)/ era_summary$std_NO,
    Z_Runs = (Runs - era_summary$mean_Runs)/ era_summary$std_Runs,
    Z_HS = (HS - era_summary$mean_HS)/ era_summary$std_HS,
    Z_Ave = (Ave - era_summary$mean_Ave)/ era_summary$std_Ave,
    Z_SR = (SR - era_summary$mean_SR)/ era_summary$std_SR,
    Z_100 = (`100` - era_summary$mean_100)/ era_summary$std_100,
    Z_50 = (`50` - era_summary$mean_50)/ era_summary$std_50,
    Z_0 = (`0` - era_summary$mean_0)/ era_summary$std_0
  ) 

#Calculate the rank statistic(a linear combination of the normalised metrics)
statistic= 10*(normalized_cricketers$Z_Runs)+5*(normalized_cricketers$Z_Ave)+2*(normalized_cricketers$Z_SR)
normalized_cricketers$statistic=statistic

#Sort the players based on ranks
df_sort= normalized_cricketers %>% arrange(desc(statistic))
df_sort=df_sort %>% mutate(Index=row_number())

#Save this sorting
z_stats = df_sort %>% select(Player, statistic)
save(z_stats, file ="z_stats.RData")
write.csv(df_sort,"ranks_Z.csv", row.names = FALSE)
