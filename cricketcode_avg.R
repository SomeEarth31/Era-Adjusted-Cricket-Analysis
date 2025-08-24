
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

# Convert columns to numeric 
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


# Summarize the data by Era (sum for each era)
era_mean <- cricketers %>%
  group_by(Era) %>%
  summarise(across(c(Mat, Inns, NO, Runs, HS, Ave, SR, `100`, `50`, `0`), mean, .names = "mean_{.col}"))


# Divide each cricketer's value by the total sum for that era
normalized_cricketers <- cricketers %>%
  mutate(
    Norm_Mat = Mat / era_mean$mean_Mat,
    Norm_Inns = Inns / era_mean$mean_Inns,
    Norm_NO = NO / era_mean$mean_NO,
    Norm_Runs = Runs / era_mean$mean_Runs,
    Norm_HS = HS / era_mean$mean_HS,
    Norm_Ave = Ave / era_mean$mean_Ave,
    Norm_SR = SR / era_mean$mean_SR,
    Norm_100 = `100` / era_mean$mean_100,
    Norm_50 = `50` / era_mean$mean_50,
    Norm_0 = `0` / era_mean$mean_0
  ) 

#Calculate the rank statistic(a linear combination of the normalised metrics)
statistic= 10*(normalized_cricketers$Norm_Runs)+5*(normalized_cricketers$Norm_Ave)+2*(normalized_cricketers$Norm_SR)
normalized_cricketers$statistic=statistic

#Sort the players based on ranks
df_sort= normalized_cricketers %>% arrange(desc(statistic))
df_sort=df_sort %>% mutate(Index=row_number())

#Save this sorting
avg_stats = df_sort %>% select(Player, statistic)
save(avg_stats, file ="avg_stats.RData")
write.csv(df_sort,"ranks_avg.csv", row.names = FALSE)
