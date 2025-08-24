library(dplyr)

#Load the two sorted datasets
load("z_stats.RData")
load("avg_stats.RData")

#combine the datasets
combed = avg_stats %>% left_join(z_stats, by="Player")

#Normalise both statistics
combed <- combed %>%
  mutate(across(.cols = c(statistic.x, statistic.y), 
                .fns = ~ ( . - min(.) ) / (max(.) - min(.))))

#Create a combined statistic by giving equal weight to both
combed$tot = combed$statistic.x + combed$statistic.y

#Sort based on new statistic
combed= combed %>% arrange(desc(tot))

#Save the final ranking
write.csv(combed,"final_ranks.csv", row.names = FALSE)