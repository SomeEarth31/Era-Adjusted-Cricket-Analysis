library(rvest)
library(dplyr)

#List of all the test playing teams
teams = c("afghanistan-40", "australia-2", "bangladesh-25", "england-1", 
          "india-6",  "ireland-29", "new-zealand-5", "pakistan-7", "south-africa-3",
          "sri-lanka-8", "west-indies-4", "zimbabwe-9")

pre_url = "https://www.espncricinfo.com/records/team/averages-batting/"
post_url = "/test-matches-1"

#Creating a combined dataframe of all the test players
cricketers = tibble()
for(t in teams){
  url = paste(pre_url, t, post_url, sep="")
  html = read_html(url)
  table = html %>% html_table()
  table = table[[1]]
  colnames(table) <- as.character(unlist(table[1, ]))
  table = table[-1, ]
  cricketers = rbind(cricketers, table)
}

#Removing duplicate players
cricketers <- cricketers %>%
  group_by(Player) %>%
  filter(n() == 1) %>%
  ungroup()

save(cricketers, file="all_players.RData")
