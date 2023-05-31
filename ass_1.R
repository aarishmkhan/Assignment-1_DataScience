library(rvest)
library(dplyr)

# Question a
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")

nifty50_table <- html %>% html_nodes("table") %>% html_table()

#nifty50_table[[1]]
#janitor::row_to_names(nifty50_table[[1]], row_number = 1)
nifty50_table[[1]]
nifty50_table2 <- nifty50_table[[1]][-c(1,14,15)]


nifty50_data <- data.frame(nifty50_table2)
write.csv(nifty50_data, "C:\\Users\\HENA\\Desktop\\Stamatics_data_science\\Q1.csv", row.names = FALSE)

# Question b

company1 <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/telecom/telecommunication-service-provider/bharti-airtel/company-info")

company1_table1 <- company1 %>% html_nodes("table") %>% html_table()

company1_table1.1a <- company1_table1[[1]][-c(2:5), -c(12:14)]
company1_table1.1b <- company1_table1[[3]][,-c(12, 13)]

company1_data.1 <- data.frame(company1_table1.1a)
company1_data.2 <- data.frame(company1_table1.1b)

company1_table1.1 <- bind_rows(company1_data.1, company1_data.2)
write.csv(company1_table, "C:\\Users\\HENA\\Desktop\\Stamatics_data_science\\Q2a.csv", row.names = FALSE)

# Question c
# 1.
tennis <- function(p) {
  
  playerA <- 0
  playerB <- 0
  for(i in 1:5)
  {
    set <- sample(0:1, size = 1, prob = c(p, 1-p))
    if(set == 0) playerB <- playerB+1
    if(set == 1) playerA <- playerA+1
    
    if(playerA == 3 || playerB == 3)
    {
      x <- i
      break
    }
  }  
  return(x)
}
res <- tennis(0.3)
print(res)

#2. 

matches <- numeric(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(.7)
}

ans <- mean(matches)
ans

# Question d
# 1.
MontyHall <- function() {
  inside_door <- c("car", "goat", "goat")
  door_chosen <- sample(1:3, 1) 
  

  if (inside_door[door_chosen] == "car") {
    door_monty <- sample(c(2, 3), 1)
  } else {
    door_monty <- setdiff(1:3, c(door_chosen, which(inside_door == "car")))
  }
  
  remaining_door <- setdiff(1:3, c(door_chosen, door_monty))
  
  if (inside_door[remaining_door] == "car") {
    return(1)  
  } else {
    return(0) 
  }
}

# 2.
num_simulations <- 1000
num_wins <- 0

for (i in 1:num_simulations) {
  result <- MontyHall()
  num_wins <- num_wins + result
}

probability_of_winning <- num_wins / num_simulations

probability_of_winning



# Question e

html2 <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

ranking <- html2 %>%  html_nodes(".countdown-index") %>% html_text() %>% trimws()
ranking

movie_names <- html2 %>% html_nodes(".article_movie_title a") %>% html_text() %>% trimws()
movie_names

tomato_scores <- html2 %>% html_nodes(".tMeterScore") %>% html_text() %>%  trimws()
tomato_scores

years <- html2 %>% html_nodes(".start-year") %>% html_text() %>%  trimws()
years

movies_data <- data.frame( Ranking = ranking, Movie = movie_names, Tomato_Score = tomato_scores, Year = years)
print(movies_data)
