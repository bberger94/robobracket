# ----------------------- #
# Randomly choose bracket 
# ----------------------- #

library(tidyverse)
library(openxlsx)

# Set parameters
season <- 2025    
bracket_id <- 100   # choose number from 0 to 999

# Determine random seed based on season and bracket ID 
convert_float_to_int <- function(x){
  raw_bytes <- writeBin(x, raw(), size = 4)  
  int_value <- readBin(raw_bytes, "integer", size = 4, endian = .Platform$endian)
  int_value
}
random_seed <- convert_float_to_int(season + bracket_id / 1000)

# Set random seed
set.seed(random_seed)

# Load probabilities
probabilities <- readRDS("probs.RDS")
probabilities <- select(probabilities, seed_A, seed_B, pwin_A)


# Generate winners
generate_winners <- function(data){
  data %>% 
    left_join(probabilities, by = c("seed_A", "seed_B")) %>% 
    left_join(probabilities, by = c("seed_A" = "seed_B", "seed_B" = "seed_A")) %>% 
    rename(pwin_A = pwin_A.x, pwin_B = pwin_A.y) %>% 
    mutate(pwin_A = coalesce(pwin_A, 1 - pwin_B),
           win_A = rbinom(n(), 1, pwin_A),
           winner = ifelse(win_A == 1, "A", "B"),
           winner_seed = ifelse(win_A, seed_A, seed_B)) 
}

# Simulate single division 
sim_div <- function(division){
  # Simulate round 1 (round of 64)
  round_1 <- tibble(
    round = "Round of 64",
    game = 1:8,
    seed_A = c(1, 8, 5, 4, 6, 3, 7, 2),
    seed_B = 17 - seed_A) %>%
    generate_winners()
  
  # Simulate round 2 (round of 32)
  round_2 <- tibble(
    round = "Round of 32",
    game = 1:4,
    seed_A = round_1$winner_seed[seq(1, 7, 2)],
    seed_B = round_1$winner_seed[seq(2, 8, 2)]) %>% 
    generate_winners()
  
  # Simulate round 3 (sweet 16)
  round_3 <- tibble(
    round = "Sweet 16",
    game = 1:2,
    seed_A = round_2$winner_seed[c(1, 3)],
    seed_B = round_2$winner_seed[c(2, 4)]) %>% 
    generate_winners()
  
  # Simulate round 4 (elite 8)
  round_4 <- tibble(
    round = "Elite 8",
    game = 1,
    seed_A = round_3$winner_seed[1],
    seed_B = round_3$winner_seed[2]) %>% 
    generate_winners()
  
  # Winners 
  bind_rows(
    round_1, 
    round_2, 
    round_3, 
    round_4
  ) %>% 
    mutate(division = division) %>% 
    select(division, round, game, seed_A, seed_B, winner_seed, pwin_A)
}

# Bind data into single bracket 
bracket <- map_dfr(c("East", "West", "South", "Midwest"), sim_div)

# View division winners
filter(bracket, round == "Elite 8")

# Save bracket to file
path <- paste0("brackets/bracket_", 
               season, "-", str_pad(bracket_id, 3, "left", 0), 
               "_seed_", random_seed, 
               ".xlsx")

write.xlsx(bracket, path, zoom = 200, headerStyle = createStyle(textDecoration = "Bold"))


