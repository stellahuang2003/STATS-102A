# this is the tournament for STATS 102A midterm
library(tidyverse)
# all .R submissions of ai_12345678 functions are in the submissions folder 
# when you test yours, make sure it gets sourced
files = list.files(path = "submissions/", pattern = ".R", full.names = TRUE)

# source everyone's file
for (file in files) {
  source(file)
}

ai_bad = function(x, strength = 9, ...) {
  UseMethod("ai_bad")
}
attributes(ai_bad) = list(alt = "Terrible AI")

# here is a competitor that you should definitely win against
ai_bad.battleship = function(x, strength = 9, memory = list()) {
  # The return value of this function should be a character string representing a target square in the
  # opponent’s ocean grid. E.g. "d-6", "i-7", etc.)
  # if strength is 0 return a random field
  if(nrow(x$history) %in% c(0, 1)) {
    return(list(target = "A-1",
                memory = list()))
  }
  # if this is not the first turn then there's a history
  # who am I and who is my opponent
  this_turn = nrow(x$history) + 1
  # did I go first?
  my_turn_odd = this_turn %% 2 == 1
  # am I fleet 1 or 2
  my_fleet_ind = my_turn_odd + 1
  opponent_fleet_ind = (!my_turn_odd) + 1
  opponent_fleet = x$fleets[[opponent_fleet_ind]]
  # all possible fields
  all_options = paste(rep(LETTERS[1:opponent_fleet$ocean[2]], each = opponent_fleet$ocean[1]), 
                      1:opponent_fleet$ocean[1], sep = "-")
  # go through them one after another
  return(list(target = all_options[nrow(x$history) / 2], 
              memory = list()))
}

ai_bad.fleet = function(x, ...) {
  # for (i in seq_along(x$ships)) {
  #   this_ship = x$ships[[i]]
  #   start_pos = paste0(LETTERS[i], "-", 1)
  #   end_pos = paste0(LETTERS[i], "-", this_ship$size)
  #   # assign location to ship
  #   x$ships[[i]]$position = c(start_pos, end_pos)
  # }
  # x
  position_fleet(x)
}

# get competing functions
competitors = lsf.str()
# they have to start with ai_
competitors = competitors[starts_with("ai_", vars = competitors)]
# we do not want the class specific versions 
competitors = competitors[!grepl("\\.", competitors)]
# look at actual competitors
# your ai should appear here, alongside ai_bad
competitors


# collect all information about each competitor
competitor_data = data.frame(competitor = competitors,
                             ID = substr(competitors, 4, 12),
                             alt = sapply(competitors, function(x) {
                               attributes(get(x))$alt
                             }))


# here is my play_bs function
# this function will not get you full credit on the assignment
play_bs = function(players, strengths = c(9, 9), 
                   oceans = list(c(10, 10), c(10, 10)), 
                   fleet_size = list(default_ships(), default_ships())) {
  # The return value for this function is a list object. The minimal contents of this list should be winner =
  #   the name of the admiral who won the game. You may need to include more data in your return object
  # to answer all of the questions in this project.
  
  # initialize a winner
  winner = "undecided"
  
  # initialize a battleship game
  # first ai gets to place its ships
  # you lose if you produce an error
  first_fleet = fleet(admiral = players[1], ocean = c(10, 10), ships = fleet_size[[1]]) %>% 
    get(as.character(players[1]))(strength = strengths[1]) %>%
    tryCatch(error = function(e){
      warning(paste0(players[2], " won because ", players[1], " caused an error."))
      winner <<- players[2]})
  
  # second ai gets to place its ships
  # you lose if you produce an error
  second_fleet = fleet(admiral = players[2], ocean = c(10, 10), ships = fleet_size[[2]]) %>% 
    get(as.character(players[2]))(strength = strengths[2]) %>%
    tryCatch(error = function(e){
      warning(paste0(players[1], " won because ", players[2], " caused an error."))
      winner <<- players[1]})
  this_battleship = battleship(fleets = list(first_fleet, second_fleet))
  
  # initialize a turn counter
  turn = 1
  # initialize a memory
  memories = list(list(), list())
  
  while (winner == "undecided") {
    if (turn %% 2 == 0) {
      current_from = players[2]
      current_to = players[1]
      current_strength = strengths[2]
      current_from_ind = 2
      current_to_ind = 1
    } else {
      current_from = players[1]
      current_to = players[2]
      current_strength = strengths[1]
      current_from_ind = 1
      current_to_ind = 2
    }
    
    # get a target
    this_output = get(as.character(current_from))(x = this_battleship, strength = current_strength, memory = memories[[current_from_ind]])  %>%
      tryCatch(error = function(e){
        warning(paste0(as.character(current_to), " won because ", as.character(current_from), " caused an error."))
        winner <<- as.character(current_to)
        "error"})
    # if your code produces an error the game is over and the other player wins
    if (identical(this_output, "error")) {
      return(list(winner = as.character(winner), turn = turn))
    }
    memories[[current_from_ind]] = this_output$memory
    this_target = this_output$target
    
    # helper function
    position_to_numeric = function(pos) {
      parts <- unlist(strsplit(pos, "-"))
      return(as.numeric(c(which(LETTERS == parts[1]), parts[2])))
    }
    
    # compute if it's a hit or miss
    # update the ships in the fleet
    this_target_pos = position_to_numeric(this_target)
    hit = rep(0, length(this_battleship$fleets[[current_to_ind]]$ships))
    
    for (i in seq_along(this_battleship$fleets[[current_to_ind]]$ships)) {
      # compute all positions of this ship
      ship_start_pos = position_to_numeric(this_battleship$fleets[[current_to_ind]]$ships[[i]]$position[1])
      ship_end_pos = position_to_numeric(this_battleship$fleets[[current_to_ind]]$ships[[i]]$position[2])
      ship_all_pos = cbind(ship_start_pos[1]:ship_end_pos[1],
                           ship_start_pos[2]:ship_end_pos[2])
      
      # Check each row of the matrix
      hits = apply(ship_all_pos, 1, function(row) {
        all(row == this_target_pos)
      })
      # if any ship is hit
      if (any(hits)) {
        hit[i] = which(hits)
        # update opponents fleets ship to be hit
        this_battleship$fleets[[current_to_ind]]$ships[[i]]$hits[hit[i]] = TRUE
        # check if all fields of their ship are hit, then it's sunk
        if (all(this_battleship$fleets[[current_to_ind]]$ships[[i]]$hits)) {
          this_battleship$fleets[[current_to_ind]]$ships[[i]]$sunk = TRUE
          # check which ships are sunk
          is_sunk = sapply(this_battleship$fleets[[current_to_ind]]$ships, function(x) {x$sunk})
          # you win if all of them are sunk
          if (all(is_sunk)) {
            winner = current_from
          }
        }
      }
    }
    # update history
    this_battleship$history[turn,] = tibble(from = current_from, to = current_to, 
                                            target = this_target, hit = any(hit != 0))
    # update turn counter
    # if (turn %% 10 == 1) print(turn)
    print(turn)
    turn = turn + 1
  }
  return(list(winner = as.character(winner), turn = turn - 1))
}

# set up tournament
size = c("default", "small", "large")
tournament = expand.grid(comp1 = competitor_data$competitor, 
                         comp2 = competitor_data$competitor, 
                         size = size)

# I'm not telling you the size of ocean you will compete on 
# default is 10x10
small_ocean = c(10, 10)
large_ocean = c(10, 10)


# I'm not telling you what size the small and large number of ships are
# default is ships of sizes 5, 4, 3, 3, 2
small_fleet = function() {
  default_ships()
}
large_fleet = function() {
  default_ships()
}

# you do not need to compete against yourself
tournament = tournament %>%
  filter(comp1 != comp2)
tournament$winner = "undecided"
tournament$turn = NA
# every combination occurs twice
# you get a rematch with the other player going first

# I might have to draw a random sample of all possible matches
for (i in 1:nrow(tournament)) {
  set.seed(i)
  # set ocean
  oceans = list(c(10, 10), c(10, 10))
  if (tournament$size[i] == "small") oceans = list(small_ocean, small_ocean)
  if (tournament$size[i] == "large") oceans = list(large_ocean, large_ocean)
  # set fleet size
  fleet_size = list(default_ships(), default_ships())
  if (tournament$size[i] == "small") fleet_size = list(small_fleet(), small_fleet())
  if (tournament$size[i] == "small") fleet_size = list(large_fleet(), large_fleet())
  
  # run actual games
  this_game = play_bs(players = c(tournament$comp1[i], tournament$comp2[i]),
                      strengths = c(9, 9),
                      oceans = oceans,
                      fleet_size = fleet_size)
  tournament$winner[i] = this_game$winner
  tournament$turn[i] = this_game$turn
}

# aggregate individual games 
# compute how often which competitor won and compute a ranking
tournament_long = tournament %>% 
  pivot_longer(cols = c("comp1", "comp2"),
               names_to = "firstsecond",
               values_to = "competitor")
tournament_res = tournament_long %>%
  mutate(win = competitor == winner) %>%
  group_by(competitor, size) %>%
  summarise(win_prop = mean(win)) %>%
  group_by(size) %>%
  mutate(rank = rank(-win_prop)) %>%
  arrange(size, rank)
tournament_res

# this is the outcome that will be used for grading
tournament_result = right_join(competitor_data, tournament_res, by = "competitor")

# remove all variables containing UIDs
# this can be shared with all students
tournament_result = tournament_result %>%
  select(-c("competitor", "ID")) %>%
  arrange(size, rank)
tournament_result
