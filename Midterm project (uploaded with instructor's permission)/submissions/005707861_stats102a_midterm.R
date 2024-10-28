# 005707861_stats102a_midterm
ai_005707861 = function(x, ...) {
  UseMethod("ai_005707861")
}

# x = this_battleship, strength = current_strength, memory = memories[[current_from_ind]]
# tournament file uses these arguments

ai_005707861.battleship <- function(x, strength = 9, memory = list()) { 
  
  # result <- ai_005707861(game, game$history, game$fleets[[opponent_player]]$ocean,
  #                                                strengths[current_player], memory[[current_player]])
  
  # opponent's ocean
  ocean <- x$fleets[[ifelse(nrow(x$history) %% 2 == 0, 1, 2)]]$ocean
  possible_targets <- expand.grid(row = LETTERS[1:ocean[1]],
                                  col = 1:ocean[2])
  possible_targets <- with(possible_targets, paste(row, col, sep = "-"))
  
  history <- x$history %>%
    filter((nrow(x$history) %% 2 == 0 & row_number() %% 2 == 1) |
           (nrow(x$history) %% 2 == 1 & row_number() %% 2 == 0))
  
  if (nrow(history) > 0 && history$hit[nrow(history)]) {
    memory$mode <- "attack"
    memory$last_hit <- history$target[nrow(history)]
    if (!is.null(memory$tried_directions) && length(memory$tried_directions) != 0) {
      memory$successful_direction <- memory$tried_directions[length(memory$tried_directions)]
    } else {
      memory$tried_directions <- character(0)
    }
  }
  
  # remove previously targeted cells
  previous_targets <- history$target
  possible_targets <- possible_targets[!(possible_targets %in% previous_targets)]
  
  # helper function
  get_target_in_direction <- function(last_hit, direction) {
    row <- substr(last_hit, 1, 1)
    col <- as.numeric(substr(last_hit, 3, nchar(last_hit)))
    if (direction == "up") {
      row <- LETTERS[match(row, LETTERS) - 1]
    } else if (direction == "down") {
      row <- LETTERS[match(row, LETTERS) + 1]
    } else if (direction == "left") {
      col <- col - 1
    } else if (direction == "right") {
      col <- col + 1
    }
    paste(row, col, sep = "-")
  }
  
  if (is.null(memory$mode) || memory$mode == "search" || strength == 0) {
    # search mode: randomly choose a target
    target <- sample(possible_targets, 1)
    memory <- list(mode = "search", last_hit = target, tried_directions = character(0))
  } else {
    # attack mode
    if (!is.null(memory$successful_direction)) {
      # continue attacking in the successful direction
      target <- get_target_in_direction(memory$last_hit, memory$successful_direction)
      if (target %in% previous_targets || !(target %in% possible_targets)) {
        # if target already hit or out of bounds, switch back to search mode
        memory$successful_direction <- NULL
        target <- sample(possible_targets, 1)
        memory <- list(mode = "search", last_hit = target, tried_directions = character(0))
      }
    } else {
      # try attacking in different directions until successful
      last_hit <- memory$last_hit
      tried_directions <- memory$tried_directions
      directions <- c("up", "down", "left", "right")
      directions <- setdiff(directions, tried_directions)
      
      # eliminate out of bounds cases
      if (match(substr(last_hit, 1, 1), LETTERS) == 1)
        directions <- directions[directions != "up"]
      if (match(substr(last_hit, 1, 1), LETTERS) == ocean[1])
        directions <- directions[directions != "down"]
      if (as.numeric(substr(last_hit, 3, nchar(last_hit))) == 1)
        directions <- directions[directions != "left"]
      if (as.numeric(substr(last_hit, 3, nchar(last_hit))) == ocean[2])
        directions <- directions[directions != "right"]
      
      if (length(directions) == 0) {
        # if all directions have been tried, switch back to search mode
        target <- sample(possible_targets, 1)
        memory <- list(mode = "search", last_hit = target, tried_directions = character(0))
      } else {
        direction <- sample(directions, 1)
        target <- get_target_in_direction(last_hit, direction)
        if (target %in% previous_targets || !(target %in% possible_targets)) {
          # if target already hit or out of bounds, switch back to search mode
          target <- sample(possible_targets, 1)
          memory <- list(mode = "search", last_hit = target, tried_directions = character(0))
        }
        memory$tried_directions <- c(memory$tried_directions, direction)
      }
    }
  }
  list(target = target, memory = memory)
}

ai_005707861.fleet = function(x, ...) {
  # helper functions
  pos_to_label <- function(row, col) {
    letter <- LETTERS[row]
    paste0(letter, "-", col)
  }
  
  generate_coords <- function(size, ocean) {
    # coord elements are vectors of length ship$size, not just front and back
    coords <- list()
    # generate horizontal
    for (i in seq_len(ocean[1])) {
      # iterate thru all rows
      for (j in seq_len(ocean[2] - size + 1)) {
        # iterate thru all possible cols
        coords <-
          c(coords, list(sapply(j:(j + size - 1), function(col) {
            pos_to_label(i, col)
          })))
      }
    }
    # generate vertical
    for (j in seq_len(ocean[2])) {
      for (i in seq_len(ocean[1] - size + 1)) {
        coords <- c(coords, list(sapply(i:(i + size - 1), function(row) {
          pos_to_label(row, j)
        })))
      }
    }
    coords
  }
  position_fleet <- function(fleet, positions = NULL) {
    if (is.null(positions)) {
      ocean <- fleet$ocean
      while(TRUE) {
        positions <- list()
        used_coords <- character(0)
        for (s in fleet$ships) {
          not_placeable <- FALSE # to account for cases where not all ships are successfully placed
          size <- s$size
          coord_candidates <- generate_coords(size, ocean)
          
          while (TRUE) {
            chosen_coords_index <- sample(1:length(coord_candidates), 1)
            chosen_coords <- coord_candidates[[chosen_coords_index]]
            if (!any(chosen_coords %in% used_coords)) { # ship is placeable
              used_coords <- c(used_coords, chosen_coords)
              positions <- c(positions, list(c(chosen_coords[1], chosen_coords[size])))
              break
            } else { # update coord_candidates to remove unusable set of coords
              coord_candidates[[chosen_coords_index]] <- NULL
              if (length(coord_candidates) == 0) {
                not_placeable <- TRUE # allows to subsequently break out of for loop
                break # break out of while loop
              }
            }
          }
          if (not_placeable) {
            break
          }
        }
        if (length(positions) == length(fleet$ships)) { # good to go, if not then continue in while loop
          break
        }
      }
      
    }
    for (i in seq_along(fleet$ships)) {
      fleet$ships[[i]]$position <- positions[[i]]
    }
    fleet
  }
  position_fleet(x)
}

attributes(ai_005707861) = list(alt = "SH")
ai_005707861