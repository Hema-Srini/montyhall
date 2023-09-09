#' Create a new Monty Hall Problem game.
#'
#' Generates a new game that consists of two doors with goats behind them,
#' and one with a car.
#'
#' @return A length 3 character vector indicating the positions of goats and the car.
#'
#' @examples
#' create_game()
#'
#' @export
create_game <- function() {
  a.game <- sample(x = c("goat", "goat", "car"), size = 3, replace = FALSE)
  return(a.game)
}

#' Select a door.
#'
#' @return A number between 1 and 3 representing the chosen door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function() {
  doors <- c(1, 2, 3)
  a.pick <- sample(doors, size = 1)
  return(a.pick)
}

#' Open a goat door.
#'
#' @param game The current game setup.
#' @param a.pick The contestant's selected door.
#'
#' @return A number between 1 and 3 representing the opened door.
#'
#' @examples
#' open_goat_door(game = c("goat", "car", "goat"), a.pick = 1)
#'
#' @export
open_goat_door <- function(game, a.pick) {
  doors <- c(1, 2, 3)
  if (game[a.pick] == "car") {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  }
  if (game[a.pick] == "goat") {
    opened.door <- doors[game != "car" & doors != a.pick]
  }
  return(opened.door)
}

#' Change the chosen door.
#'
#' @param stay Boolean indicating whether to stay with the original choice.
#' @param opened.door The door that was opened to reveal a goat.
#' @param a.pick The original choice.
#'
#' @return A number between 1 and 3 representing the final chosen door.
#'
#' @examples
#' change_door(stay = TRUE, opened.door = 2, a.pick = 1)
#'
#' @export
change_door <- function(stay, opened.door, a.pick) {
  doors <- c(1, 2, 3)
  if (stay) {
    final.pick <- a.pick
  }
  if (!stay) {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }
  return(final.pick)
}

#' Determine the game outcome.
#'
#' @param final.pick The final chosen door.
#' @param game The current game setup.
#'
#' @return "WIN" if the final pick is the car, "LOSE" if it's a goat.
#'
#' @examples
#' determine_winner(final.pick = 2, game = c("goat", "car", "goat"))
#'
#' @export
determine_winner <- function(final.pick, game) {
  if (game[final.pick] == "car") {
    return("WIN")
  }
  if (game[final.pick] == "goat") {
    return("LOSE")
  }
}

#' Play a single Monty Hall game.
#'
#' @return A data frame with two rows, one for staying and one for switching strategies,
#' each containing the strategy and outcome.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function() {
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)
  final.pick.stay <- change_door(stay = TRUE, opened.door, first.pick)
  final.pick.switch <- change_door(stay = FALSE, opened.door, first.pick)
  outcome.stay <- determine_winner(final.pick.stay, new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)
  strategy <- c("stay", "switch")
  outcome <- c(outcome.stay, outcome.switch)
  game.results <- data.frame(strategy, outcome, stringsAsFactors = FALSE)
  return(game.results)
}

#' Play multiple Monty Hall games.
#'
#' @param n The number of games to play.
#'
#' @return A data frame summarizing the outcomes of all games played.
#'
#' @examples
#' play_n_games(100)
#'
#' @export
play_n_games <- function(n = 100) {
  library(dplyr)
  results.list <- list()
  loop.count <- 1
  for (i in 1:n) {
    game.outcome <- play_game()
    results.list[[loop.count]] <- game.outcome
    loop.count <- loop.count + 1
  }
  results.df <- dplyr::bind_rows(results.list)
  table(results.df) %>%
    prop.table(margin = 1) %>%
    round(2) %>%
    print()
  return(results.df)
}
