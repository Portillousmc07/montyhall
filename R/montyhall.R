#' @title Create a new Monty Hall Problem game.
#' @description `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#' @details The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. This simulation tests both strategies.
#' @return A length-3 character vector indicating the positions of goats and the car.
#' @examples
#'   create_game()
#' @export
create_game <- function() {
  a.game <- sample(x = c("goat","goat","car"), size = 3, replace = FALSE)
  return(a.game)
}

#' @title Select an initial door
#' @description Randomly select one of doors 1, 2, or 3 as the player's first pick.
#' @return Integer 1, 2, or 3.
#' @examples
#'   set.seed(1); select_door()
#' @export
select_door <- function() {
  sample(c(1,2,3), size = 1)
}

#' @title Host opens a goat door
#' @description Opens a door that is not the player's pick and has a goat.
#' @param game Character vector from `create_game()`.
#' @param a.pick Integer (1:3) initial player pick.
#' @return Integer (1:3) for the opened door.
#' @examples
#'   set.seed(1); g <- create_game(); fp <- select_door(); open_goat_door(g, fp)
#' @export
open_goat_door <- function(game, a.pick) {
  doors <- c(1,2,3)
  if (game[a.pick] == "car") {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  }
  if (game[a.pick] == "goat") {
    opened.door <- doors[game != "car" & doors != a.pick]
  }
  opened.door
}

#' @title Stay or switch to the other unopened door
#' @description Computes the final pick given the strategy.
#' @param stay Logical. TRUE = stay; FALSE = switch.
#' @param opened.door Integer (1:3) opened by the host.
#' @param a.pick Integer (1:3) initial pick.
#' @return Integer (1:3) final chosen door.
#' @examples
#'   change_door(stay=TRUE,  opened.door=2, a.pick=1)
#'   change_door(stay=FALSE, opened.door=2, a.pick=1)
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick) {
  doors <- c(1,2,3)
  if (stay) return(a.pick)
  doors[doors != opened.door & doors != a.pick]
}

#' @title Determine the outcome (WIN/LOSE)
#' @description Compares the final pick against the game setup.
#' @param final.pick Integer (1:3) final door selection.
#' @param game Character vector from `create_game()`.
#' @return "WIN" if car; otherwise "LOSE".
#' @examples
#'   determine_winner(1, c("car","goat","goat"))
#' @export
determine_winner <- function(final.pick, game) {
  if (game[final.pick] == "car") return("WIN")
  if (game[final.pick] == "goat") return("LOSE")
}

#' @title Play one game under both strategies
#' @description Plays a single game and returns outcomes for stay and switch.
#' @return A data.frame with columns `strategy` and `outcome`.
#' @examples
#'   set.seed(42); play_game()
#' @export
play_game <- function() {
  new.game    <- create_game()
  first.pick  <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)

  final.stay    <- change_door(stay = TRUE,  opened.door, first.pick)
  final.switch  <- change_door(stay = FALSE, opened.door, first.pick)

  outcome.stay   <- determine_winner(final.stay,   new.game)
  outcome.switch <- determine_winner(final.switch, new.game)

  data.frame(
    strategy = c("stay","switch"),
    outcome  = c(outcome.stay, outcome.switch),
    stringsAsFactors = FALSE
  )
}

#' @title Simulate many games and summarize
#' @description Runs `n` games, prints win proportions by strategy, returns all rows.
#' @param n Integer number of games (default 100).
#' @return A data.frame with 2*n rows (stay and switch per game).
#' @examples
#'   set.seed(1); results <- play_n_games(20); head(results)
#' @export
play_n_games <- function(n = 100) {
  results.list <- vector("list", n)
  for (i in seq_len(n)) results.list[[i]] <- play_game()
  results.df <- do.call(rbind, results.list)
  tab <- prop.table(table(results.df), margin = 1)
  print(round(tab, 2))
  results.df
}
