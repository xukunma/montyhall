#' Create a Monty Hall game
#'
#' Initialize a new Monty Hall game with one car and two goats hidden behind three doors.
#'
#' @return A character vector of length 3, with one "car" and two "goat" (order randomized).
#' @examples
#' g <- create_game()
#' length(g) == 3
#' @export
create_game <- function() {
  doors <- c("goat", "goat", "car")
  sample(doors)
}

#' Player selects a door
#'
#' Player makes the initial choice of one of the three doors.
#'
#' @param door Integer in 1:3, the chosen door number.
#' @return Integer in 1:3, the selected door number.
#' @examples
#' pick <- select_door(1)
#' pick
#' @export
select_door <- function(door) {
  stopifnot(length(door) == 1L, door %in% 1:3)
  as.integer(door)
}

#' Open a goat door (always 1 door)
#'
#' Reveal a door that has a goat and is not the player's initial choice.
#'
#' @param game Character vector from [create_game()].
#' @param first_pick Integer in 1:3, the player's initial door selection.
#' @return Integer (length 1) giving the door opened to reveal a goat.
#' @examples
#' g <- create_game(); open_goat_door(g, 1)
#' @export
open_goat_door <- function(game, first_pick) {
  stopifnot(is.character(game), length(game) == 3L,
            length(first_pick) == 1L, first_pick %in% 1:3)
  candidates <- setdiff(1:3, first_pick)
  goats <- candidates[game[candidates] == "goat"]
  as.integer(sample(goats, size = 1))
}

#' Change door after reveal (always 1 door)
#'
#' After a goat door is revealed, switch to the remaining unopened door.
#'
#' @param first_pick Integer in 1:3, the initial door selection.
#' @param opened_door Integer in 1:3, the revealed goat door.
#' @return Integer (length 1), the switched door number.
#' @examples
#' change_door(1, 2)
#' @export
change_door <- function(first_pick, opened_door) {
  stopifnot(length(first_pick) == 1L, length(opened_door) == 1L,
            first_pick %in% 1:3, opened_door %in% 1:3, first_pick != opened_door)
  as.integer(setdiff(1:3, c(first_pick, opened_door))[1])
}

#' Determine winner
#'
#' Check whether the player's final pick wins the car.
#'
#' @param game Character vector from [create_game()].
#' @param final_pick Integer in 1:3, the player's final door choice.
#' @return Logical, `TRUE` if the player wins the car, `FALSE` otherwise.
#' @examples
#' g <- create_game()
#' determine_winner(g, 1)
#' @export
determine_winner <- function(game, final_pick) {
  stopifnot(is.character(game), length(game) == 3L,
            length(final_pick) == 1L, final_pick %in% 1:3)
  isTRUE(game[final_pick] == "car")
}

#' Play one Monty Hall game (force scalar picks)
#'
#' Simulate playing the Monty Hall game once, with or without switching.
#'
#' @param switch Logical, `TRUE` to switch after reveal, `FALSE` to stay.
#' @return Logical (length 1), `TRUE` if the player wins the car, `FALSE` otherwise.
#' @examples
#' play_game(TRUE); play_game(FALSE)
#' @export
play_game <- function(switch = TRUE) {
  game <- create_game()
  first_pick <- as.integer(select_door(sample(1:3, 1))[1])
  opened <- as.integer(open_goat_door(game, first_pick)[1])
  final_pick <- if (isTRUE(switch)) {
    as.integer(change_door(first_pick, opened)[1])
  } else {
    first_pick
  }
  isTRUE(determine_winner(game, final_pick))
}

#' Play multiple Monty Hall games
#'
#' Simulate many games to estimate the winning probability with stay/switch strategy.
#'
#' @param n Integer, number of games to play (> 0).
#' @param switch Logical, `TRUE` to switch, `FALSE` to stay.
#' @return Numeric proportion of wins in `[0, 1]`.
#' @examples
#' \donttest{
#'   simulate_games(100, switch = TRUE)
#'   simulate_games(100, switch = FALSE)
#' }
#' @export
simulate_games <- function(n = 1000, switch = TRUE) {
  stopifnot(is.numeric(n), length(n) == 1L, n > 0)
  wins <- vapply(seq_len(n), function(i) isTRUE(play_game(switch)), logical(1))
  mean(wins)
}

#' Play multiple games (alias)
#'
#' A thin wrapper for [simulate_games()] kept for compatibility with templates.
#'
#' @inheritParams simulate_games
#' @return Numeric proportion of wins.
#' @examples
#' \donttest{
#'   play_n_games(100, TRUE)
#' }
#' @export
play_n_games <- function(n = 1000, switch = TRUE) {
  simulate_games(n = n, switch = switch)
}
