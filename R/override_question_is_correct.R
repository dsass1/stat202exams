#override tutorial completion
#' @importFrom learnr question_ui_initialize
#' @importFrom learnr question_ui_completed
#' @importFrom learnr question_ui_try_again
#' @importFrom learnr question_is_valid
#' @import learnr
#' @import ISDStutorials
#' 
#' 
# Override to mark every question as wrong
#' @export
question_is_correct.blank <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.wordbank <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.multidrop <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.matching <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.learnr_checkbox <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.learnr_numeric <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.learnr_radio <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}