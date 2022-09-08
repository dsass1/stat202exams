#override tutorial completion
#de @importFrom learnr question_is_correct
#' @import learnr
#' @import ISDStutorials
#' 
#' 
# Override to mark every question as wrong
#' @export
question_is_correct.default <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

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
