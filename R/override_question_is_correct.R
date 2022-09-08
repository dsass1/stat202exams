#override tutorial completion

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
question_is_correct.checkbox <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.numeric <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}

#' @export
question_is_correct.radio <- function(question, value, ...) {
  learnr::mark_as(FALSE)
}