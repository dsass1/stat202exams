#override tutorial completion
# @importFrom learnr question_is_correct
#' @import learnr
#' @import ISDStutorials
#' 
#' 
# Override to mark every question as wrong


#' @export
question_is_correct.default <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.blank <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.wordbank <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.multidrop <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.matching <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.learnr_checkbox <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.learnr_numeric <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}

#' @export
question_is_correct.learnr_radio <- function(question, value, ...) {
  return(learnr::mark_as(FALSE))
}
