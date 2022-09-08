
#' Knitr question print methods
#'
#' \code{knitr::\link[knitr]{knit_print}}
#' @inheritParams knitr::knit_print
#' @export
#' @importFrom knitr knit_print
#' @import shiny
#' @import learnr
#' @method knit_print tutorial_question
#' @rdname knit_print
#'
knit_print.tutorial_question <- function(x, ...) {
  question <- x
  ui <- question_module_ui_exam(question$ids$question)

  # too late to try to set a chunk attribute
  # knitr::set_chunkattr(echo = FALSE)
  rmarkdown::shiny_prerendered_chunk(
    'server',
    sprintf(
      'ISDStutorials:::exam_prerendered_chunk(%s, session = session)', #format
      learnr:::dput_to_string(question)
    )
  )

  # regular knit print the UI
  knitr::knit_print(ui)
}


question_module_ui_exam <- function(id) {
  ns <- NS(id)
  div(
    class = "panel panel-default tutorial-question-container",
    div(
      "data-label" = as.character(id),
      class = "tutorial-question panel-body",
      uiOutput(ns("answer_container")),
      #uiOutput(ns("message_container")),
      uiOutput(ns("action_button_container")),
      learnr:::withLearnrMathJax()
    )
  )
}


# internal function to print exam
exam_prerendered_chunk <- function(question, ..., session = getDefaultReactiveDomain()) {
  learnr:::store_question_cache(question)

  question_state <-
    callModule(
      exam_module_server,
      question$ids$question,
      question = question,
      session = session
    )

  observe({
    req(question_state())
    learnr:::set_tutorial_state(question$label, question_state(), session = session)
  })

  question_state
}


exam_module_server <- function(
    input, output, session,
    question
) {

  output$answer_container <- renderUI({
    if (is.null(question$loading)) {
      learnr:::question_ui_loading(question)
    } else {
      div(
        class="loading",
        question$loading
      )
    }
  })

  # Setup reactive here that will be updated by the question modules
  question_state <- reactiveVal()

  observeEvent(
    req(session$userData$learnr_state() == "restored"),
    once = TRUE,
    exam_module_server_impl(input, output, session, question, question_state)
  )

  question_state
}

exam_module_server_impl <- function(
    input, output, session,
    question,
    question_state = NULL
) {

  ns <- getDefaultReactiveDomain()$ns
  # set a seed for each user session for question methods to use
  question$seed <- learnr:::random_seed()

  # only set when a submit button has been pressed
  # (or reset when try again is hit)
  # (or set when restoring)
  submitted_answer <- reactiveVal(NULL, label = "submitted_answer")

  is_correct_info <- reactive(label = "is_correct_info", {
    # question has not been submitted
    if (is.null(submitted_answer())) return(NULL)
    # find out if answer is right
    ret <- question_is_correct(question, submitted_answer())
    #ret <- learnr::mark_as(FALSE)
    if (!inherits(ret, "learnr_mark_as")) {
      stop("`question_is_correct(question, input$answer)` must return a result from `correct`, `incorrect`, or `mark_as`")
    }
    ret
  })

  # should present all messages?
  # is_done <- reactive(label = "is_done", {
  #   if (is.null(is_correct_info())) return(NULL)
  #   (!isTRUE(question$allow_retry)) || is_correct_info()$correct
  # })

  # Never done
  is_done <- reactive(label = "is_done", {
    if (is.null(is_correct_info())) return(NULL)
    FALSE}
    )

  button_type <- reactive(label = "button type", {
    if (is.null(submitted_answer())) {
      "submit"
    } else {
      # is_correct_info() should be valid
      if (is.null(is_correct_info())) {
        stop("`is_correct_info()` is `NULL` in a place it shouldn't be")
      }

      # update the submit button label
      # always try again
      if (is_correct_info()$correct) {
        "correct"
        #"try_again"
      } else {
        # not correct
        if (isTRUE(question$allow_retry)) {
          # not correct, but may try again
          "try_again"
        } else {
          # not correct and can not try again
          "incorrect"
          #"try_again"
        }
      }
    }
  })

  # disable / enable for every input$answer change
  answer_is_valid <- reactive(label = "answer_is_valid", {
    if (is.null(submitted_answer())) {
      question_is_valid(question, input$answer)
    } else {
      question_is_valid(question, submitted_answer())
    }
  })

  init_question <- function(restoreValue = NULL) {
    if (question$random_answer_order) {
      # Shuffle visible answer options (i.e. static, non-function answers)
      is_visible_option <- !learnr:::answer_type_is_function(question$answers)
      question$answers[is_visible_option] <<- learnr:::shuffle(question$answers[is_visible_option])
    }
    submitted_answer(restoreValue)
  }

  # restore past submission
  #  If no prior submission, it returns NULL
  past_submission_answer <- learnr:::retrieve_question_submission_answer(session, question$label)
  # initialize like normal... nothing has been submitted
  #   or
  # initialize with the past answer
  #  this should cascade throughout the app to display correct answers and final outputs
  init_question(past_submission_answer)


  output$action_button_container <- renderUI({
    learnr:::question_button_label(
      question,
      button_type(),
      answer_is_valid()
    )
  })

  output$message_container <- renderUI({
    req(!is.null(is_correct_info()), !is.null(is_done()))

    learnr:::withLearnrMathJax(
      learnr:::question_messages(
        question,
        messages = is_correct_info()$messages,
        is_correct = is_correct_info()$correct,
        is_done = is_done()
      )
    )
  })

  output$answer_container <- renderUI({
    if (is.null(submitted_answer())) {
      # has not submitted, show regular answers
      return(
        # if there is an existing input$answer, display it.
        # if there is no answer... init with NULL
        # Do not re-render the UI for every input$answer change
        learnr:::withLearnrMathJax(
          question_ui_initialize(question, isolate(input$answer))
        )
      )
    }

    # has submitted

    if (is.null(is_done())) {
      # has not initialized
      return(NULL)
    }

    # if (is_done()) {
    #   # if the question is 'done', display the final input ui and disable everything
    #
    #   return(
    #     learnr:::withLearnrMathJax(
    #       question_ui_completed(question, submitted_answer())
    #     )
    #   )
    # }

    # if the question is NOT 'done', disable the current UI
    #   until it is reset with the try again button

    return(
      learnr:::withLearnrMathJax(
        learnr::question_ui_try_again(question, submitted_answer())
      )
    )
  })


  observeEvent(input$action_button, {

    if (button_type() == "try_again") {
      # maintain current submission / do not randomize answer order
      # only reset the submitted answers
      # does NOT reset input$answer
      submitted_answer(NULL)

      # submit "reset" to server
      learnr:::event_trigger(
        session,
        "reset_question_submission",
        data = list(
          label    = as.character(question$label),
          question = as.character(question$question)
        )
      )
      return()
    }

    submitted_answer(input$answer)

    # submit question to server
    learnr:::event_trigger(
      session = session,
      event   = "question_submission",
      data    = list(
        label    = as.character(question$label),
        question = as.character(question$question),
        answer   = as.character(input$answer),
        correct  = is_correct_info()$correct
      )
    )

  })

  observe({
    # Update the `question_state()` reactive to report state back to the Shiny session
    req(submitted_answer(), is.reactive(question_state))
    current_answer_state <- list(
      type = "question",
      answer = submitted_answer(),
      correct = is_correct_info()$correct
    )
    question_state(current_answer_state)
  })
}
