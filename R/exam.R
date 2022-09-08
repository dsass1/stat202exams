
#' Knitr quiz print methods
#'
#' \code{knitr::\link[knitr]{knit_print}} methods for \code{\link{question}} and \code{\link{quiz}}
#' @inheritParams knitr::knit_print
#' @export
#' @importFrom knitr knit_print
#' @import shiny
#' @import learnr
#' @method knit_print tutorial_question
#' @rdname knit_print
knit_print.tutorial_question <- function(x, ...) {
  question <- x
  ui <- question_module_ui(question$ids$question)
  
  # too late to try to set a chunk attribute
  # knitr::set_chunkattr(echo = FALSE)
  rmarkdown::shiny_prerendered_chunk(
    'server',
    sprintf(
      'learnr:::question_prerendered_chunk(%s, session = session)',
      dput_to_string(question)
    )
  )
  
  # regular knit print the UI
  knitr::knit_print(ui)
}
#' @method knit_print tutorial_quiz
#' @export
#' @rdname knit_print
knit_print.tutorial_quiz <- function(x, ...) {
  quiz <- x
  caption_tag <- if (!is.null(quiz$caption)) {
    list(knitr::knit_print(
      tags$div(class = "panel-heading tutorial-quiz-title", quiz$caption)
    ))
  }
  
  append(
    caption_tag,
    lapply(quiz$questions, knitr::knit_print)
  )
}


retrieve_all_question_submissions <- function(session) {
  state_objects <- learnr:::get_all_state_objects(session, exercise_output = FALSE)
  
  # create submissions from state objects
  submissions <- learnr:::submissions_from_state_objects(state_objects)
  
  submissions
}

retrieve_question_submission_answer <- function(session, question_label) {
  question_label <- as.character(question_label)
  
  for (submission in retrieve_all_question_submissions(session)) {
    if (identical(as.character(submission$id), question_label)) {
      return(submission$data$answer)
    }
  }
  return(NULL)
}


question_prerendered_chunk <- function(question, ..., session = getDefaultReactiveDomain()) {
  learnr:::store_question_cache(question)
  
  question_state <-
    callModule(
      question_module_server,
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

question_module_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "panel panel-default tutorial-question-container",
    div(
      "data-label" = as.character(id),
      class = "tutorial-question panel-body",
      uiOutput(ns("answer_container")),
      uiOutput(ns("message_container")),
      uiOutput(ns("action_button_container")),
      learnr:::withLearnrMathJax()
    )
  )
}

question_module_server <- function(
    input, output, session,
    question
) {
  
  output$answer_container <- renderUI({
    if (is.null(question$loading)) {
      question_ui_loading(question)
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
    question_module_server_impl(input, output, session, question, question_state)
  )
  
  question_state
}

question_module_server_impl <- function(
    input, output, session,
    question,
    question_state = NULL
) {
  
  ns <- getDefaultReactiveDomain()$ns
  # set a seed for each user session for question methods to use
  question$seed <- random_seed()
  
  # only set when a submit button has been pressed
  # (or reset when try again is hit)
  # (or set when restoring)
  submitted_answer <- reactiveVal(NULL, label = "submitted_answer")
  
  is_correct_info <- reactive(label = "is_correct_info", {
    # question has not been submitted
    if (is.null(submitted_answer())) return(NULL)
    # find out if answer is right
    ret <- question_is_correct(question, submitted_answer())
    if (!inherits(ret, "learnr_mark_as")) {
      stop("`question_is_correct(question, input$answer)` must return a result from `correct`, `incorrect`, or `mark_as`")
    }
    ret
  })
  
  # should present all messages?
  is_done <- reactive(label = "is_done", {
    if (is.null(is_correct_info())) return(NULL)
    FALSE
    #DOES NOT APPEAR TO CHANGE ANYTHING
    #(!isTRUE(question$allow_retry)) || is_correct_info()$correct
  })
  
  
  button_type <- reactive(label = "button type", {
    if (is.null(submitted_answer())) {
      "submit"
    } else {
      # is_correct_info() should be valid
      if (is.null(is_correct_info())) {
        stop("`is_correct_info()` is `NULL` in a place it shouldn't be")
      }
      
      # update the submit button label
      if (is_correct_info()$correct) {
        "try_again"
        #"correct"
      } else {
        # not correct
        if (isTRUE(question$allow_retry)) {
          # not correct, but may try again
          "try_again"
        } else {
          # not correct and can not try again
          #"incorrect"
          "try_again"
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
      is_visible_option <- !answer_type_is_function(question$answers)
      question$answers[is_visible_option] <<- shuffle(question$answers[is_visible_option])
    }
    submitted_answer(restoreValue)
  }
  
  # restore past submission
  #  If no prior submission, it returns NULL
  past_submission_answer <- retrieve_question_submission_answer(session, question$label)
  # initialize like normal... nothing has been submitted
  #   or
  # initialize with the past answer
  #  this should cascade throughout the app to display correct answers and final outputs
  init_question(past_submission_answer)
  
  
  output$action_button_container <- renderUI({
    question_button_label(
      question,
      button_type(),
      answer_is_valid()
    )
  })
  
  output$message_container <- renderUI({
    req(!is.null(is_correct_info()), !is.null(is_done()))
    
    learnr:::withLearnrMathJax(
      question_messages(
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
    
    if (is_done()) {
      # if the question is 'done', display the final input ui and disable everything
      
      return(
        learnr:::withLearnrMathJax(
          question_ui_completed(question, submitted_answer())
        )
      )
    }
    
    # if the question is NOT 'done', disable the current UI
    #   until it is reset with the try again button
    
    return(
      learnr:::withLearnrMathJax(
        question_ui_try_again(question, submitted_answer())
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
      event_trigger(
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
    event_trigger(
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



question_button_label <- function(question, label_type = "submit", is_valid = TRUE) {
  label_type <- match.arg(label_type, c("submit", "try_again", "correct", "incorrect"))
  
  if (label_type %in% c("correct", "incorrect")) {
    # No button when answer is correct or incorrect (wrong without try again)
    return(NULL)
  }
  
  button_label <- question$button_labels[[label_type]]
  is_valid <- isTRUE(is_valid)
  
  default_class <- "btn-primary"
  warning_class <- "btn-warning"
  
  action_button_id <- NS(question$ids$question)("action_button")
  
  if (label_type == "submit") {
    button <- actionButton(
      action_button_id, button_label,
      class = default_class
    )
    if (!is_valid) {
      button <- disable_all_tags(button)
    }
    button
  } else if (label_type == "try_again") {
    mutate_tags(
      actionButton(
        action_button_id, button_label,
        class = warning_class
      ),
      paste0("#", action_button_id),
      function(ele) {
        ele$attribs$class <- str_remove(ele$attribs$class, "\\s+btn-default")
        ele
      }
    )
  }
}

question_messages <- function(question, messages, is_correct, is_done) {
  
  # Always display the incorrect, correct, or try again messages
  default_message <-
    if (is_correct) {
      question$messages$correct
    } else {
      # not correct
      if (is_done) {
        question$messages$incorrect
      } else {
        question$messages$try_again
      }
    }
  
  if (!is.null(messages)) {
    if (!is.list(messages)) {
      # turn vectors into lists
      messages <- tagList(messages)
    }
  }
  
  # display the default messages first
  if (!is.null(default_message)) {
    if (!is.null(messages)) {
      messages <- tagList(default_message, messages)
    } else {
      messages <- default_message
    }
  }
  
  # get regular message
  if (is.null(messages)) {
    message_alert <- NULL
  } else {
    alert_class <- if (is_correct) "alert-success" else "alert-danger"
    if (length(messages) > 1) {
      # add breaks inbetween similar messages
      break_tag <- list(tags$br(), tags$br())
      all_messages <- replicate(length(messages) * 2 - 1, {break_tag}, simplify = FALSE)
      # store in all _odd_ positions
      all_messages[(seq_along(messages) * 2) - 1] <- messages
      messages <- tagList(all_messages)
    }
    message_alert <- tags$div(
      class = paste0("alert ", alert_class),
      messages
    )
  }
  
  
  if (is.null(question$messages$message)) {
    always_message_alert <- NULL
  } else {
    always_message_alert <- tags$div(
      class = "alert alert-info",
      question$messages$message
    )
  }
  
  # get post question message only if the question is done
  if (isTRUE(is_done) && !is.null(question$messages$post_message)) {
    post_alert <- tags$div(
      class = "alert alert-info",
      question$messages$post_message
    )
  } else {
    post_alert <- NULL
  }
  
  # set UI message
  if (all(
    is.null(message_alert),
    is.null(always_message_alert),
    is.null(post_alert)
  )) {
    NULL
  } else {
    htmltools::tagList(message_alert, always_message_alert, post_alert)
  }
}

question_ui_loading <- function(question) {
  n_paragraphs <- max(length(str_match_all(question$question, "</p>")), 1)
  paras <- lapply(seq_len(n_paragraphs), function(...) {
    spans <- lapply(seq_len(sample(2:8, 1)), function(...) {
      htmltools::span(class = sprintf("placeholder col-%s", sample(2:7, 1)))
    })
    htmltools::p(spans)
  })
  
  q_opts <- NULL
  if (length(intersect(question$type, c("learnr_radio", "learnr_checkbox"))) > 0) {
    q_opts <- htmltools::tags$ul(
      lapply(seq_along(question$answers), function(...) {
        htmltools::tags$li(
          htmltools::span(class = "placeholder col-3")
        )
      })
    )
  }
  
  button <- htmltools::tags$a(
    href = "#",
    tabindex = "-1",
    class = "btn btn-primary disabled placeholder col-3",
    `aria-hidden` = "true"
  )
  
  htmltools::div(
    class = "loading placeholder-glow",
    paras,
    q_opts,
    button
  )
}
