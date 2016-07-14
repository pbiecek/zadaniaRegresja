#' @title The Regression Game
#'
#' @description
#' The \code{regression} function is used for solving problems in the data-based
#' game "The regression Game".
#'
#' @param ... \code{regression} function is called by different arguments, which
#' vary depending on a problem that Beta and Bit are trying to solve. See
#' \code{Details} in order to learn more about the list of possible arguments.
#'
#' @details Every time when some additional hints are needed one should add
#' \code{hint = TRUE} argument to the \code{regression} function.
#'
#' In this game you are helping a Professor Pearson.
#' You can communicate with him through \code{regression} function.
#'
#' In each call add \code{subject} parameter that will indicate which message
#' you are answering. Add \code{content} parameter. It's value should match the
#' information that the professor has asked you to provide him.
#'
#' Data used in the game comes frome the study of Polish upper-secondary
#' schools first grade students. It was conducted firstly in the same time as
#' PISA 2009 study, with use of the same cognitive tests and questionnaires as
#' in PISA 2009, but on a different group of students (in Poland most of the
#' students in a PISA sample attends lower-secondary schools). The students who
#' participated in the first wave of the study were followed in the 2nd grade of
#' upper-secondary school within the reserach program \emph{Our further study
#' and work} (\emph{Nasza Dalsza Nauka i Praca}). Both studies were conducted by
#' the Institute of Philosophy and Sociology Polish Academy of Sciences.
#' \strong{The original data was changed a little, to better fit the purpose of
#' the game.}
#'
#' "The Regression Game" is a free of charge, educational project of the
#' SmarterPoland.pl Foundation.
#' @author
#' Tomasz Żółtak - the idea and the implementation,
#' Przemyslaw Biecek - comments and the integration with this package.
#'
#' @examples
#' regression()
#' regression(hint = TRUE)
#' @rdname regression
#' @export
regression <- function(...) {
  args = list(...)
  state = FALSE

  return(invisible(state))
}
