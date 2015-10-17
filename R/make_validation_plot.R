#' Create a plot representing a Hosmer-Lemeshow goodness-of-fit test.
#'
#' @param models list or numeric. If a numeric vector, this will be
#'    interpreted as a vector of risk scores to compare to a
#'    dependent variable. If a \code{tundraContainer} (see the
#'    tundra package), the second argument should be a validation
#'    data set to be scored ad-hoc with the model object.
#'    If a \emph{named} list is passed, multiple validation graphs
#'    will be overlayed onto one plot; this list can heterogeneously
#'    consist of numeric vectors or scores or \code{tundraContainer}
#'    model objects.
#' @param validation_data data.frame or integer. If a \code{data.frame},
#'    the column given by \code{dep_var_name} (by default "dep_var")
#'    will be extracted and used as the empirical signal (0 or 1).
#'    For each bucket (see the \code{buckets} parameter), a comparison
#'    of the mean score in that quantile to the empirical mean 
#'    of the dependent variable will be graphed. An ideal classifier
#'    will fully separate the positive and negative cases and look
#'    like the Hamiltonian step function jumping from 0 to 1 after
#'    some cutoff.
#'
#'    If an integer is passed, this will be assumed to be the dependent
#'    variable in the same order as the scores given by the \code{models}
#'    parameter (if it is also a numeric vector). Note that one cannot
#'    pass model objects, that is \code{tundraContainers}, in the 
#'    first parameter \code{models} if \code{validation_data} is a vector.
#' @param buckets integer. The number of cuts (by default, 10).
#' @param dep_var_name character. The name of the dependent variable.
#'    This will be used to extract a column out of the \code{validation_data}
#'    for comparison against the fitted risk scores. By default,
#'    \code{"dep_var"}.
#' @export
make_validation_plot <- function(models, validation_data,
                                 buckets = 10, dep_var_name = "dep_var") {
  
}

#' @rdname make_validation_plot
#' @export
validation_plot <- make_validation_plot

