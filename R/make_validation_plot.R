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
#' @param id_var_name character. The name of the ID variable. By default,
#'    simply \code{"id"}.
#' @param plot logical. Whether or not to plot to an output device
#'    straight away, by default \code{TRUE}.
#' @export
make_validation_plot <- function(models, validation_data,
                                 buckets = 10, dep_var_name = "dep_var",
                                 id_var_name = "id", plot = TRUE, ...) {
  stopifnot(is.data.frame(validation_data) || is.numeric(validation_data),
            is.numeric(buckets), is.simple_string(dep_var_name),
            is.simple_string(id_var_name), is.simple_logical(plot))

  if (!is.list(models)) { models <- list(models) }
  models <- lapply(models, validate_model)

  validation_object <-
    make_validation_object(models, validation_data, buckets,
                           dep_var_name, id_var_name)

  if (isTRUE(plot)) {
    output_validation_plot(validation_object, ...)
  }

  invisible(validation_object)
}

#' @param scale numeric. Adjust plot size, by default \code{1}.
#' @param output_type function. The output type for the plot, by default \code{\link[grDevices]{png}}.
#' @param filename character. Path to save output png file (for only a single plot_type).
#' @param ... additional arguments to \code{\link[graphics]{plot}}.
#' @param validation_object validation_object. Internal parameter.
#' @rdname make_validation_plot
output_validation_plot <- function(validation_object, scale = 1, output_type = png,
                                   filename = NULL, ...) {
  stopifnot(is.function(output_type))

  # Plot the results.
  if (!is.null(filename)) {
    output_type(filename, width = scale * 600, height = scale * 500)
    on.exit(dev.off(), add = TRUE)
  }

  plot(validation_object, scale = scale, ...)
}

validate_model <- function(model) {
  if (!is.numeric(model) && !is(model, "tundraContainer")) {
    stop("Currently, only numeric and tundraContainer inputs are ",
         "accepted in the first argument to ",
         sQuote("make_validation_plot"), "; instead, I received a ",
         sQuote(crayon::red(class(model)[1L])))
  }
}

# The following Gini definition is adapted from https://www.kaggle.com/wiki/RCodeForGini
# It uses y_pred to rank y_actual, and generate Lorenz Curve by calculating the cumulative sum of y_act vs y_pred
# And Gini is the sum of diff between Lorenz Curve and the Line of Equality
# Note that the area between the two curves is Gini / length(y_actual)
# And the ratio of this area to the area of perfect inequality is Gini * 2/length(y_actual)
# This is precisely the Gini coefficient (http://en.wikipedia.org/wiki/Gini_coefficient)
SumModelGini <- function(y_actual, y_pred){
  # length(y_actual) == length(y_pred) already checked by function body
  df <- data.frame(y_act = y_actual, y_pred = y_pred)

  df <- df[order(df$y_pred), ]
  df$cumPosFound <- cumsum(df$y_act)
  totalPos <- sum(df$y_act)
  df$Lorentz <- df$cumPosFound / totalPos

  #This 45 degree diagonal reference line is the 'Line of Equality'
  df$random = (1:nrow(df))/nrow(df)

  df$Gini <- df$Lorentz - df$random

  sum(df$Gini)
}

# Normalizion by Gini of perfect prediction
# a.l.a. y_pred preserves the order of y_act, we should expect NormalizedGini(nGini) to be 1
# Thus, this metric is only sensitive to order of the prediction rather than the actual magnitude
# @examples
#      any monotonic transformation (e.g y_pred = y_act^2) will still yield nGini = 1
#      a total reverse of order (e.g. y_act = 1:1000, y_pred = 1000:1) will yield nGini = -1
#      nGini is bounded from [-1, 1]
#      any perturbation of order will shift nGnini downwards from 1
#      a random order (e.g. y_act = 1:1000, y_pred = rnorm(1000) )will yield nGini ~ 0
NormalizedGini <- function(y_actual, y_pred){
  SumModelGini(y_actual, y_pred) / SumModelGini(y_actual, y_actual)
}

# Root-Mean-Square Deviation
RMSD <- function(y_actual, y_pred){
  sqrt(mean((y_actual - y_pred)^2))
}

make_validation_object <- function(scores, ...) {
  UseMethod("make_validation_object")
}

make_validation_object.list <- function(scores, ...) {
  structure(lapply(seq_along(scores), function(i) {
    model_name <- names(scores)[i]                  
    if (model_name == "" || is.null(model_name)) {
      model_name <- paste0("Model ", i)
    }
    make_validation_object(scores[[i]], ..., model_name = model_name)
  }, ...), names = names(scores))
}

make_validation_object.tundraContainer <- function(scores, validation_data, ...) {
  scores <- scores$predict(validation_data)
  make_validation_object(scores, validation_data = validation_data, ...)
}

make_validation_object.numeric <- function(scores, validation_data, buckets,
                                           dep_var_name, id_var_name,
                                           model_name = "Model") {
  # Create S3 class to store validation information.
  validation_object <- structure(list(), class = "validation_object")
  validation_object$model_name <- model_name

  # Get the ordered response (actual and predicted)
  if (length(scores) != NROW(validation_data)) {
    stop("Prediction vector argument of unequal length to ",
         "validation dataframe.")
  }

  # Warn user that something is wrong
  if (all(is.na(scores))) {
    stop("All predictions are NA.")
  } else if (any(is.na(scores))) {
    warning("Some predictions are NA.")
    keep   <- !is.na(scores)
    scores <- scores[keep]
    validation_data <- validation_data[keep, ]
  } else if (length(unique(scores)) == 1) {
    stop("Predictions are all identical!")
  }

  if (length(unique(validation_data[[dep_var_name]])) > 2) {
    # For continuous output, use "normalized Gini" and "normalized RMSD" as error metric
    # Definition:
    #   "nGini" reflects the relative order between y_act and y_pred, its value is bounded between [-1, 1]
    #           a perfect preservation of order will yield nGini = 1, and a total reverse of order will yield nGnini = -1
    #   "nRMSD" is the most common metric for Regression Problems, a perfect prediction will yield nRMSD = 1
    #           In practice the order should ~ 1, if you see nRMSD > 10, you should be worried
    validation_object$NRMSD <- NormalizedRMSD(scores, validation_data[[dep_var_name]])
    validation_object$gini  <- NormalizedGini(scores, validation_data[[dep_var_name]])
  } else {
    # For Classification problem, use ROC as error metric
    # warn if data frame is too large for pROC::ci
    roc <- pROC::ci(factor(validation_data[[dep_var_name]]), scores, of = "auc")

    # compute validation metrics
    validation_object$roc    <- roc[2L]
    validation_object$roc_le <- roc[2L] - roc[1L] # lower error
    validation_object$roc_ue <- roc[3L] - roc[2L] # upper error
  }

  ordered_response <- validation_data[[dep_var_name]][order(scores)]

  normalizer_name <- "blah" # TODO: (RK) Implement.
  if (is.element(normalizer_name, names(validation_data))) {
    ordered_normalizer <- validation_data[[normalizer_name]][order(scores)]
  }
  ordered_preds <- scores[order(scores)]

  # find average in buckets
  bins <- floor(buckets*(seq_along(ordered_preds)-1)/length(ordered_preds))+1
  validation_object$bin_lo <- tapply(ordered_preds, bins, min, na.rm = TRUE)
  validation_object$bin_hi <- tapply(ordered_preds, bins, max, na.rm = TRUE)
  validation_object$bucket_size <- table(bins)
  validation_object$mean_response <- tapply(ordered_response, bins, mean, na.rm=T)
  validation_object$total_response <- tapply(ordered_response, bins, sum, na.rm=T)
  validation_object$mean_prediction <- tapply(ordered_preds, bins, mean, na.rm=T)

  if (is.element(normalizer_name, names(validation_data))) {
    ordered_ids <- validation_data[order(scores), id_var_name]
    groups <- split(ordered_ids, bins)
    validation_object$normalized_mean_response <-
      vapply(groups, function(ids) unlist(Map(weighted.mean,
        list(validation_data[[dep_var_name]][validation_data[[id_var_name]] %in% ids]),
        list(validation_data[[normalizer_name]][validation_data[[id_var_name]] %in% ids])))
      , numeric(1))
    validation_object$normalizer <- tapply(ordered_normalizer, bins, sum, na.rm = TRUE)
  }

  validation_object
}

#' @rdname make_validation_plot
#' @export
validation_plot <- make_validation_plot

