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

  validation_object <- structure(class = "validation_object_list",
    make_validation_object(models, validation_data, buckets,
                           dep_var_name, id_var_name)
  )

  if (isTRUE(plot)) {
    output_validation_plot(validation_object, ...)
  }

  invisible(validation_object)
}

#' @param scale numeric. Adjust plot size, by default \code{1}.
#' @param output_type function. The output type for the plot, by default \code{\link[grDevices]{png}}.
#' @param filename character. Path to save output png file (for only a single plot_type).
#' @param ... additional arguments to \code{\link[graphics]{plot}}.
#' @param validation_object_list validation_object_list. Internal parameter.
#' @rdname make_validation_plot
output_validation_plot <- function(validation_object_list, scale = 1, output_type = grDevices::png,
                                   filename = NULL, ...) {
  stopifnot(is(validation_object_list, "validation_object_list"))
  stopifnot(is.function(output_type))

  # Plot the results.
  if (!is.null(filename)) {
    output_type(filename, width = scale * 600, height = scale * 500)
    on.exit(dev.off(), add = TRUE)
  }

  plot(validation_object_list, scale = scale, ...)
}

#' @rdname plot_validation_object_list
#' @param x object. 
#' @param ... Internal.
#' @export
plot.validation_object_list <- function(x, ...) {
  plot_validation_object_list(validation_object_list = x, ...)
}

#' Plot a \code{validation_plot} object.
#'
#' @param validation_object_list list. The validation objects to plot.
#' @param ylab character. (For plot_type = "standard" only. Default is "Default (\%)")
#' @param title character. The title of the plot, by default \code{NULL} for 
#'    no title.
#' @param linecolors character. For plot_type = "standard" only.
#'    Vector of colors for each model. Default is rainbow.
#' @param line_types character. "Solid", "dashed" or "dotted", by default the former.
#' @param plotroc logical. Whether to print ROC in legend.
#' @param numbers logical. Whether to print number of defaults in
#'    each bucket above the plotted point, but only if you are
#'    plotting a single model. Default is \code{TRUE}.
#' @param showpoints logical. If \code{TRUE}, print only lines
#'    on validation graph, not points.
#' @param increment integer. Increments of y-axis scale, in percent.
#'    Default = 10.
#' @param n_per_row integer.
#' @param scale integer. If different than 1, used to scale output image.
#' @param use_normalizer logical. 
#' @export
plot_validation_object_list <- function(validation_object_list, ylab = "Default (%)",
                                        title = NULL, linecolors = c(), line_types = NULL,
                                        plotroc = TRUE, numbers = TRUE, showpoints = FALSE,
                                        increment = 10, n_per_row = 1, scale = 1,
                                        use_normalizer = FALSE) {

  # mean responses for all models
  if (use_normalizer) {
    all_responses <- unlist(lapply(validation_object_list, function(x) x$normalized_mean_response))
    buckets <- length(validation_object_list[[1]]$normalized_mean_response)
  } else {
    all_responses <- unlist(lapply(validation_object_list, function(x) x$mean_response))
    buckets <- length(validation_object_list[[1]]$mean_response)
  }
  number_of_obs <- sum(validation_object_list[[1]]$bucket_size)

  # set up plot parameters
  if (missing(increment)) { # automatically set plot increment if user doesn't specify
      rr <- max(all_responses, na.rm=TRUE) - min(all_responses, na.rm=TRUE)
      n <- ceiling(log10(rr/5))
      x <- c(10^(n-1), 2*10^(n-1), 5*10^(n-1), 10^n, 2*10^n, 5*10^n)
      m <- rr/x
      inc <- x[m == m[which.min(abs(m[m >= 4] - 5))]]
  } else {
      inc <- increment/100
  }
  ymin <- floor(min(all_responses, na.rm=TRUE)/inc) * inc
  ymax <- ceiling(max(all_responses, na.rm=TRUE)/inc) * inc

  # set up plot
  op <- par(oma = c(5,2,0,0), mgp = c(3,1,0), bty='l', yaxs='i', xpd=NA)
  plot(c(1,buckets), c(ymin, ymax),
       xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', type = 'n',)
  axis(side = 1, at = 1:buckets, cex = scale, cex.axis = scale)

  if ('gini' %in% names(validation_object_list[[1]])) { # y-axis is unbounded
    axis(side = 2, las = 2,
         at = seq(ymin+inc, ymax, by = inc),
         labels = seq(ymin+inc, ymax, by = inc), cex.axis = scale)
    ylab = "mean y_prediction in each decile"
  } else { # y-axis is a pct
    axis(side = 2, las = 2,
         at = seq(ymin+inc, ymax, by = inc),
         labels = sapply(seq(ymin+inc, ymax,by = inc), function(x) paste0(100*x,'%')), cex.axis = scale)
  }
  if (buckets == 10) {
    mtext("Decile", 1, 3, font=2, cex=1.2*scale)
  } else if (buckets == 5) {
    mtext("Quintile", 1, 3, font=2, cex=1.2*scale)
  } else {
    mtext("Quantile", 1, 3, font=2, cex=1.2*scale)
  }
  mtext(ylab, 2, 4, font=2, cex=1.2*scale)
  #grid(nx = NA, ny = NULL, lty = 1)

  # get plot coordinates
  xmin <- par("usr")[1]; xmax <- par("usr")[2]; x_delta <- xmax - xmin
  ymin <- par("usr")[3]; ymax <- par("usr")[4]; y_delta <- ymax - ymin

  # make title
  if (!is.null(title)) {
    rect(xmin, ymax+0.05*y_delta, xmax, ymax+0.15*y_delta,
         border = NA, col='slateblue4')
    text(0.5*(xmin+xmax), ymax+0.1*y_delta, labels=title, col='#ffffff', font=2, cex=scale)
  }

  # write size of data set
  nrows_in_legend <- (length(validation_object_list)-1)/n_per_row
  x <- graphics::grconvertX(0.02, from='ndc', to='user')
  y <- graphics::grconvertY(0.02, from='ndc', to='user')
  label <- paste0(number_of_obs, " data points used in analysis")
  text(x, y, labels = label, adj = 0, cex = scale)

  # add validation curves for all models
  index <- 0
  for (vobj in validation_object_list) {
    index <- index + 1
    with(vobj, {

      # plot formatting arguments
      # - line width
      lwd <- ifelse(showpoints, 1, 6)
      # - line type
      lty <- 1
      if (length(line_types)==1) {
        lty=line_types
      } else if (index <= length(line_types)) {
        lty <- line_types[index]
      }
      # - color
      if (index <= length(linecolors)) {
        ccc <- linecolors[[index]]
      } else {
        ccc <- default_colors(length(validation_object_list))[index]
      }

      # add points and lines to plot for this model
      if (use_normalizer) {
        if (showpoints) points(seq_len(buckets), normalized_mean_response, pch = 19, col = ccc)
        lines(seq_len(buckets), normalized_mean_response, col = ccc, lwd = lwd, lty = lty)
      } else {
        if (showpoints) points(seq_len(buckets), mean_response, pch = 19, col = ccc)
        lines(seq_len(buckets), mean_response, col = ccc, lwd = lwd, lty = lty)
      }

      # add legend
      if (plotroc && 'roc' %in% names(vobj)) {
        label <- substitute(
          paste(m,' (AUC = ', r[-le]^{+ue}, ')'),
          list(m = model_name, r = round(roc, digits=3), le = round(roc_le, digits=3), ue = round(roc_ue, digits=3))
        )
      } else if (plotroc && 'gini' %in% names(vobj)) {
        label <- substitute(
          paste(m,' (Gini = ', g, '), (NRMSD = ', RMSD, ')'),
          list(m = model_name, g = round(gini, digits=3), RMSD = round(NRMSD, digits = 3))
        )
      } else {
        label <- model_name
      }
      row <- floor((index-1)/n_per_row)
      column <- (index-1) %% n_per_row
      xd <- x_delta/n_per_row
      xlo <- xmin + column*xd
      xhi <- xlo + xd
      xm <- (xlo + xhi)/2
      x1 <- xlo + 1*x_delta/20 # line start
      x2 <- xlo + 3*x_delta/20 # line end
      x3 <- xlo + 4*x_delta/20 # text start
      y <- ymin - 0.25*y_delta - row*y_delta*0.06
      lines(c(x1, x2), c(y, y), col = ccc, lwd = lwd, lty = lty)
      text(x = x3, y = y, labels = label, adj = 0, col = ccc, cex = scale)

      # add number of ones above the plotted point
      if (numbers && length(validation_object_list)==1) {
        if (use_normalizer) {
          labels <- paste(formatC(100*normalized_mean_response, digits=3, format='fg'), '%', sep='')
          text(seq_len(buckets), normalized_mean_response + 0.05*y_delta, labels, cex = scale)
        } else if('gini' %in% names(validation_object_list[[1]])){
          labels <- paste(formatC(mean_response, digits=3, format='fg'), sep='')
          text(seq_len(buckets), mean_response + 0.05*y_delta, labels, cex = scale)
        }else{
          labels <- paste(formatC(100*mean_response, digits=3, format='fg'), '%', sep='')
          text(seq_len(buckets), mean_response + 0.05*y_delta, labels, cex = scale)
        }
      }
    })
  }

  # add lift if there are only 2 models
  if (length(validation_object_list)==2) {
    if (use_normalizer) {
      a0 <- validation_object_list[[1]]$normalized_mean_response[1]
      a1 <- validation_object_list[[2]]$normalized_mean_response[1]
      b0 <- validation_object_list[[1]]$normalized_mean_response[buckets]
      b1 <- validation_object_list[[2]]$normalized_mean_response[buckets]
    } else {
      a0 <- validation_object_list[[1]]$mean_response[1]
      a1 <- validation_object_list[[2]]$mean_response[1]
      b0 <- validation_object_list[[1]]$mean_response[buckets]
      b1 <- validation_object_list[[2]]$mean_response[buckets]
    }
    lift_a <- paste0(round(100*(a0-a1)/a1),'%')
    lift_b <- paste0(round(100*(b0-b1)/b1),'%')

    dd <- 0.02*x_delta
    lines(c(1, 1), c(a0, a1), lwd=2)
    text(c(1-dd, 1-dd), c(0.5*(a0+a1), 0.5*(a0+a1)), labels = lift_a, adj = 1)
    lines(c(buckets, buckets), c(b0, b1), lwd=2)
    text(c(buckets+dd, buckets+dd), c(0.5*(b0+b1), 0.5*(b0+b1)), labels = lift_b, adj = 0, cex=0.9)
  }
}

validate_model <- function(model) {
  if (!is.numeric(model) && !is(model, "tundraContainer")) {
    stop("Currently, only numeric and tundraContainer inputs are ",
         "accepted in the first argument to ",
         sQuote("make_validation_plot"), "; instead, I received a ",
         sQuote(crayon::red(class(model)[1L])))
  }
  model
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
  structure(lapply(seq_along(scores), function(i, ...) {
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

# Normalized RMSD to faciliate comparison between datasets with different scales
NormalizedRMSD <- function(y_actual, y_pred){
  if(max(y_actual) == min(y_actual)){
    stop("the indicator is homogenous and uninformative")
  }
  RMSD(y_actual, y_pred)/( max(y_actual) - min(y_actual) )
}

default_colors <- function(num) {
  colors <- c("#BE5050FF", "#9BBA5FFF", "#8065A1FF",
    "#F5954FFF", "#1E2A36FF", "#43AC47FF")
  if (num > length(colors)) {
    c(colors, grDevices::rainbow(length(colors) - num))
  } else { utils::head(colors, num) }
}

