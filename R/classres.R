#' Results of classification
#'
#' @description
#' \code{classres} is used to store results classification for one or multiple classes.
#'
#' @param c.pred
#' matrix with predicted values (+1 or -1) for each class.
#' @param c.ref
#' matrix with reference values for each class.
#' @param p.pred
#' matrix with probability values for each class.
#' @param ncomp.selected
#' vector with selected number of components for each class.
#'
#' @details
#' There is no need to create a \code{classres} object manually, it is created automatically when
#' build a classification model (e.g. using \code{\link{simca}} or \code{\link{plsda}}) or apply
#' the model to new data. For any classification method from \code{mdatools}, a class using to
#' represent results of classification (e.g. \code{\link{simcares}}) inherits fields and methods of
#' \code{classres}.
#'
#' @return
#' \item{c.pred}{predicted class values (+1 or -1).}
#' \item{p.pred}{predicted class probabilities.}
#' \item{c.ref}{reference (true) class values if provided.}
#'
#' The following fields are available only if reference values were provided.
#' \item{tp}{number of true positives.}
#' \item{tn}{number of true negatives.}
#' \item{fp}{nmber of false positives.}
#' \item{fn}{number of false negatives.}
#' \item{specificity}{specificity of predictions.}
#' \item{sensitivity}{sensitivity of predictions.}
#' \item{misclassified}{ratio of misclassified objects.}
#'
#' @seealso
#' Methods \code{classres} class:
#' \tabular{ll}{
#'  \code{\link{showPredictions.classres}} \tab shows table with predicted values.\cr
#'  \code{\link{plotPredictions.classres}} \tab makes plot with predicted values.\cr
#'  \code{\link{plotSensitivity.classres}} \tab makes sn plot.\cr
#'  \code{\link{plotSpecificity.classres}} \tab makes specificity plot.\cr
#'  \code{\link{plotMisclassified.classres}} \tab makes ms ratio plot.\cr
#'  \code{\link{plotPerformance.classres}} \tab makes plot with misclassified ratio, specificity
#'  and sensitivity values.\cr
#' }
#'
#' @export
classres <- function(c.pred, c.ref = NULL, p.pred = NULL, ncomp.selected = 1) {

   if (length(dim(c.pred)) != 3) {
      stop("Wrong number of dimensions for 'c.pred' array (should be 3-way array).")
   }

   obj <- list()
   obj$c.ref <- if (!is.null(c.ref)) as.factor(c.ref)
   obj$c.pred <- c.pred
   obj$p.pred <- p.pred
   obj$nclasses <- dim(c.pred)[3]
   obj$ncomp <- dim(c.pred)[2]
   obj$classnames <- dimnames(c.pred)[[3]]

   # check that ncomp.selected is correct
   if (is.null(ncomp.selected)) ncomp.selected <- obj$ncomp
   if (ncomp.selected < 1 || ncomp.selected > obj$ncomp) {
      stop("Wrong value for 'ncomp.selected' parameer.")
   }

   obj$ncomp.selected <- ncomp.selected

   if (!is.null(c.ref)) {
      obj <- c(obj, classres.getPerformance(c.ref, c.pred))
   }


   obj$call <- match.call()
   class(obj) <- "classres"

   return(obj)
}

#' Confusion matrix for classification results
#'
#' @details
#' Returns confusion matrix for classification results represented by the object.
#'
#' @param obj
#' classification results (object of class \code{simcares}, \code{simcamres}, etc)
#' @param ncomp
#' number of components to make the matrix for (NULL - use selected for a model).
#' @param ...
#' other arguments
#'
#' @description
#' The columns of the matrix correspond to classification results, rows - to the real classes. In
#' case of soft classification with multiple classes (e.g. SIMCAM) sum of values for every row
#' will not correspond to the total number of class members as the same object can be classified
#' as a member of several classes or non of them.
#'
#' @export
getConfusionMatrix.classres <- function(obj, ncomp = obj$ncomp.selected, ...) {

   if (is.null(obj$c.ref)) {
      stop("Reference classes are not available!")
   }

   attrs <- mda.getattr(obj$c.pred)
   c.pred <- obj$c.pred[, ncomp, , drop = FALSE]
   c.ref <- obj$c.ref

   # remove excluded rows
   if (length(attrs$exclrows) > 0) {
      c.pred <- c.pred[-attrs$exclrows, , , drop = FALSE]
      c.ref <- c.ref[-attrs$exclrows]
   }

   # get class names and numbers
   ref.classes <- levels(c.ref)
   ref.nclasses <- length(ref.classes)

   # compute the confusion matrix
   out <- matrix(0, nrow = ref.nclasses, ncol = obj$nclasses + 1)
   none <- rep(TRUE, length(c.ref))
   for (i in seq_len(obj$nclasses)) {
      ind <- c.pred[, , i] > 0
      out[, i] <- table(c.ref[ind], exclude = FALSE)
      none[ind] <- FALSE
   }

   # find ones that were not classified as member of any class and names
   out[, obj$nclasses + 1] <- table(c.ref[none], exclude = FALSE)
   rownames(out) <- ref.classes
   colnames(out) <- c(obj$classnames, "None")

   # reorder the table to match class name order in results
   ind1 <- match(colnames(out), rownames(out))
   ind1 <- ind1[!is.na(ind1)]
   ind2 <- seq_len(nrow(out))[-ind1]
   out <- out[c(ind1, ind2), , drop = FALSE]

   return(out)
}

#' Show predicted class values
#'
#' @description
#' Shows a table with predicted class values for classification result.
#'
#' @param obj
#' object with classification results (e.g. \code{plsdares} or \code{simcamres}).
#' @param ncomp
#' number of components to show the predictions for (NULL - use selected for a model).
#' @param ...
#' other parameters
#'
#' @details
#' The function prints a matrix where every column is a class and every row is an data object.
#' The matrix has either -1 (does not belong to the class) or +1 (belongs to the class) values.
#'
#' @export
showPredictions.classres <- function(obj, ncomp = obj$ncomp.selected, ...) {

   pred <- obj$c.pred[, ncomp, ]
   dim(pred) <- dim(obj$c.pred)[c(1, 3)]
   dimnames(pred) <- dimnames(obj$c.pred)[c(1, 3)]

   print(pred)
   cat("\n")
}

#' as.matrix method for classification results
#'
#' @description
#' Generic \code{as.matrix} function for classification results. Returns matrix with performance
#' values for specific class.
#'
#' @param x
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param ncomp
#' model complexity (number of components) to show the parameters for.
#' @param nc
#' if there are several classes, which class to show the parameters for.
#' @param ...
#' other arguments
#'
#' @export
as.matrix.classres <- function(x, ncomp = NULL, nc = 1, ...) {

   if (is.null(x$c.ref)) return()

   if (length(nc) != 1) {
      stop("Wrong value for 'nc' parameter.")
   }

   specificity <- if (is.null(x$specificity)) matrix(NA, x$nclasses, x$ncomp) else x$specificity
   out <- cbind(
      x$tp[nc, ], x$fp[nc, ], x$tn[nc, ], x$fn[nc, ],
      round(specificity[nc, ], 3),
      round(x$sensitivity[nc, ], 3),
      round(1 - x$misclassified[nc, ], 3)
   )

   out[is.nan(out)] <- NA
   colnames(out) <- c("TP", "FP", "TN", "FN", "Spec.", "Sens.", "Accuracy")
   rownames(out) <- dimnames(x$c.pred)[[2]]

   if (!is.null(ncomp)) {
      out <- out[ncomp, , drop = FALSE]
   }

   return(out)
}

#' Print information about classification result object
#'
#' @description
#' Generic \code{print} function for classification results. Prints information about major fields
#' of the object.
#'
#' @param x
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param str
#' User specified text (e.g. to be used for particular method, like PLS-DA, etc).
#' @param ...
#' other arguments
#'
#' @export
print.classres <- function(x, str = "Classification results (class classres)\nMajor fields:", ...) {

   if (nchar(str) > 0) fprintf("\n%s\n", str)

   cat("$c.pred - predicted class values\n")

   if (!is.null(x$c.ref)) {
      cat("$c.ref - reference (true) class values\n")
      cat("$tp - number of true positives\n")
      cat("$tn - number of true negatives\n")
      cat("$fp - number of false positives\n")
      cat("$fn - number of false negatives\n")
      cat("$specificity - specificity of predictions\n")
      cat("$sensitivity - sn of predictions\n")
      cat("$misclassified - misclassification ratio for predictions\n")
   }
}

#' Summary statistics about classification result object
#'
#' @description
#' Generic \code{summary} function for classification results. Prints performance values for the
#' results.
#'
#' @param object
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param ncomp
#' which number of components to make the plot for (use NULL to show results for all available).
#' @param nc
#' vector with class numbers to show the summary for.
#' @param ...
#' other arguments
#'
#' @export
summary.classres <- function(object, ncomp = object$ncomp.selected,
   nc = seq_len(object$nclasses), ...) {

   cat("\nClassification results (class classres) summary\n")

   if (is.null(object$c.ref)) {
      cat("No reference data provided to calculate prediction performance.")
      return()
   }

   fprintf("\nNumber of selected components: %d", ncomp)
   fprintf("\nNumber of classes: %d\n", object$nclasses)

   # detailed results for several components
   for (i in nc) {
      fprintf("\nClass '%s':\n", object$classnames[[i]])
      print(as.matrix.classres(object, nc = i, ncomp = ncomp))
   }

}

################################
#  Static methods              #
################################

#' Calculation of  classification performance parameters
#'
#' @description
#' Calculates and returns performance parameters for classification result (e.g. number of false
#' negatives, false positives, sn, specificity, etc.).
#'
#' @param c.ref
#' reference class values for objects (vector with numeric or text values)
#' @param c.pred
#' predicted class values for objects (array nobj x ncomponents x nclasses)
#'
#' @return
#' Returns a list with following fields:
#' \tabular{ll}{
#'    \code{$fn} \tab number of false negatives (nclasses x ncomponents) \cr
#'    \code{$fp} \tab number of false positives (nclasses x ncomponents) \cr
#'    \code{$tp} \tab number of true positives (nclasses x ncomponents) \cr
#'    \code{$sensitivity} \tab sn values (nclasses x ncomponents) \cr
#'    \code{$specificity} \tab specificity values (nclasses x ncomponents) \cr
#'    \code{$specificity} \tab ms ratio values (nclasses x ncomponents) \cr
#' }
#'
#' @details
#' The function is called automatically when a classification result with reference values is
#' created, for example when applying a \code{plsda} or \code{simca} models.
#'
classres.getPerformance <- function(c.ref, c.pred) {

   if (is.null(c.ref) || is.null(c.pred)) {
      stop("Both reference and predicted class values are required.")
   }

   if (length(c.ref) != dim(c.pred)[1]) {
      stop("Number of objects in reference and predicted results should be the same.")
   }

   # remove excluded rows for correct calculation of performance
   dim(c.ref) <- NULL
   attrs <- mda.getattr(c.pred)
   if (length(attrs$exclrows) > 0) {
      c.pred <- c.pred[-attrs$exclrows, , , drop = FALSE]
      c.ref <- c.ref[-attrs$exclrows]
   }

   ncomp <- dim(c.pred)[2]
   nclasses <- dim(c.pred)[3]

   tp <- matrix(0, nrow = nclasses, ncol = ncomp)
   fp <- matrix(0, nrow = nclasses, ncol = ncomp)
   fn <- matrix(0, nrow = nclasses, ncol = ncomp)
   tn <- matrix(0, nrow = nclasses, ncol = ncomp)

   # compute main performance indicators
   classnames <- dimnames(c.pred)[[3]]
   for (i in seq_len(nclasses)) {
      fn[i, ] <- colSums((c.ref == classnames[i]) & (c.pred[, , i, drop = FALSE] == -1))
      fp[i, ] <- colSums((c.ref != classnames[i]) & (c.pred[, , i, drop = FALSE] == 1))
      tp[i, ] <- colSums((c.ref == classnames[i]) & (c.pred[, , i, drop = FALSE] == 1))
      tn[i, ] <- colSums((c.ref != classnames[i]) & (c.pred[, , i, drop = FALSE] == -1))
   }

   # compute main statistics
   sn <- tp / (tp + fn)
   sp <- tn / (tn + fp)
   ms <- (fp + fn) / (tp + tn + fp + fn)

   # add row with summary for all classes
   sn <- rbind(sn, colSums(tp) / colSums(tp + fn))
   sp <- rbind(sp, colSums(tn) / colSums(tn + fp))
   ms <- rbind(ms, colSums(fp + fn) / colSums(tp + tn + fp + fn))

   # add names
   row_names <- dimnames(c.pred)[[3]]
   col_names <- dimnames(c.pred)[[2]]
   rownames(fn) <- rownames(fp) <- rownames(tp) <- rownames(tn) <- row_names
   colnames(fn) <- colnames(fp) <- colnames(tp) <- colnames(sn) <- colnames(sp) <- col_names
   rownames(sn) <- rownames(sp) <- rownames(ms) <- c(row_names, "Total")

   # in case of one class classifier set sensitivity NULL
   if (all(is.na(sp))) sp <- NULL

   return(
      list(
         "fn" = fn,
         "fp" = fp,
         "tp" = tp,
         "tn" = tn,
         "sensitivity" = sn,
         "specificity" = sp,
         "misclassified" = ms
      )
   )
}


################################
#  Plotting methods            #
################################

#' Plot for class belonging probability
#'
#' @description
#' Makes a plot with class belonging probabilities for each object of the classification results.
#' Works only with classification methods, which compute this probability (e.g. SIMCA).
#'
#' @param obj
#' classification results (e.g. object of class \code{simcamres}).
#' @param ncomp
#' number of components to use the probabilities for.
#' @param nc
#' if there are several classes, which class to make the plot for.
#' @param type
#' type of the plot
#' @param ylim
#' vector with limits for y-axis
#' @param show.lines
#' shows a horizontal line at p = 0.5
#' @param ...
#' most of the graphical parameters from \code{\link{mdaplot}} function can be used.
#'
#' @export
plotProbabilities.classres <- function(obj, ncomp = obj$ncomp.selected, nc = 1, type = "h",
   ylim = c(0, 1.1), show.lines = c(NA, 0.5), ...) {

   if (is.null(obj$p.pred)) {
      stop("No probability values are available.")
   }

   if (nc > obj$nclasses || nc < 1) {
      stop("Wrong value for argument 'nc'.")
   }

   plot_data <- obj$p.pred[, ncomp, nc]
   cname <- obj$classnames[[nc]]
   attr(plot_data, "name") <- sprintf("Class probabilities, %s (ncomp = %d)", cname, ncomp)
   attr(plot_data, "yaxis.name") <- "Probability"
   attr(plot_data, "xaxis.name") <- attr(obj$c.pred, "yaxis.name")

   return(mdaplot(plot_data, show.lines = show.lines, type = type, ylim = ylim, ...))
}

#' Sensitivity plot for classification results
#'
#' @description
#' Makes a plot with sn values vs. model complexity (e.g. number of components) for
#' classification results.
#'
#' @param obj
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param legend.position
#' position of the legend (as in \code{mdaplotg}).
#' @param ...
#' other parameters for \code{\link{plotPerformance.classres}}
#'
#' @details
#' See examples in description of \code{\link{plsdares}}, \code{\link{simcamres}}, etc.
#'
#' @export
plotSensitivity.classres <- function(obj, legend.position = "bottomright", ...) {
   return(plotPerformance(obj, param = "sensitivity", legend.position = legend.position, ...))
}

#' Specificity plot for classification results
#'
#' @description
#' Makes a plot with specificity values vs. model complexity (e.g. number of components) for
#' classification results.
#'
#' @param obj
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param legend.position
#' position of the legend (as in \code{mdaplotg}).
#' @param ...
#' other parameters for \code{\link{plotPerformance.classres}}
#'
#' @details
#' See examples in description of \code{\link{plsdares}}, \code{\link{simcamres}}, etc.
#'
#' @export
plotSpecificity.classres <- function(obj, legend.position = "bottomright", ...) {
   return(plotPerformance(obj, param = "specificity", legend.position = legend.position, ...))
}

#' Misclassified ratio plot for classification results
#'
#' @description
#' Makes a plot with ms ratio values vs. model complexity (e.g. number of components) for
#' classification results.
#'
#' @param obj
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param ...
#' other parameters for \code{\link{plotPerformance.classres}}
#'
#' @details
#' See examples in description of \code{\link{plsdares}}, \code{\link{simcamres}}, etc.
#'
#' @export
plotMisclassified.classres <- function(obj, ...) {
   return(plotPerformance(obj, param = "misclassified", ...))
}

#' Performance plot for classification results
#'
#' @description
#' Makes a plot with classification performance parameters vs. model complexity (e.g. number of
#' components) for classification results.
#'
#' @param obj
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param nc
#' if there are several classes, which class to make the plot for.
#' @param type
#' type of the plot
#' @param param
#' which performance parameter to make the plot for (can be a vector with several values).
#' @param labels
#' what to show as labels for plot objects.
#' @param ylab
#' label for y axis
#' @param ylim
#' vector with two values - limits for y axis
#' @param xticks
#' vector with x-axis tick values
#' @param show.plot
#' logical, shall plot be created or just plot series object is needed
#' @param ...
#' most of the graphical parameters from \code{\link{mdaplot}} function can be used.
#'
#' @details
#' See examples in description of \code{\link{plsdares}}, \code{\link{simcamres}}, etc.
#'
#' @export
plotPerformance.classres <- function(obj, nc = 1, type = "b",
   param = c("sensitivity", "specificity", "misclassified"), labels = "values",
   ylab = "", ylim = c(0, 1.1), xticks = seq_len(obj$ncomp), show.plot = TRUE, ...) {

   if (is.null(obj$c.ref)) {
      stop("No reference data available")
   }

   # check if parameters requested are not NULL
   param <- param[param %in% sapply(names(obj), function(x) if (!is.null(obj[[x]])) x)]
   if (length(param) == 0) {
      stop("Performance parameteres you requested are not available in this result object.")
   }

   # prepare plot data
   plot_data <- do.call(rbind, lapply(obj[param], function(x) x[nc, , drop = FALSE]))

   attr(plot_data, "name") <- if (length(param) == 1) capitalize(param) else sprintf(
      "Classification performance (%s)", obj$classnames[[nc]])

   attr(plot_data, "xaxis.name") <- "Components"
   rownames(plot_data) <- param

   # if no plot needed return the plat data
   if (!show.plot) {
      return(plot_data)
   }

   mdaplotg(plot_data, type = type, xticks = xticks, ylim = ylim, ylab = ylab, labels = labels, ...)
}

#' Prediction plot for classification results
#'
#' @description
#' Makes a plot with predicted class values for classification results.
#'
#' @param obj
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param nc
#' vector with classes to show predictions for.
#' @param ncomp
#' model complexity (number of components) to make the plot for.
#' @param ylab
#' label for y axis
#' @param show.plot
#' logical, shall plot be created or just plot series object is needed
#' @param ...
#' most of the graphical parameters from \code{\link{mdaplotg}} or \code{\link{mdaplot}} function
#' can be used.
#'
#' @details
#' See examples in description of \code{\link{plsdares}}, \code{\link{simcamres}}, etc.
#'
#' @export
plotPredictions.classres <- function(obj, nc = seq_len(obj$nclasses), ncomp = obj$ncomp.selected,
   ylab = "", show.plot = TRUE, ...) {

   # prepare data and attributes
   attrs <- mda.getattr(obj$c.pred)
   c.pred <- as.matrix(obj$c.pred[, ncomp, nc])
   row_ind <- seq_len(nrow(c.pred))
   class_names <- obj$classnames[nc]
   class_numbers <- seq_along(nc) + 1

   # multiply classes to integers starting from 2 (1 will be for none)
   plot_data <- (c.pred > 0) %*% diag(class_numbers, length(nc), length(nc))

   # fine those which were not classified as members of any class (none)
   plot_data <- cbind(rowSums(plot_data) == 0, plot_data)

   # unfold matrix with class numbers and merge with row indices
   plot_data <- cbind(row_ind, as.numeric(plot_data))

   # remove rows with zeros as class number
   plot_data <- plot_data[plot_data[, 2] > 0, , drop = FALSE]

   # add row names and exclude hidden rows
   if (is.null(attrs$yaxis.name)) attrs$yaxis.name <- "Objects"
   plot_data <- mda.exclrows(plot_data, plot_data[, 1] %in% attrs$exclrows)
   rownames(plot_data) <- rownames(c.pred)[plot_data[, 1]]
   colnames(plot_data) <- c(attrs$yaxis.name, "Classes")
   attr(plot_data, "name") <- sprintf("Predictions (ncomp = %d)", ncomp)

   if (!show.plot) {
      return(plot_data)
   }

   yticks <- c(1, class_numbers)
   yticklabels <- c("None", class_names)

   cgroup <- if (!is.null(obj$c.ref)) as.factor(obj$c.ref[plot_data[, 1]])
   mdaplot(plot_data, type = "p", ylab = ylab, yticks = yticks,
      cgroup = cgroup, yticklabels = yticklabels, ...)
}

#' Plot function for classification results
#'
#' @description
#' Generic plot function for classification results.
#' Alias for \code{\link{plotPredictions.classres}}.
#'
#' @param x
#' classification results (object of class \code{plsdares}, \code{simcamres}, etc.).
#' @param ...
#' other arguments for \code{plotPredictions()} method.
#'
#' @export
plot.classres <- function(x, ...) {
   plotPredictions.classres(x, ...)
}


################################
#  ROC/AUC methods             #
################################

#' Calculate ROC curve and AUC for classification results
#'
#' @description
#' Computes Receiver Operating Characteristic (ROC) curve and calculates the Area Under the
#' Curve (AUC) for PLS-DA and other classification models. Similar to the auroc function
#' from the mixOmics package.
#'
#' @param obj
#' classification results (object of class \code{classres}, \code{plsdares}, etc.).
#' @param ncomp
#' number of components to calculate ROC/AUC for (if NULL, uses selected optimal value).
#' @param nc
#' which class to calculate ROC/AUC for (can be a vector for multiple classes).
#'
#' @return
#' A list with class "roc" containing:
#' \item{roc}{a list of ROC curve data for each class, each containing TPR, FPR, and thresholds.}
#' \item{auc}{a named vector of AUC values for each class.}
#' \item{ncomp}{number of components used.}
#' \item{classnames}{names of the classes.}
#'
#' @details
#' The ROC curve is constructed by varying the classification threshold on the continuous
#' predictions (\code{p.pred} values) and calculating the True Positive Rate (TPR, sensitivity)
#' and False Positive Rate (FPR, 1-specificity) at each threshold.
#'
#' The AUC (Area Under the Curve) is calculated using the trapezoidal rule and provides a
#' single-number summary of the classifier performance, where:
#' \itemize{
#'   \item AUC = 1.0: perfect classifier
#'   \item AUC = 0.5: random classifier
#'   \item AUC < 0.5: worse than random (predictions are inverted)
#' }
#'
#' This function requires reference class values (\code{c.ref}) to be available in the
#' classification results object.
#'
#' @seealso
#' \code{\link{plotROC.classres}} for plotting ROC curves.
#'
#' @examples
#' \dontrun{
#' # Create a PLS-DA model
#' library(mdatools)
#' data(iris)
#'
#' x.cal <- iris[seq(1, nrow(iris), 2), 1:4]
#' c.cal <- iris[seq(1, nrow(iris), 2), 5]
#' x.test <- iris[seq(2, nrow(iris), 2), 1:4]
#' c.test <- iris[seq(2, nrow(iris), 2), 5]
#'
#' model <- plsda(x.cal, c.cal, ncomp = 3, cv = 1)
#' res <- predict(model, x.test, c.test)
#'
#' # Calculate ROC/AUC
#' roc_res <- getROC(res)
#' print(roc_res$auc)
#'
#' # Plot ROC curves
#' plotROC(res)
#' }
#'
#' @export
getROC.classres <- function(obj, ncomp = obj$ncomp.selected, nc = seq_len(obj$nclasses)) {

   # Check if reference values are available
   if (is.null(obj$c.ref)) {
      stop("Reference class values are required to calculate ROC/AUC.")
   }

   # Check if prediction probabilities are available
   if (is.null(obj$p.pred)) {
      stop("Prediction values (p.pred) are required to calculate ROC/AUC.")
   }

   # Validate ncomp
   if (is.null(ncomp) || ncomp < 1 || ncomp > obj$ncomp) {
      stop(sprintf("'ncomp' must be between 1 and %d, got %s", obj$ncomp, 
                   ifelse(is.null(ncomp), "NULL", as.character(ncomp))))
   }

   # Validate nc
   if (any(nc < 1) || any(nc > obj$nclasses)) {
      stop(sprintf("'nc' must be between 1 and %d, got values outside this range", 
                   obj$nclasses))
   }

   # Get attributes and remove excluded rows
   attrs <- mda.getattr(obj$p.pred)
   c.ref <- obj$c.ref
   p.pred <- obj$p.pred[, ncomp, , drop = FALSE]

   if (length(attrs$exclrows) > 0) {
      p.pred <- p.pred[-attrs$exclrows, , , drop = FALSE]
      c.ref <- c.ref[-attrs$exclrows]
   }

   # Calculate ROC curve and AUC for each class
   roc_list <- list()
   auc_values <- numeric(length(nc))
   names(auc_values) <- obj$classnames[nc]

   for (i in seq_along(nc)) {
      class_idx <- nc[i]
      class_name <- obj$classnames[class_idx]

      # Get predictions and true labels for this class
      scores <- p.pred[, 1, class_idx]
      labels <- c.ref == class_name

      # Calculate ROC curve
      roc_data <- calculateROC(scores, labels)

      # Calculate AUC using trapezoidal rule
      auc_values[i] <- calculateAUC(roc_data$fpr, roc_data$tpr)

      roc_list[[class_name]] <- roc_data
   }

   result <- list(
      roc = roc_list,
      auc = auc_values,
      ncomp = ncomp,
      classnames = obj$classnames[nc]
   )

   class(result) <- "roc"
   return(result)
}

#' @rdname getROC.classres
#' @export
getROC <- function(obj, ...) {
   UseMethod("getROC")
}

#' Calculate ROC curve from scores and labels
#'
#' @description
#' Internal function to calculate ROC curve data points.
#'
#' @param scores
#' numeric vector of prediction scores
#' @param labels
#' logical vector of true class labels (TRUE for positive class)
#'
#' @return
#' A list containing TPR, FPR, and thresholds
#'
#' @keywords internal
calculateROC <- function(scores, labels) {

   # Sort by scores in descending order
   order_idx <- order(scores, decreasing = TRUE)
   scores_sorted <- scores[order_idx]
   labels_sorted <- labels[order_idx]

   # Total positives and negatives
   n_pos <- sum(labels)
   n_neg <- sum(!labels)

   # Initialize vectors for ROC curve
   # Add points at (0,0) and (1,1)
   n_points <- length(scores) + 1
   tpr <- numeric(n_points)
   fpr <- numeric(n_points)
   thresholds <- numeric(n_points)

   # Start at (0, 0) with threshold = Inf
   tpr[1] <- 0
   fpr[1] <- 0
   thresholds[1] <- Inf

   # Calculate cumulative TP and FP
   tp <- 0
   fp <- 0

   for (i in seq_along(scores_sorted)) {
      if (labels_sorted[i]) {
         tp <- tp + 1
      } else {
         fp <- fp + 1
      }

      tpr[i + 1] <- tp / n_pos
      fpr[i + 1] <- fp / n_neg
      thresholds[i + 1] <- scores_sorted[i]
   }

   return(list(
      tpr = tpr,
      fpr = fpr,
      thresholds = thresholds
   ))
}

#' Calculate AUC using trapezoidal rule
#'
#' @description
#' Internal function to calculate Area Under the ROC Curve.
#'
#' @param fpr
#' numeric vector of False Positive Rates
#' @param tpr
#' numeric vector of True Positive Rates
#'
#' @return
#' Numeric AUC value
#'
#' @keywords internal
calculateAUC <- function(fpr, tpr) {
   # Sort by FPR to ensure correct ordering
   order_idx <- order(fpr)
   fpr <- fpr[order_idx]
   tpr <- tpr[order_idx]

   # Trapezoidal integration
   auc <- 0
   for (i in seq_len(length(fpr) - 1)) {
      auc <- auc + (fpr[i + 1] - fpr[i]) * (tpr[i] + tpr[i + 1]) / 2
   }

   return(auc)
}

#' Plot ROC curves for classification results
#'
#' @description
#' Creates a plot of ROC (Receiver Operating Characteristic) curves showing the True Positive
#' Rate vs. False Positive Rate for classification results.
#'
#' @param obj
#' classification results (object of class \code{classres}, \code{plsdares}, etc.) or
#' an object of class \code{roc} returned by \code{\link{getROC.classres}}.
#' @param ncomp
#' number of components to use (if NULL, uses selected optimal value). Only used if
#' \code{obj} is not already a \code{roc} object.
#' @param nc
#' which class(es) to plot ROC curves for. Can be a vector for multiple classes.
#' @param show.auc
#' logical, whether to show AUC values in the legend.
#' @param main
#' main title for the plot.
#' @param legend.position
#' position of the legend ("bottomright", "topright", "topleft", "bottomleft", or "none").
#' @param col
#' vector of colors for each class.
#' @param lty
#' vector of line types for each class.
#' @param lwd
#' line width.
#' @param ...
#' other graphical parameters.
#'
#' @details
#' The function creates a ROC curve plot with the diagonal reference line (random classifier).
#' If multiple classes are specified, they are plotted with different colors and included in
#' the legend with their respective AUC values (if \code{show.auc = TRUE}).
#'
#' @seealso
#' \code{\link{getROC.classres}} for calculating ROC curves and AUC values.
#'
#' @examples
#' \dontrun{
#' # Create a PLS-DA model
#' library(mdatools)
#' data(iris)
#'
#' x.cal <- iris[seq(1, nrow(iris), 2), 1:4]
#' c.cal <- iris[seq(1, nrow(iris), 2), 5]
#' x.test <- iris[seq(2, nrow(iris), 2), 1:4]
#' c.test <- iris[seq(2, nrow(iris), 2), 5]
#'
#' model <- plsda(x.cal, c.cal, ncomp = 3, cv = 1)
#' res <- predict(model, x.test, c.test)
#'
#' # Plot ROC curves
#' plotROC(res)
#' plotROC(res, nc = 1:2)  # Plot only first two classes
#' }
#'
#' @export
plotROC.classres <- function(obj, ncomp = NULL, nc = NULL, show.auc = TRUE,
                              main = "ROC Curves", legend.position = "bottomright",
                              col = NULL, lty = 1, lwd = 2, ...) {

   # If obj is already a roc object, use it directly
   if (inherits(obj, "roc")) {
      roc_res <- obj
      if (is.null(nc)) nc <- seq_along(roc_res$classnames)
   } else {
      # Calculate ROC if not already done
      if (is.null(ncomp)) ncomp <- obj$ncomp.selected
      if (is.null(nc)) nc <- seq_len(obj$nclasses)
      roc_res <- getROC(obj, ncomp = ncomp, nc = nc)
   }

   n_classes <- length(roc_res$classnames)

   # Set up colors if not provided
   if (is.null(col)) {
      col <- mdaplot.getColors(n_classes)
   } else if (length(col) < n_classes) {
      col <- rep(col, length.out = n_classes)
   }

   # Set up line types
   if (length(lty) < n_classes) {
      lty <- rep(lty, length.out = n_classes)
   }

   # Create empty plot
   plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
        xlab = "False Positive Rate (1 - Specificity)",
        ylab = "True Positive Rate (Sensitivity)",
        main = main, ...)

   # Add diagonal reference line (random classifier)
   abline(0, 1, col = "gray", lty = 2)

   # Plot ROC curves for each class
   for (i in seq_along(roc_res$classnames)) {
      class_name <- roc_res$classnames[i]
      roc_data <- roc_res$roc[[class_name]]
      lines(roc_data$fpr, roc_data$tpr, col = col[i], lty = lty[i], lwd = lwd)
   }

   # Add legend
   if (legend.position != "none") {
      legend_labels <- if (show.auc) {
         sprintf("%s (AUC = %.3f)", roc_res$classnames, roc_res$auc)
      } else {
         roc_res$classnames
      }

      legend(legend.position, legend = legend_labels, col = col,
             lty = lty, lwd = lwd, bty = "n")
   }
}

#' @rdname plotROC.classres
#' @export
plotROC <- function(obj, ...) {
   UseMethod("plotROC")
}

#' Print method for ROC results
#'
#' @param x
#' ROC results object (class "roc")
#' @param ...
#' other arguments
#'
#' @export
print.roc <- function(x, ...) {
   cat("\nROC Curve and AUC Results\n")
   cat("-------------------------\n")
   fprintf("Number of components: %d\n", x$ncomp)
   fprintf("Number of classes: %d\n", length(x$classnames))
   cat("\nAUC values:\n")
   for (i in seq_along(x$auc)) {
      fprintf("  %s: %.4f\n", names(x$auc)[i], x$auc[i])
   }
   cat("\n")
}
