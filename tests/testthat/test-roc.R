# Tests for ROC/AUC functionality

# Store temporary file paths for cleanup
pdf_file <- tempfile("mdatools-test-roc-", fileext = ".pdf")
txt_file <- tempfile("mdatools-test-roc-", fileext = ".txt")

setup({
   pdf(file = pdf_file)
   sink(txt_file, append = FALSE, split = FALSE)
})

teardown({
   dev.off()
   sink()
   # Clean up temporary files
   if (file.exists(pdf_file)) unlink(pdf_file)
   if (file.exists(txt_file)) unlink(txt_file)
})

## prepare datasets
data(iris)
cal.ind <- c(1:25, 51:75, 101:125)
val.ind <- c(26:50, 76:100, 126:150)

Xc <- iris[cal.ind, 1:4]
Xv <- iris[val.ind, 1:4]

cc.all <- iris[cal.ind, 5]
cv.all <- iris[val.ind, 5]

context("ROC/AUC: basic functionality")

test_that("getROC works for multi-class PLS-DA", {
   # Create a PLS-DA model with multiple classes
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   
   # Get predictions for test set
   expect_silent(res <- predict(m, Xv, cv.all))
   
   # Calculate ROC/AUC
   expect_silent(roc_res <- getROC(res))
   
   # Check structure
   expect_true(inherits(roc_res, "roc"))
   expect_true("roc" %in% names(roc_res))
   expect_true("auc" %in% names(roc_res))
   expect_true("ncomp" %in% names(roc_res))
   expect_true("classnames" %in% names(roc_res))
   
   # Check that AUC values are between 0 and 1
   expect_true(all(roc_res$auc >= 0 & roc_res$auc <= 1))
   
   # Check that we have ROC data for each class
   expect_equal(length(roc_res$roc), 3)
   expect_equal(length(roc_res$auc), 3)
   
   # Check ROC data structure for each class
   for (class_name in names(roc_res$roc)) {
      roc_data <- roc_res$roc[[class_name]]
      expect_true("tpr" %in% names(roc_data))
      expect_true("fpr" %in% names(roc_data))
      expect_true("thresholds" %in% names(roc_data))
      expect_equal(length(roc_data$tpr), length(roc_data$fpr))
      expect_equal(length(roc_data$tpr), length(roc_data$thresholds))
   }
   
   cat("\nROC/AUC results for multi-class model:\n")
   print(roc_res)
})

test_that("getROC works for specific classes", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   expect_silent(res <- predict(m, Xv, cv.all))
   
   # Get ROC for only first two classes
   expect_silent(roc_res <- getROC(res, nc = 1:2))
   expect_equal(length(roc_res$auc), 2)
   expect_equal(length(roc_res$roc), 2)
   
   # Get ROC for only one class
   expect_silent(roc_res <- getROC(res, nc = 1))
   expect_equal(length(roc_res$auc), 1)
   expect_equal(length(roc_res$roc), 1)
})

test_that("getROC works with different number of components", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   expect_silent(res <- predict(m, Xv, cv.all))
   
   # Test with different ncomp values
   for (ncomp in 1:3) {
      expect_silent(roc_res <- getROC(res, ncomp = ncomp))
      expect_equal(roc_res$ncomp, ncomp)
      expect_true(all(roc_res$auc >= 0 & roc_res$auc <= 1))
   }
})

test_that("getROC fails without reference values", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   
   # Predictions without reference values
   expect_silent(res <- predict(m, Xv))
   
   # Should fail when trying to calculate ROC
   expect_error(getROC(res), "Reference class values are required")
})

test_that("getROC works with calibration results", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   
   # Get ROC from calibration results
   expect_silent(roc_res <- getROC(m$calres))
   expect_true(inherits(roc_res, "roc"))
   expect_true(all(roc_res$auc >= 0 & roc_res$auc <= 1))
})

test_that("getROC works with cross-validation results", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   
   # Get ROC from cross-validation results
   expect_silent(roc_res <- getROC(m$cvres))
   expect_true(inherits(roc_res, "roc"))
   expect_true(all(roc_res$auc >= 0 & roc_res$auc <= 1))
})

context("ROC/AUC: plotting")

test_that("plotROC works for classification results", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   expect_silent(res <- predict(m, Xv, cv.all))
   
   # Plot ROC curves
   expect_silent(plotROC(res))
   
   # Plot for specific classes
   expect_silent(plotROC(res, nc = 1))
   expect_silent(plotROC(res, nc = 1:2))
   
   # Plot without AUC in legend
   expect_silent(plotROC(res, show.auc = FALSE))
   
   # Plot without legend
   expect_silent(plotROC(res, legend.position = "none"))
})

test_that("plotROC works with roc object", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   expect_silent(res <- predict(m, Xv, cv.all))
   
   # Get ROC results
   expect_silent(roc_res <- getROC(res))
   
   # Plot from roc object
   expect_silent(plotROC(roc_res))
})

test_that("plotROC works with calibration results", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   
   # Plot ROC for calibration results
   expect_silent(plotROC(m$calres))
})

test_that("plotROC works with cross-validation results", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   
   # Plot ROC for cross-validation results
   expect_silent(plotROC(m$cvres))
})

context("ROC/AUC: one-class model")

test_that("getROC works for one-class PLS-DA", {
   cc.vir <- cc.all == "virginica"
   cv.vir <- cv.all == "virginica"
   
   expect_silent(m <- plsda(Xc, cc.vir, 3, cv = 1, classname = "virginica"))
   expect_silent(res <- predict(m, Xv, cv.vir))
   
   # Calculate ROC/AUC for one-class model
   expect_silent(roc_res <- getROC(res))
   expect_equal(length(roc_res$auc), 1)
   expect_true(roc_res$auc >= 0 & roc_res$auc <= 1)
   
   cat("\nROC/AUC for one-class model:\n")
   print(roc_res)
})

test_that("plotROC works for one-class PLS-DA", {
   cc.vir <- cc.all == "virginica"
   cv.vir <- cv.all == "virginica"
   
   expect_silent(m <- plsda(Xc, cc.vir, 3, cv = 1, classname = "virginica"))
   expect_silent(res <- predict(m, Xv, cv.vir))
   
   # Plot ROC curve for one-class model
   expect_silent(plotROC(res))
})

context("ROC/AUC: edge cases")

test_that("getROC handles invalid parameters", {
   expect_silent(m <- plsda(Xc, cc.all, 3, cv = 1))
   expect_silent(res <- predict(m, Xv, cv.all))
   
   # Invalid ncomp
   expect_error(getROC(res, ncomp = 0))
   expect_error(getROC(res, ncomp = 10))
   
   # Invalid nc
   expect_error(getROC(res, nc = 0))
   expect_error(getROC(res, nc = 10))
})
