###############################################################################
# Example: ROC/AUC for PLS-DA Models
# 
# This script demonstrates how to calculate ROC curves and AUC values
# for PLS-DA models using the mdatools package, similar to the mixOmics
# package auroc function.
###############################################################################

library(mdatools)

# Load the iris dataset
data(iris)

# Create calibration and test sets
cal.ind <- seq(1, nrow(iris), 2)
test.ind <- seq(2, nrow(iris), 2)

x.cal <- iris[cal.ind, 1:4]
c.cal <- iris[cal.ind, 5]
x.test <- iris[test.ind, 1:4]
c.test <- iris[test.ind, 5]

###############################################################################
# Example 1: Multi-class PLS-DA model
###############################################################################

cat("Example 1: Multi-class PLS-DA with ROC/AUC\n")
cat("==========================================\n\n")

# Build a PLS-DA model with 3 components and cross-validation
model <- plsda(x.cal, c.cal, ncomp = 3, cv = 1, info = "Iris classification")

# Select optimal number of components
model <- selectCompNum(model, 1)

# Show model summary
summary(model)

# Apply model to test set
test.res <- predict(model, x.test, c.test)

# Calculate ROC curves and AUC values
cat("\nCalculating ROC curves and AUC values...\n")
roc.res <- getROC(test.res)

# Print AUC results
print(roc.res)

# Plot ROC curves for all classes
cat("\nPlotting ROC curves for all classes...\n")
p <- plotROC(test.res, main = "ROC Curves - Test Set (All Classes)")
print(p)

# Plot ROC curves for specific classes
cat("\nPlotting ROC curves for individual classes...\n")
p1 <- plotROC(test.res, nc = 1, main = "ROC: setosa")
p2 <- plotROC(test.res, nc = 2, main = "ROC: versicolor")
p3 <- plotROC(test.res, nc = 3, main = "ROC: virginica")
print(p1)
print(p2)
print(p3)

###############################################################################
# Example 2: ROC/AUC for calibration and cross-validation results
###############################################################################

cat("\nExample 2: ROC/AUC for calibration and CV results\n")
cat("==================================================\n\n")

# Calculate ROC for calibration results
cat("\nCalibration set ROC/AUC:\n")
roc.cal <- getROC(model$calres)
print(roc.cal)

# Calculate ROC for cross-validation results
cat("\nCross-validation ROC/AUC:\n")
roc.cv <- getROC(model$cvres)
print(roc.cv)

# Compare ROC curves for calibration, CV, and test sets
cat("\nComparing ROC curves for different datasets...\n")
p1 <- plotROC(model$calres, nc = 1, main = "ROC: Calibration")
p2 <- plotROC(model$cvres, nc = 1, main = "ROC: Cross-Validation")
p3 <- plotROC(test.res, nc = 1, main = "ROC: Test Set")
print(p1)
print(p2)
print(p3)

###############################################################################
# Example 3: One-class PLS-DA model
###############################################################################

cat("\nExample 3: One-class PLS-DA with ROC/AUC\n")
cat("=========================================\n\n")

# Create binary classification problem (virginica vs. others)
c.cal.bin <- c.cal == "virginica"
c.test.bin <- c.test == "virginica"

# Build one-class PLS-DA model
model.oneclass <- plsda(x.cal, c.cal.bin, ncomp = 3, cv = 1, 
                        classname = "virginica", 
                        info = "One-class: virginica")

# Select optimal number of components
model.oneclass <- selectCompNum(model.oneclass, 1)

# Apply to test set
test.res.oneclass <- predict(model.oneclass, x.test, c.test.bin)

# Calculate and plot ROC
cat("\nOne-class model ROC/AUC:\n")
roc.oneclass <- getROC(test.res.oneclass)
print(roc.oneclass)

p <- plotROC(test.res.oneclass, main = "ROC Curve - One-class Model (virginica)")
print(p)

###############################################################################
# Example 4: ROC curves with different numbers of components
###############################################################################

cat("\nExample 4: Comparing ROC curves for different component numbers\n")
cat("================================================================\n\n")

# Build model with more components
model.multi <- plsda(x.cal, c.cal, ncomp = 5, cv = 1)
test.res.multi <- predict(model.multi, x.test, c.test)

# Calculate AUC for different numbers of components
cat("\nAUC values for different numbers of components (class: setosa):\n")
for (nc in 1:5) {
   roc.temp <- getROC(test.res.multi, ncomp = nc, nc = 1)
   cat(sprintf("  %d components: AUC = %.4f\n", nc, roc.temp$auc[1]))
}

# Plot ROC curves for different component numbers
cat("\nPlotting ROC curves for different component numbers...\n")
for (nc in 1:5) {
   p <- plotROC(test.res.multi, ncomp = nc, nc = 1, 
           main = sprintf("ROC: %d comp(s)", nc))
   print(p)
}

###############################################################################
# Example 5: Customizing ROC plots
###############################################################################

cat("\nExample 5: Customizing ROC plots\n")
cat("=================================\n\n")

# Plot with custom colors and without AUC in legend
p1 <- plotROC(test.res, col = c("red", "blue", "green"), 
        show.auc = FALSE, 
        main = "Custom ROC Curves (No AUC)",
        legend.position = "bottom")
print(p1)

# Plot with different line width
p2 <- plotROC(test.res, lwd = 1.5,
        main = "ROC Curves with Custom Line Width",
        legend.position = "right")
print(p2)

# Plot without legend
p3 <- plotROC(test.res, legend.position = "none",
        main = "ROC Curves (No Legend)")
print(p3)

cat("\n\nAll examples completed!\n")
cat("Note: Review the generated plots to verify ROC/AUC functionality.\n\n")
