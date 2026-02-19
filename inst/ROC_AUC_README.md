# ROC/AUC Functionality for PLS-DA Models

This document describes the ROC (Receiver Operating Characteristic) curve and AUC (Area Under the Curve) functionality added to the mdatools package for PLS-DA models.

## Overview

The ROC/AUC functionality provides a way to evaluate the performance of PLS-DA classification models, similar to the `auroc` function in the mixOmics package. This implementation includes:

- **ROC curve calculation**: Computes True Positive Rate (TPR) and False Positive Rate (FPR) at various classification thresholds
- **AUC calculation**: Computes the Area Under the ROC Curve using trapezoidal integration
- **Visualization**: Plots ROC curves using ggplot2 with customizable appearance and optional AUC values in legends

## Functions

### `getROC(obj, ncomp = obj$ncomp.selected, nc = seq_len(obj$nclasses))`

Calculates ROC curves and AUC values for classification results.

**Arguments:**
- `obj`: Classification results object (e.g., `plsdares`, `classres`)
- `ncomp`: Number of components to use (default: selected optimal value)
- `nc`: Which class(es) to calculate ROC/AUC for (default: all classes)

**Returns:**
A list with class "roc" containing:
- `roc`: List of ROC curve data for each class (TPR, FPR, thresholds)
- `auc`: Named vector of AUC values for each class
- `ncomp`: Number of components used
- `classnames`: Names of the classes

**Example:**
```r
library(mdatools)
data(iris)

# Create PLS-DA model
x.cal <- iris[seq(1, 150, 2), 1:4]
c.cal <- iris[seq(1, 150, 2), 5]
x.test <- iris[seq(2, 150, 2), 1:4]
c.test <- iris[seq(2, 150, 2), 5]

model <- plsda(x.cal, c.cal, ncomp = 3, cv = 1)
test.res <- predict(model, x.test, c.test)

# Calculate ROC/AUC
roc.res <- getROC(test.res)
print(roc.res)
```

### `plotROC(obj, ncomp = NULL, nc = NULL, show.auc = TRUE, ...)`

Creates a ggplot2 plot of ROC curves.

**Arguments:**
- `obj`: Classification results object or a `roc` object from `getROC()`
- `ncomp`: Number of components to use (default: selected optimal value)
- `nc`: Which class(es) to plot (default: all classes)
- `show.auc`: Whether to show AUC values in the legend (default: TRUE)
- `main`: Main title for the plot
- `legend.position`: Position of legend ("bottom", "top", "left", "right", or "none")
- `col`: Vector of colors for each class
- `lwd`: Line width

**Returns:**
A ggplot2 plot object that can be displayed with `print()` or further customized.

**Example:**
```r
# Plot ROC curves for all classes
p <- plotROC(test.res, main = "ROC Curves - Test Set")
print(p)

# Plot for specific classes only
plotROC(test.res, nc = 1:2)

# Customize appearance
plotROC(test.res, col = c("red", "blue", "green"), lwd = 2)
```

### `print.roc(x, ...)`

Prints a summary of ROC results including AUC values for each class.

## Use Cases

### 1. Multi-class Classification

```r
# Build multi-class PLS-DA model
model <- plsda(x.cal, c.cal, ncomp = 3, cv = 1)
test.res <- predict(model, x.test, c.test)

# Calculate and plot ROC for all classes
roc.res <- getROC(test.res)
plotROC(test.res)
```

### 2. One-class Classification

```r
# Build one-class PLS-DA model
c.cal.bin <- c.cal == "virginica"
c.test.bin <- c.test == "virginica"

model <- plsda(x.cal, c.cal.bin, ncomp = 3, cv = 1, classname = "virginica")
test.res <- predict(model, x.test, c.test.bin)

# Calculate and plot ROC
roc.res <- getROC(test.res)
plotROC(test.res)
```

### 3. Comparing Calibration, Cross-Validation, and Test Results

```r
model <- plsda(x.cal, c.cal, ncomp = 3, cv = 1)
test.res <- predict(model, x.test, c.test)

# Compare ROC curves
par(mfrow = c(1, 3))
plotROC(model$calres, main = "Calibration")
plotROC(model$cvres, main = "Cross-Validation")
plotROC(test.res, main = "Test Set")
par(mfrow = c(1, 1))
```

### 4. Evaluating Different Numbers of Components

```r
model <- plsda(x.cal, c.cal, ncomp = 5, cv = 1)
test.res <- predict(model, x.test, c.test)

# Calculate AUC for different component numbers
for (nc in 1:5) {
   roc.temp <- getROC(test.res, ncomp = nc, nc = 1)
   cat(sprintf("%d components: AUC = %.4f\n", nc, roc.temp$auc[1]))
}

# Plot ROC curves for different component numbers
par(mfrow = c(2, 3))
for (nc in 1:5) {
   plotROC(test.res, ncomp = nc, nc = 1, 
           main = sprintf("ROC: %d comp(s)", nc))
}
par(mfrow = c(1, 1))
```

## Implementation Details

### ROC Curve Calculation

The ROC curve is constructed by:
1. Using continuous prediction values (`p.pred`) from the PLS-DA model
2. Sorting samples by prediction scores in descending order
3. Calculating TPR and FPR at each unique threshold
4. Computing cumulative true positives and false positives

### AUC Calculation

The AUC is calculated using the trapezoidal rule:
- AUC = 1.0: Perfect classifier
- AUC = 0.5: Random classifier
- AUC < 0.5: Worse than random (predictions are inverted)

### Compatibility

The ROC/AUC functions work with:
- Multi-class PLS-DA models
- One-class PLS-DA models
- Calibration results (`model$calres`)
- Cross-validation results (`model$cvres`)
- Test set results (`predict(model, x.test, c.test)`)

## Comparison with mixOmics

This implementation is inspired by the `auroc` function from the mixOmics package and provides similar functionality:

**Similarities:**
- Calculates ROC curves and AUC for PLS-DA models
- Uses continuous predictions rather than hard class assignments
- Provides plotting functionality with customizable options
- Works with multi-class classification

**Differences:**
- Integration with mdatools package structure and conventions
- Uses mdatools plotting system for consistency
- Compatible with mdatools cross-validation and preprocessing workflows

## References

- Rohart F, Gautier B, Singh A, Lê Cao KA (2017). mixOmics: An R package for 'omics feature selection and multiple data integration. PLoS Comput Biol 13(11): e1005752.

## See Also

- `plsda()` - PLS-DA model building
- `predict.plsda()` - PLS-DA predictions
- `plotPerformance.classres()` - Classification performance plots
- `plotSensitivity.classres()` - Sensitivity plots
- `plotSpecificity.classres()` - Specificity plots
