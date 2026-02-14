# Examples for mdatools Package

This directory contains example scripts demonstrating the use of various features in the mdatools package.

## ROC/AUC Example

**File:** `roc_auc_example.R`

Demonstrates how to calculate ROC (Receiver Operating Characteristic) curves and AUC (Area Under the Curve) values for PLS-DA models. This functionality is similar to the `auroc` function from the mixOmics package.

The example covers:
- Multi-class PLS-DA models with ROC/AUC calculation
- ROC/AUC for calibration, cross-validation, and test results
- One-class PLS-DA models
- Comparing ROC curves for different numbers of components
- Customizing ROC plot appearance

### Running the Example

```r
# From R console
source(system.file("examples/roc_auc_example.R", package = "mdatools"))
```

### Key Functions

- `getROC()` - Calculate ROC curves and AUC values for classification results
- `plotROC()` - Plot ROC curves with optional AUC values in legend
- `print.roc()` - Print summary of ROC results

### Example Output

The functions calculate AUC values for each class and generate ROC curve plots showing:
- True Positive Rate (Sensitivity) vs. False Positive Rate (1 - Specificity)
- Diagonal reference line representing a random classifier
- AUC values in the legend for each class
