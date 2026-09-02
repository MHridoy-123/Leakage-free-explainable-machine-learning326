# Repeated WQI validation code

This repository script reproduces the reviewer-requested repeated-validation workflow used in the revised manuscript.

## Main analysis

- Five-fold cross-validation repeated ten times
- Training-fold median imputation
- Training-fold scaling for scale-sensitive models
- Six regression algorithms
- Fold-specific Random Forest predictor ranking
- Paired full-versus-reduced Random Forest comparison
- Predictor stability over 50 validation folds
- Repeated three-class Random Forest classification
- Mean, SD, and 95% confidence intervals
- High-resolution PNG figures and Excel outputs

## Installation

```bash
python -m pip install -r requirements.txt
```

## Run

```bash
python repeated_wqi_validation.py   --data "water_quality.xlsx"   --sheet 0   --target WQI   --output results
```

If the dataset uses explicit WQI sub-index columns whose names are not automatically detected, exclude them manually:

```bash
python repeated_wqi_validation.py   --data "water_quality.xlsx"   --target WQI   --exclude "Turbidity sub-index" "pH sub-index" "TH sub-index" "Ca sub-index" "Mg sub-index" "NO3 sub-index"   --output results
```

## Interpretation note

This is repeated cross-validation with prespecified settings, not nested cross-validation. The fold-specific top-four model is a candidate reduced configuration. Importance and perturbation analyses describe predictive association and do not establish causality.
