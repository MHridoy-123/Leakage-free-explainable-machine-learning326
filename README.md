# Leakage-Controlled WQI Modelling

This repository contains the computational workflow associated with the manuscript:

**Leakage-controlled explainable machine learning for freshwater WQI prediction and candidate monitoring reduction**

## Analysis workflow

- Five-fold cross-validation repeated ten times
- Training-fold median imputation
- Training-fold scaling for scale-sensitive models
- Fold-specific Random Forest predictor ranking
- Paired full-versus-reduced Random Forest comparison
- Predictor-ranking stability across 50 validation folds
- Repeated three-class classification
- Mean, standard deviation, and 95% confidence intervals

## Installation

```bash
python -m pip install -r requirements.txt


Leakage-Controlled WQI Modelling
This repository contains the computational workflow associated with the manuscript:

Leakage-controlled explainable machine learning for freshwater WQI prediction and candidate monitoring reduction

Analysis workflow
Five-fold cross-validation repeated ten times
Training-fold median imputation
Training-fold scaling for scale-sensitive models
Fold-specific Random Forest predictor ranking
Paired full-versus-reduced Random Forest comparison
Predictor-ranking stability across 50 validation folds
Repeated three-class classification
Mean, standard deviation, and 95% confidence intervals
Installation
python -m pip install -r requirements.txt




python code/repeated_wqi_validation.py \
  --data "data/water_quality.xlsx" \
  --target WQI \
  --output results
