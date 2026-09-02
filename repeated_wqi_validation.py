#!/usr/bin/env python3
"""
Reproducible repeated-validation analysis for:
"Leakage-controlled explainable machine learning for freshwater WQI
prediction and candidate monitoring reduction"

Workflow
--------
1. Read CSV or Excel water-quality dataset.
2. Exclude WQI-derived sub-index variables.
3. Evaluate six regression algorithms by 5-fold CV repeated 10 times.
4. Fit all preprocessing operations within each training partition.
5. Recalculate Random Forest feature ranking within each training fold.
6. Compare full RF with a fold-specific reduced top-four RF on identical folds.
7. Quantify predictor-ranking stability across 50 training folds.
8. Evaluate 3-class Random Forest classification using repeated held-out predictions.
9. Export numerical results, held-out predictions, and publication-ready figures.

Important interpretation
------------------------
- This is repeated cross-validation with prespecified model settings, not nested CV.
- The reduced top-four model is a candidate configuration, not an optimized protocol.
- Feature importance and perturbation outputs describe predictive association, not causality.

Example
-------
python repeated_wqi_validation.py --data water_quality.xlsx --sheet 0 --target WQI --output results
"""

from __future__ import annotations

import argparse
import json
import platform
import sys
from collections import Counter, defaultdict
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sklearn
from scipy import stats
from sklearn.base import clone
from sklearn.compose import TransformedTargetRegressor
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.linear_model import ElasticNet, Lasso, LinearRegression, Ridge
from sklearn.metrics import (
    accuracy_score,
    balanced_accuracy_score,
    cohen_kappa_score,
    confusion_matrix,
    f1_score,
    mean_absolute_error,
    mean_squared_error,
    precision_recall_fscore_support,
    r2_score,
    recall_score,
)
from sklearn.model_selection import RepeatedKFold, RepeatedStratifiedKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR

RANDOM_SEED = 326
N_SPLITS = 5
N_REPEATS = 10
TOP_K = 4
RF_TREES = 1000

# Add exact dataset-specific sub-index column names here if required.
DEFAULT_EXCLUDE_PATTERNS = (
    "subindex", "sub_index", "sub-index", "sub index", "qi_", "quality_rating"
)


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Repeated leakage-controlled WQI validation")
    p.add_argument("--data", required=True, help="Input .csv, .xlsx, or .xls file")
    p.add_argument("--sheet", default="0", help="Excel sheet name or zero-based index")
    p.add_argument("--target", default="WQI", help="Continuous target column")
    p.add_argument("--id-column", default=None, help="Optional sample-ID column to exclude")
    p.add_argument("--exclude", nargs="*", default=[], help="Additional predictor columns to exclude")
    p.add_argument("--output", default="wqi_repeated_cv_results", help="Output directory")
    p.add_argument("--seed", type=int, default=RANDOM_SEED)
    return p.parse_args()


def read_data(path: Path, sheet: str) -> pd.DataFrame:
    suffix = path.suffix.lower()
    if suffix == ".csv":
        return pd.read_csv(path)
    if suffix in {".xlsx", ".xls"}:
        sheet_arg = int(sheet) if str(sheet).isdigit() else sheet
        engine = "openpyxl" if suffix == ".xlsx" else "xlrd"
        return pd.read_excel(path, sheet_name=sheet_arg, engine=engine)
    raise ValueError("Input must be CSV, XLSX, or XLS.")


def normalize_name(name: str) -> str:
    return "".join(ch.lower() if ch.isalnum() else "_" for ch in str(name)).strip("_")


def prepare_xy(df: pd.DataFrame, target: str, id_column: str | None, extra_exclude: list[str]):
    if target not in df.columns:
        matches = [c for c in df.columns if normalize_name(c) == normalize_name(target)]
        if len(matches) != 1:
            raise KeyError(f"Target '{target}' not found. Available columns: {list(df.columns)}")
        target = matches[0]

    excluded = set(extra_exclude)
    if id_column:
        excluded.add(id_column)

    for col in df.columns:
        n = normalize_name(col)
        if col != target and any(p.replace("-", "_").replace(" ", "_") in n for p in DEFAULT_EXCLUDE_PATTERNS):
            excluded.add(col)

    predictor_cols = [c for c in df.columns if c != target and c not in excluded]
    X = df[predictor_cols].apply(pd.to_numeric, errors="coerce")
    y = pd.to_numeric(df[target], errors="coerce")

    valid = y.notna()
    X, y = X.loc[valid].reset_index(drop=True), y.loc[valid].reset_index(drop=True)
    all_missing = X.columns[X.isna().all()].tolist()
    if all_missing:
        X = X.drop(columns=all_missing)
        excluded.update(all_missing)

    if len(X) < N_SPLITS:
        raise ValueError("Too few valid observations for five-fold cross-validation.")
    return X, y, predictor_cols, sorted(excluded), target


def ci95(values: np.ndarray) -> tuple[float, float]:
    values = np.asarray(values, dtype=float)
    mean = values.mean()
    if len(values) < 2:
        return mean, mean
    half = stats.t.ppf(0.975, len(values) - 1) * values.std(ddof=1) / np.sqrt(len(values))
    return mean - half, mean + half


def regression_models(seed: int):
    scaled = lambda model: Pipeline([
        ("imputer", SimpleImputer(strategy="median")),
        ("scaler", StandardScaler()),
        ("model", model),
    ])
    return {
        "Linear regression": scaled(LinearRegression()),
        "Ridge": scaled(Ridge(alpha=10.0)),
        "Lasso": scaled(Lasso(alpha=0.10, max_iter=100000, random_state=seed)),
        "Elastic Net": scaled(ElasticNet(alpha=0.10, l1_ratio=0.50, max_iter=100000, random_state=seed)),
        "SVR": scaled(SVR(kernel="rbf", C=10.0, gamma="scale", epsilon=0.10)),
        "Random Forest": Pipeline([
            ("imputer", SimpleImputer(strategy="median")),
            ("model", RandomForestRegressor(
                n_estimators=RF_TREES,
                max_features="sqrt",
                min_samples_leaf=1,
                random_state=seed,
                n_jobs=-1,
            )),
        ]),
    }


def regression_metrics(y_true, y_pred):
    return {
        "RMSE": mean_squared_error(y_true, y_pred) ** 0.5,
        "MAE": mean_absolute_error(y_true, y_pred),
        "R2": r2_score(y_true, y_pred),
    }


def repeated_regression(X, y, seed: int):
    rkf = RepeatedKFold(n_splits=N_SPLITS, n_repeats=N_REPEATS, random_state=seed)
    models = regression_models(seed)
    n = len(y)
    predictions = {name: np.full((N_REPEATS, n), np.nan) for name in models}
    fold_rows = []

    for split_index, (train_idx, test_idx) in enumerate(rkf.split(X, y)):
        repeat = split_index // N_SPLITS + 1
        fold = split_index % N_SPLITS + 1
        for name, estimator in models.items():
            fitted = clone(estimator)
            fitted.fit(X.iloc[train_idx], y.iloc[train_idx])
            pred = fitted.predict(X.iloc[test_idx])
            predictions[name][repeat - 1, test_idx] = pred
            m = regression_metrics(y.iloc[test_idx], pred)
            fold_rows.append({"repeat": repeat, "fold": fold, "model": name, **m})

    repeat_rows, oof_rows = [], []
    for name, matrix in predictions.items():
        if np.isnan(matrix).any():
            raise RuntimeError(f"Missing held-out predictions for {name}")
        for repeat in range(1, N_REPEATS + 1):
            pred = matrix[repeat - 1]
            repeat_rows.append({"repeat": repeat, "model": name, **regression_metrics(y, pred)})
            for i, p in enumerate(pred):
                oof_rows.append({"repeat": repeat, "row_index": i, "observed": y.iloc[i], "predicted": p, "model": name})
    return pd.DataFrame(fold_rows), pd.DataFrame(repeat_rows), pd.DataFrame(oof_rows)


def summarize_regression(repeat_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for model, g in repeat_df.groupby("model"):
        row = {"Model": model}
        for metric in ["RMSE", "MAE", "R2"]:
            vals = g[metric].to_numpy()
            lo, hi = ci95(vals)
            row.update({
                f"{metric}_mean": vals.mean(),
                f"{metric}_SD": vals.std(ddof=1),
                f"{metric}_CI_low": lo,
                f"{metric}_CI_high": hi,
            })
        rows.append(row)
    return pd.DataFrame(rows).sort_values("RMSE_mean").reset_index(drop=True)


def full_reduced_and_stability(X, y, seed: int):
    rkf = RepeatedKFold(n_splits=N_SPLITS, n_repeats=N_REPEATS, random_state=seed)
    n = len(y)
    full_pred = np.full((N_REPEATS, n), np.nan)
    reduced_pred = np.full((N_REPEATS, n), np.nan)
    ranking_rows, prediction_rows = [], []

    for split_index, (train_idx, test_idx) in enumerate(rkf.split(X, y)):
        repeat = split_index // N_SPLITS + 1
        fold = split_index % N_SPLITS + 1
        imputer = SimpleImputer(strategy="median")
        X_train = pd.DataFrame(imputer.fit_transform(X.iloc[train_idx]), columns=X.columns, index=train_idx)
        X_test = pd.DataFrame(imputer.transform(X.iloc[test_idx]), columns=X.columns, index=test_idx)

        ranker = RandomForestRegressor(
            n_estimators=RF_TREES, max_features="sqrt", min_samples_leaf=1,
            random_state=seed + split_index, n_jobs=-1,
        )
        ranker.fit(X_train, y.iloc[train_idx])
        importance = pd.Series(ranker.feature_importances_, index=X.columns).sort_values(ascending=False)
        rank = importance.rank(ascending=False, method="min")
        selected = importance.head(TOP_K).index.tolist()

        for col in X.columns:
            ranking_rows.append({
                "repeat": repeat, "fold": fold, "predictor": col,
                "importance": importance[col], "rank": rank[col],
                "top4": int(col in selected),
            })

        full = RandomForestRegressor(
            n_estimators=RF_TREES, max_features="sqrt", min_samples_leaf=1,
            random_state=seed + split_index, n_jobs=-1,
        )
        full.fit(X_train, y.iloc[train_idx])
        pf = full.predict(X_test)

        reduced = RandomForestRegressor(
            n_estimators=RF_TREES, max_features="sqrt", min_samples_leaf=1,
            random_state=seed + split_index, n_jobs=-1,
        )
        reduced.fit(X_train[selected], y.iloc[train_idx])
        pr = reduced.predict(X_test[selected])
        full_pred[repeat - 1, test_idx] = pf
        reduced_pred[repeat - 1, test_idx] = pr

        selected_text = "; ".join(selected)
        for pos, idx in enumerate(test_idx):
            prediction_rows.append({
                "repeat": repeat, "fold": fold, "row_index": int(idx),
                "observed": y.iloc[idx], "full_prediction": pf[pos],
                "reduced_prediction": pr[pos], "selected_predictors": selected_text,
            })

    comparison_rows = []
    for repeat in range(1, N_REPEATS + 1):
        fm = regression_metrics(y, full_pred[repeat - 1])
        rm = regression_metrics(y, reduced_pred[repeat - 1])
        comparison_rows.append({
            "repeat": repeat,
            "Full_RMSE": fm["RMSE"], "Reduced_RMSE": rm["RMSE"],
            "Delta_RMSE": rm["RMSE"] - fm["RMSE"],
            "Full_MAE": fm["MAE"], "Reduced_MAE": rm["MAE"],
            "Full_R2": fm["R2"], "Reduced_R2": rm["R2"],
            "Lower_RMSE_model": "Full" if fm["RMSE"] < rm["RMSE"] else "Reduced",
        })

    ranking = pd.DataFrame(ranking_rows)
    stability = ranking.groupby("predictor").agg(
        Top4_frequency_pct=("top4", lambda x: 100 * x.mean()),
        Median_rank=("rank", "median"),
        Rank_Q1=("rank", lambda x: x.quantile(0.25)),
        Rank_Q3=("rank", lambda x: x.quantile(0.75)),
        Mean_importance=("importance", "mean"),
    ).reset_index()
    stability["Rank_IQR"] = stability["Rank_Q3"] - stability["Rank_Q1"]
    stability = stability.sort_values(["Top4_frequency_pct", "Mean_importance"], ascending=[False, False])
    return pd.DataFrame(comparison_rows), stability, ranking, pd.DataFrame(prediction_rows)


def classify_labels(y: pd.Series):
    q1, q2 = y.quantile([1/3, 2/3]).tolist()
    labels = pd.cut(y, bins=[-np.inf, q1, q2, np.inf], labels=["Low", "Moderate", "Better"], include_lowest=True)
    return labels.astype(str), q1, q2


def specificity_multiclass(y_true, y_pred, labels):
    cm = confusion_matrix(y_true, y_pred, labels=labels)
    total = cm.sum()
    out = {}
    for i, label in enumerate(labels):
        tp = cm[i, i]
        fn = cm[i, :].sum() - tp
        fp = cm[:, i].sum() - tp
        tn = total - tp - fn - fp
        out[label] = tn / (tn + fp) if (tn + fp) else np.nan
    return out


def repeated_classification(X, y_cont, seed: int):
    y, q1, q2 = classify_labels(y_cont)
    labels = ["Low", "Moderate", "Better"]
    rskf = RepeatedStratifiedKFold(n_splits=N_SPLITS, n_repeats=N_REPEATS, random_state=seed)
    n = len(y)
    pred_matrix = np.empty((N_REPEATS, n), dtype=object)
    pred_matrix[:] = None
    oof_rows = []

    for split_index, (train_idx, test_idx) in enumerate(rskf.split(X, y)):
        repeat = split_index // N_SPLITS + 1
        fold = split_index % N_SPLITS + 1
        model = Pipeline([
            ("imputer", SimpleImputer(strategy="median")),
            ("model", RandomForestClassifier(
                n_estimators=RF_TREES, max_features="sqrt", min_samples_leaf=1,
                class_weight="balanced", random_state=seed + split_index, n_jobs=-1,
            )),
        ])
        model.fit(X.iloc[train_idx], y.iloc[train_idx])
        pred = model.predict(X.iloc[test_idx])
        pred_matrix[repeat - 1, test_idx] = pred
        for pos, idx in enumerate(test_idx):
            oof_rows.append({"repeat": repeat, "fold": fold, "row_index": int(idx), "observed_class": y.iloc[idx], "predicted_class": pred[pos]})

    repeat_rows = []
    all_true, all_pred = [], []
    for repeat in range(1, N_REPEATS + 1):
        pred = pred_matrix[repeat - 1].astype(str)
        true = y.to_numpy()
        repeat_rows.append({
            "repeat": repeat,
            "Accuracy": accuracy_score(true, pred),
            "Balanced_accuracy": balanced_accuracy_score(true, pred),
            "Macro_F1": f1_score(true, pred, labels=labels, average="macro", zero_division=0),
            "Kappa": cohen_kappa_score(true, pred, labels=labels),
        })
        all_true.extend(true); all_pred.extend(pred)

    repeat_df = pd.DataFrame(repeat_rows)
    summary_rows = []
    for metric in ["Accuracy", "Balanced_accuracy", "Macro_F1", "Kappa"]:
        vals = repeat_df[metric].to_numpy(); lo, hi = ci95(vals)
        summary_rows.append({"Metric": metric, "Mean": vals.mean(), "SD": vals.std(ddof=1), "CI_low": lo, "CI_high": hi})

    precision, recall, f1, support = precision_recall_fscore_support(all_true, all_pred, labels=labels, zero_division=0)
    spec = specificity_multiclass(all_true, all_pred, labels)
    classwise = pd.DataFrame({
        "Class": labels, "Precision": precision, "Sensitivity": recall,
        "Specificity": [spec[x] for x in labels], "F1": f1, "Support": support,
    })
    cm = pd.DataFrame(confusion_matrix(all_true, all_pred, labels=labels), index=[f"Observed_{x}" for x in labels], columns=[f"Predicted_{x}" for x in labels])
    distribution = pd.DataFrame({"Class": labels, "Unique_observations": [(y == x).sum() for x in labels]})
    return y, q1, q2, repeat_df, pd.DataFrame(summary_rows), classwise, cm, distribution, pd.DataFrame(oof_rows)


def make_figures(output: Path, reg_summary, stability, cm):
    # Figure 1: model performance excluding unstable linear model for readability.
    plot_df = reg_summary[reg_summary["Model"] != "Linear regression"].sort_values("RMSE_mean")
    fig, ax = plt.subplots(figsize=(8, 5), constrained_layout=True)
    ax.bar(plot_df["Model"], plot_df["RMSE_mean"], yerr=plot_df["RMSE_SD"], capsize=4)
    ax.set_ylabel("RMSE")
    ax.set_title("Repeated held-out regression performance")
    ax.tick_params(axis="x", rotation=30, labelsize=10)
    ax.tick_params(axis="y", labelsize=10)
    fig.savefig(output / "Figure_model_performance.png", dpi=600, bbox_inches="tight")
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(8, 5), constrained_layout=True)
    p = stability.sort_values("Top4_frequency_pct", ascending=True)
    ax.barh(p["predictor"], p["Top4_frequency_pct"])
    ax.set_xlabel("Top-four inclusion frequency (%)")
    ax.set_title("Predictor-ranking stability across 50 validation folds")
    ax.tick_params(labelsize=10)
    fig.savefig(output / "Figure_feature_stability.png", dpi=600, bbox_inches="tight")
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(6, 5), constrained_layout=True)
    im = ax.imshow(cm.values, cmap="Blues")
    ax.set_xticks(range(len(cm.columns)), [x.replace("Predicted_", "") for x in cm.columns])
    ax.set_yticks(range(len(cm.index)), [x.replace("Observed_", "") for x in cm.index])
    ax.set_xlabel("Predicted class"); ax.set_ylabel("Observed class")
    ax.set_title("Aggregated repeated held-out confusion matrix")
    for i in range(cm.shape[0]):
        for j in range(cm.shape[1]):
            ax.text(j, i, int(cm.iloc[i, j]), ha="center", va="center", fontsize=12)
    fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    fig.savefig(output / "Figure_confusion_matrix.png", dpi=600, bbox_inches="tight")
    plt.close(fig)


def main():
    args = parse_args()
    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=True)
    df = read_data(Path(args.data), args.sheet)
    X, y, original_predictors, excluded, target = prepare_xy(df, args.target, args.id_column, args.exclude)

    fold_reg, repeat_reg, oof_reg = repeated_regression(X, y, args.seed)
    reg_summary = summarize_regression(repeat_reg)
    comparison, stability, fold_rankings, paired_oof = full_reduced_and_stability(X, y, args.seed)
    classes, q1, q2, cls_repeat, cls_summary, classwise, cm, class_dist, cls_oof = repeated_classification(X, y, args.seed)

    full_summary = {
        "Full_RMSE_mean": comparison["Full_RMSE"].mean(),
        "Full_RMSE_SD": comparison["Full_RMSE"].std(ddof=1),
        "Reduced_RMSE_mean": comparison["Reduced_RMSE"].mean(),
        "Reduced_RMSE_SD": comparison["Reduced_RMSE"].std(ddof=1),
        "Full_R2_mean": comparison["Full_R2"].mean(),
        "Reduced_R2_mean": comparison["Reduced_R2"].mean(),
        "Full_lower_RMSE_pct": 100 * (comparison["Lower_RMSE_model"] == "Full").mean(),
    }

    with pd.ExcelWriter(output / "Repeated_WQI_validation_results.xlsx", engine="openpyxl") as writer:
        reg_summary.to_excel(writer, sheet_name="Regression summary", index=False)
        repeat_reg.to_excel(writer, sheet_name="Regression repeats", index=False)
        fold_reg.to_excel(writer, sheet_name="Regression folds", index=False)
        oof_reg.to_excel(writer, sheet_name="Regression OOF", index=False)
        comparison.to_excel(writer, sheet_name="Full reduced repeats", index=False)
        paired_oof.to_excel(writer, sheet_name="Full reduced OOF", index=False)
        stability.to_excel(writer, sheet_name="Feature stability", index=False)
        fold_rankings.to_excel(writer, sheet_name="Fold rankings", index=False)
        cls_summary.to_excel(writer, sheet_name="Classification summary", index=False)
        cls_repeat.to_excel(writer, sheet_name="Classification repeats", index=False)
        classwise.to_excel(writer, sheet_name="Classwise", index=False)
        cm.to_excel(writer, sheet_name="Confusion matrix")
        class_dist.to_excel(writer, sheet_name="Class distribution", index=False)
        cls_oof.to_excel(writer, sheet_name="Classification OOF", index=False)

    make_figures(output, reg_summary, stability, cm)
    metadata = {
        "input_file": str(Path(args.data).resolve()),
        "target": target,
        "n_observations": len(y),
        "n_predictors": X.shape[1],
        "predictors": X.columns.tolist(),
        "excluded_columns": excluded,
        "random_seed": args.seed,
        "validation": f"{N_SPLITS}-fold cross-validation repeated {N_REPEATS} times",
        "classification_tertiles": {"q1": q1, "q2": q2},
        "paired_full_reduced_summary": full_summary,
        "python": sys.version,
        "platform": platform.platform(),
        "pandas": pd.__version__,
        "numpy": np.__version__,
        "scikit_learn": sklearn.__version__,
    }
    (output / "analysis_metadata.json").write_text(json.dumps(metadata, indent=2, default=float), encoding="utf-8")

    print("Analysis completed.")
    print(reg_summary.to_string(index=False))
    print("\nFull-versus-reduced summary:")
    print(json.dumps(full_summary, indent=2))
    print(f"\nOutputs saved to: {output.resolve()}")


if __name__ == "__main__":
    main()
