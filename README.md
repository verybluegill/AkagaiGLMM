# AkagaiGLMM

## Overview

This repository contains the analysis workflow for CPUE standardization and visualization based on akagai fishing logbook data from the Watahama Fisheries Cooperative Association (渡波漁船組合).

The scripts in this repository are used to:

- check raw logbook data
- clean and restructure the data for analysis
- fit GLMMs for CPUE standardization
- compare candidate models
- export tables and figures for interpretation and reporting

This is a research code repository prepared for public sharing. The main intended readers are collaborators, but the structure and outputs are also described for external readers.

## Analysis flow

Run the scripts manually, one by one, in the following order:

`01_check_raw_data.R` → `02_make_clean_data_and_figures.R` → `03_fit_glmm_models.R`

What each step does:

- `R/01_check_raw_data.R`
  reads `ActualData/Akagai_sheet.xlsx`, checks raw values, and writes check tables and check figures to `output/`
- `R/02_make_clean_data_and_figures.R`
  reads the check tables, creates cleaned datasets for downstream analysis, and writes additional summary tables and figures
- `R/03_fit_glmm_models.R`
  reads `data_processed/akagai_glmm_input.csv`, fits candidate GLMMs, compares models by AIC, and exports standardized indices and comparison figures

Example commands in R:

```r
source(file.path("R", "01_check_raw_data.R"))
source(file.path("R", "02_make_clean_data_and_figures.R"))
source(file.path("R", "03_fit_glmm_models.R"))
```

## Required data

To run the workflow, you must manually place the real input file at:

- `ActualData/Akagai_sheet.xlsx`

The real dataset is not included in this public repository.

For reference on the expected workbook format, the repository includes:

- `ActualData/example_Akagai_sheet.xlsx`

The repository also keeps supporting files that are used by the existing scripts, including:

- `ActualData/AreaLonLat.csv`

`ActualData/` itself is kept in the repository, but the real workbook `ActualData/Akagai_sheet.xlsx` is excluded from version control.

## How to run

1. Place the real workbook as `ActualData/Akagai_sheet.xlsx`.
2. Open the project in R or RStudio.
3. Run the three scripts manually in order.
4. Check intermediate files in `output/` and processed datasets in `data_processed/` if needed.
5. For public sharing, copy selected figures and tables from the local analysis outputs into `docs/results/`.

The repository is written with manual stepwise execution in mind rather than a single pipeline command.

## Main results

For the public-facing repository, the main outputs to highlight are:

- `docs/results/aic_compare_all.csv`
- `docs/results/overlay_best_combined_2x2.png`

`aic_compare_all.csv` is the integrated model comparison table across response variables. It summarizes candidate-model fit statistics, including AIC and delta AIC, and is intended to show how model choice was evaluated on a common comparison dataset for each response.

`overlay_best_combined_2x2.png` is the main summary figure for the four size classes (`chu`, `dai`, `toku`, `tokudai`). It overlays the standardized index from the selected GLMM and the corresponding raw relative CPUE, making it easier to see where model-based standardization and unstandardized annual patterns are similar or different.

When `docs/results/overlay_best_combined_2x2.png` is available in the public repository, it can be embedded as follows:

![Combined overlay of raw relative CPUE and standardized indices](docs/results/overlay_best_combined_2x2.png)

Interpretation should remain cautious. Broadly, the standardized index is useful for examining year-to-year variation after adjusting for modeled effects such as area, vessel, month, effort, and, where selected, depth structure. By contrast, raw CPUE is the unstandardized catch-per-unit-effort summary and can reflect changes in sampling composition as well as changes in catch rates. Differences between the two may therefore suggest the influence of covariate structure, not only biological change.

## Important notes

- Depth missingness:
  records with missing or unusable depth values are retained in earlier processing where possible, but `depth_glmm` becomes `NA` when both depth columns are missing or when both recorded depths exceed the threshold rule used in cleaning. Depth-dependent model comparisons are therefore based on rows with usable depth information.
- Speed and duration replacement rules:
  in `R/02_make_clean_data_and_figures.R`, speed is replaced with `3` knots when missing, `<= 0.5`, or `> 5`. Duration is replaced with `60` minutes when missing, `<= 10`, or `> 90`.
- Standardized CPUE index:
  the standardized index is an index derived from the fitted model and should not be interpreted as absolute stock abundance or absolute biomass.
- Model comparison subset:
  candidate models are compared on the same subset within each response so that AIC differences reflect model structure rather than changing input rows.

## Outputs

During local analysis, the scripts write intermediate and final files mainly to:

- `output/`
- `data_processed/`

These directories are intentionally excluded from version control because they contain generated products from local runs.

For the public repository, readers should first look at curated outputs under:

- `docs/results/`

Recommended public-facing files include the main model-comparison table and selected figures such as:

- `docs/results/aic_compare_all.csv`
- `docs/results/overlay_best_combined_2x2.png`

Other files such as `best_model_table.csv` and `best_model_summary.csv` can still be useful internally, but they are not the primary entry points for the public README.
