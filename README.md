# AkagaiGLMM

## 概要

このリポジトリは、アカガイ操業日誌データを用いて、CPUE（単位努力量当たり漁獲量）の標準化と年変動の可視化を行うための解析ワークフローをまとめたものです。

本リポジトリでは、操業日誌データに対して以下を段階的に実施します。

- 生データのチェック
- 解析用データのクリーニングと再構造化
- GLMM（一般化線形混合モデル）によるCPUE標準化
- 候補モデルの比較
- 解釈・報告用の表および図の出力

同じ構造のデータが与えられれば、再現的に解析できるように設計しています。  

## 解析フロー

スクリプトは、以下の順に実行してください。

`01_check_raw_data.R` → `02_make_clean_data_and_figures.R` → `03_fit_glmm_models.R`

### 各ステップの内容

- `R/01_check_raw_data.R`  
  `ActualData/Akagai_sheet.xlsx` を読み込み、生データの値をチェックし、チェック用の表と図を `output/` に出力します。

- `R/02_make_clean_data_and_figures.R`  
  チェック結果を読み込み、解析用の整形済みデータセットを作成し、追加の集計表および図を出力します。

- `R/03_fit_glmm_models.R`  
  `data_processed/akagai_glmm_input.csv` を読み込み、候補GLMMを当てはめ、AIC によってモデル比較を行い、標準化指数および比較図を出力します。

### Rでの実行例

```r
source(file.path("R", "01_check_raw_data.R"))
source(file.path("R", "02_make_clean_data_and_figures.R"))
source(file.path("R", "03_fit_glmm_models.R"))
````


## 必要なデータ

このワークフローを実行するためには、実データの Excel ファイルを以下の場所に配置する必要があります。

* `ActualData/Akagai_sheet.xlsx`

この公開リポジトリには、**実データは含まれていません**。

入力ファイル形式の参考として、以下のダミーファイルを同梱しています。

* `ActualData/example_Akagai_sheet.xlsx`

また、既存スクリプトで使用する補助ファイルとして、以下も含まれています。

* `ActualData/AreaLonLat.csv`


## 実行方法

1. 実データファイルを `ActualData/Akagai_sheet.xlsx` として配置します。
2. R または RStudio でプロジェクトを開きます。
3. `01_check_raw_data.R`、`02_make_clean_data_and_figures.R`、`03_fit_glmm_models.R` の順で実行します。
4. 必要に応じて、`output/` 内の中間出力や `data_processed/` 内の整形済みデータを確認します。



## 主な結果

公開用リポジトリでまず見るべき成果物は、`docs/results/` に整理した以下のファイルです。

* `docs/results/aic_compare_all.csv`
* `docs/results/overlay_best_combined_2x2.png`
best_model_summary.csvから、ことがわかります。
### 標準化CPUE指数と raw CPUE の比較

以下の図は、4つのサイズ区分（`chu`, `dai`, `toku`, `tokudai`）について、選択されたGLMMによる標準化CPUEと 標準化前の相対CPUEを重ねて示したものです。

![Raw relative CPUE と標準化CPUEの比較](docs/results/overlay_best_combined_2x2.png)

図から、ことがわかります。

### モデル比較

`aic_compare_all.csv` は、各応答変数に対する候補モデルの比較結果をまとめた統合テーブルです。
Bestモデルは以下です。

### 解釈上の基本姿勢


## 重要な注意点

### 深度欠損の扱い

深度が欠損または利用不能なレコードは、可能な限り前処理段階ではNAとして保持されます。
ただし、両方の深度列が欠損している場合や、記録された両方の深度がクリーニングで用いた閾値ルールの対象外となる場合には、`depth_glmm` は `NA` とされます。
そのため、深度を含むモデル比較は、**利用可能な深度情報を持つ行**に基づいて行われます。

### speed / duration の置換ルール

`R/02_make_clean_data_and_figures.R` において、以下の置換ルールを用いています。

* speed は、欠損、`<= 0.5`、または `> 5` の場合に `3` ノットへ置換
* duration は、欠損、`<= 10` 分、または `> 90` 分の場合に `60` 分へ置換

これらは解析入力を安定化するための実務的ルールであり、結果の解釈時にはこの前処理を前提として考える必要があります。

### モデル比較は同一サブセット上で実施

候補モデル間の AIC 比較は、各応答変数ごとに同一のデータ上で行っています。
これは、AIC の差が入力データの違いではなく、モデル構造の違いを反映するようにするためです。AICはそうやって判断する必要があるからです。


## 出力

ローカル環境で解析を実行すると、主に以下のディレクトリに中間および最終成果物が保存されます。

* `output/`
* `data_processed/`

これらはローカル実行ごとに生成されるファイルを含むため、バージョン管理から除外しています。

公開用リポジトリでは、まず以下のディレクトリを参照してください。

* `docs/results/`

特に公開用として主要な成果物は以下です。

* `docs/results/aic_compare_all.csv`
* `docs/results/overlay_best_combined_2x2.png`