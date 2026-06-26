# Academic Performance Analysis

Statistical analysis in R on a real student performance dataset,
exploring factors that influence academic outcomes.

## Project Structure
- `eda_academic_performance_aplicatia1.R` — Exploratory data analysis,
  simple/multiple/polynomial regression, Ridge, LASSO and Elastic Net
  with cross-validation
- `panel_aplicatia2_worldbank.R` — Panel data econometrics on World Bank
  indicators for 10 European countries (2010-2020): pooled OLS, fixed
  effects, random effects, Hausman test

## Techniques Used
- Exploratory Data Analysis (EDA)
- OLS Regression with VIF, HC1 robust standard errors
- Ridge / LASSO / Elastic Net via cv.glmnet
- Panel data: Fixed Effects, Random Effects, Hausman test
- Breusch-Pagan test, F-test

## Requirements
R 4.x with the following packages:
tidyverse, psych, corrplot, lmtest, sandwich, glmnet, plm

## How to Run
1. Clone the repository
2. Open the `.R` files in RStudio
3. Install required packages:
   `install.packages(c("tidyverse", "psych", "corrplot", "lmtest", "sandwich", "glmnet", "plm"))`
4. Run the scripts

## Tech Stack
R · tidyverse · glmnet · plm · lmtest · sandwich
