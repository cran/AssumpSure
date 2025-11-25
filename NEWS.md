# AssumpSure 1.1.4

## Bugs fixes
- Fixed an error in the ANOVA normality check where Shapiro–Wilk failed on groups with zero variance.
- Added a user-facing message explaining that normality cannot be tested when a group has identical values.

## New features
- Added concise inline messages beneath continuous-variable dropdowns in the t-test/ANOVA and LM/LMM sections to clarify that only continuous variables are shown.


# AssumpSure 1.1.3

## Bugs fixes
- Fixed detection of numeric-coded grouping variables for random effects (e.g., participant IDs coded as 1, 2, 3) in LMM input.
- Added a new helper function `safe_run()` to handle computation interruptions gracefully in Shiny, preventing errors when users stop a process manually.



# AssumpSure 1.1.2

## Bug fixes
- Corrected the name of the first category in Chi-square and Fisher tests.
- Corrected error messages for Mann–Whitney U test when fewer than 2 groups or more than 2 groups are detected.
- Corrected error message for one-way ANOVA when fewer than 3 groups are detected.
- Added error messages for Kruskal–Wallis test when only 1 or 2 groups are detected.

## New features
- Added model metrics matrix for LM, LMM, GLM, and zero-inflated models showing key fit statistics (AIC, BIC, logLik, etc.).



# AssumpSure 1.1.1

## Bug fixes
- Fixed detection of numeric-coded categorical variables (e.g., groups coded as 1, 2, 3). 
- Fixed incorrect detection of numeric-coded categorical variables in Poisson, negative binomial, and zero-inflated negative binomial models.
- Improved handling of categorical variables to accept up to 10 levels. 
- Corrected error notifications to more clearly distinguish between numeric value columns and categorical group columns.
- Corrected the interpretation of the Shapiro–Wilk test to make it clearer.

## New features and user guidance
- Added a clear message explaining that the "categorical" are limited to 10 levels to ensure ANOVA and Kruskal–Wallis results remain reliable and interpretable.
- Added a clear message explaining that the "categorical" are limited to 15 unique levels to improve validity and interpretability of Fisher’s exact and Chi-square tests.
- Added asterisks to the boxplots and a tooltip explaining their interpretation.


# AssumpSure 1.1.0

## Bug fixes
- Fixed incorrect statistical test recommendations in some cases.
- Logical columns are now detected correctly.
- Fixed a bug where continuous variables were sometimes not recognized, which blocked parametric and nonparametric tests.
- Prevented Chi-square tests when any categorical variable has only one level, with a clear user message.
- Correlation heatmap now accepts the prevalence filter input 
- Corrected handling of covariates with perfect separation or near-zero variance, with informative messages and robust plotting.
- Fixed intermittent failures of the bicor correlation method.

## New features and user guidance
- Added a clear message explaining that the "continuous" type does not accept count data. Only truly continuous variables are allowed.
- Added guidance for normality testing with small sample sizes.
- Reformatted test outputs to be easier to copy into spreadsheets.
- Added tooltips on how to interpret QQ plots and histograms.
- Improved test selection: if variances are unequal but normality holds, recommend Welch’s tests instead of Mann–Whitney or Kruskal–Wallis or One-Way ANOVA, depending on design.
- Added effect size estimates for t-tests, Mann-Whitney, Wilcoxon rank sum, one-way ANOVA, and Kruskal Wallis. 
- Added guidance when bicor analysis fails.
- Added a tooltip explaining interpretation of correlation coefficients.
- Added a Shapiro–Wilk test below the histogram for checking the normality of the dependent variable, to help users with interpretation.
- Added tooltips for interpreting regression model assumptions.
- Added Poisson regression with a dedicated overdispersion test and guidance.
- Added a zero-inflation test for negative binomial regression and guidance.
- Added zero-inflated negative binomial regression.
