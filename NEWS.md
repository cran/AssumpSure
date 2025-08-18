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
