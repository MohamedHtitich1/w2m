# mwmm <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/MohamedHtitich1/mwmm)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Measuring What Matters Most

> *"Measuring what matters most for sustainable development"*

**mwmm** is an R package for sustainable development measurement built around the **temporal entropy weighting** method. The core `modified_entropy()` function identifies which indicators "matter most" by measuring how their information diversity (discriminating power) evolves over time.

### The Key Insight

Traditional composite indices assign fixed weights to indicators. But what if the importance of different dimensions changes over time? The modified entropy method reveals:

- **Which indicators are becoming MORE important** (increasing discriminating power)
- **Which indicators show convergence** (decreasing importance as countries become similar)
- **How priorities shift** across different development dimensions

## Installation

```r
# Install from GitHub
devtools::install_github("MohamedHtitich1/mwmm")

# Load the package
library(mwmm)

# See all available functions
mwmm_info()
```

## Quick Start

### 1. Calculate Temporal Entropy Weights

```r
library(mwmm)
library(dplyr)

# Prepare panel data (countries × years × indicators)
# Indicators should be normalized to comparable scales

result <- modified_entropy(
  data = indicators_df[, c("health", "education", "environment", "governance")],
  time = indicators_df$year,
  alpha = 0.95  # Temporal decay parameter
)

# View results
print(result)

# See which indicators matter most (and their trends)
summary(result)

# Visualize the evolution of importance
plot(result)
```

### 2. Interpret the Results

```r
# Access the weights matrix (indicators × years)
result$weights

# Get tidy format for further analysis
result$weights_long

# Summary with trend interpretation
result$summary
#>        indicator mean_weight   trend          interpretation
#> 1    environment       0.312  0.0045    Increasing importance
#> 2     governance       0.285 -0.0023  Decreasing (convergence)
#> 3      education       0.215  0.0001       Stable importance
#> 4         health       0.188 -0.0018  Decreasing (convergence)
```

### 3. Create Faceted Visualization (Paper-Style)

```r
# Define dimension groupings
dimensions <- list(
  "State Capacity" = c("fiscal_cap", "coord_cap", "deliv_cap"),
  "Democratic Accountability" = c("instit_acc", "elect_acc", "soc_acc"),
  "Basic Human Needs" = c("nutrition", "water", "shelter", "safety"),
  "Foundations of Wellbeing" = c("knowledge", "health", "environment")
)

# Create publication-ready faceted plot
plot_entropy_faceted(result, dimensions, ncol = 2)
```

## The Modified Entropy Method

### Mathematical Foundation

The entropy of an indicator measures how much it discriminates between units:

- **Low entropy**: All countries have similar values → Low discriminating power
- **High entropy**: Values are spread out → High discriminating power

The modified entropy method extends this by:

1. **Computing entropy weights for each time period**
2. **Applying temporal decay** (recent periods weighted more via `alpha` parameter)
3. **Tracking how weights evolve** to identify shifting priorities

### The Alpha Parameter

```r
# alpha = 1.0: All years weighted equally
result_equal <- modified_entropy(data, time, alpha = 1.0)

# alpha = 0.95: Recent years weighted more (recommended)
result_decay <- modified_entropy(data, time, alpha = 0.95)

# alpha ≈ 1 (e.g., exp(-0.0001)): Minimal decay
result_minimal <- modified_entropy(data, time, alpha = exp(-0.0001))
```

## Full Function Reference

### Core Functions

| Function | Description |
|----------|-------------|
| `modified_entropy()` | **Core function** - Temporal entropy weights |
| `entropy_weights()` | Static entropy weights (single time point) |
| `calculate_composite()` | Apply weights to create composite index |

### Composite Indices

| Function | Description |
|----------|-------------|
| `calculate_cesp()` | Carbon Efficiency of Social Progress |
| `calculate_jts()` | Just Transition Score (CESP + Material Footprint) |
| `calculate_mpi()` | Mazziotta-Pareto Index (penalizes imbalance) |

### Statistical Analysis

| Function | Description |
|----------|-------------|
| `phillips_sul()` | Club convergence analysis |
| `extract_clubs()` | Extract club membership from convergence results |
| `fit_segmented()` | Segmented (piecewise) regression |
| `segment_params()` | Extract segment intercepts and slopes |

### Machine Learning

| Function | Description |
|----------|-------------|
| `rf_importance()` | Random Forest variable importance |
| `fit_ctree()` | Conditional inference trees |

### Visualization

| Function | Description |
|----------|-------------|
| `plot.mwmm_entropy()` | Plot entropy weight evolution |
| `plot_entropy_faceted()` | Faceted plot by dimension (paper-style) |
| `world_choropleth()` | World map with Mollweide projection |
| `trajectory_plot()` | Time series trajectories |
| `scatter_labeled()` | Scatter plot with smart labels |
| `theme_mwmm()` | Publication-ready ggplot theme |

### Utilities

| Function | Description |
|----------|-------------|
| `rescale_0_100()` | Min-max normalization to 0-100 |
| `rescale_spi()` | SPI-style normalization with reference points |
| `geometric_mean()` | Geometric mean |
| `gam_impute()` | GAM-based panel data imputation |
| `winsorize()` | Cap extreme values |

## Example: Full Workflow

```r
library(mwmm)
library(dplyr)
library(tidyr)

# 1. Prepare data (example with simulated data)
set.seed(42)
n_countries <- 100
years <- 1990:2020

panel <- expand.grid(
  country = paste0("Country_", 1:n_countries),
  year = years
) %>%
  mutate(
    # Indicators with different temporal patterns
    health = rnorm(n(), 70, 10) + (year - 1990) * 0.3,
    education = rnorm(n(), 60, 15),
    environment = rnorm(n(), 50, 20) - (year - 1990) * 0.1,  # Converging
    governance = rnorm(n(), 55, 12) + (year - 1990) * 0.5   # Growing importance
  )

# 2. Normalize indicators
panel_norm <- panel %>%
  group_by(year) %>%
  mutate(across(health:governance, ~ rescale_0_100(.))) %>%
  ungroup()

# 3. Calculate modified entropy weights
entropy_result <- modified_entropy(
  data = panel_norm %>% select(health:governance),
  time = panel_norm$year,
  alpha = 0.95
)

# 4. Examine results
print(entropy_result)
summary(entropy_result)

# 5. Visualize
plot(entropy_result)

# 6. Apply weights to create composite index
weights_2020 <- entropy_result$weights[, "2020"]

panel_2020 <- panel_norm %>%
  filter(year == 2020) %>%
  mutate(
    composite = calculate_composite(
      data = select(., health:governance),
      weights = weights_2020
    )
  )

# 7. Map the results
world_choropleth(
  panel_2020,
  value_col = "composite",
  code_col = "country",
  title = "Composite Index (2020)"
)
```

## Research Applications

This toolkit has been used in:

1. **"The Evolving Importance of Social and Governance Measures Over Time"** - The original paper developing the modified entropy method

2. **Carbon Efficiency of Social Progress (CESP)** - *Geography and Sustainability* (under revision)

3. **Climate Perceptions Index** - *Environmental and Sustainability Indicators*

4. **Just Transition Score** - *Measurement*

## Citation

```bibtex
@software{mwmm,
  author = {Htitich, Mohamed},
  title = {mwmm: Measuring What Matters Most - Temporal Entropy Weighting for Sustainable Development},
  year = {2025},
  url = {https://github.com/MohamedHtitich1/mwmm}
}

@article{htitich2025evolving,
  author = {Htitich, Mohamed},
  title = {The Evolving Importance of Social and Governance Measures Over Time},
  journal = {Working Paper},
  year = {2025}
}
```

## Dependencies

**Core (installed automatically):**
- dplyr, tidyr, purrr, rlang, stringr
- ggplot2, viridis, ggrepel

**Suggested (install as needed):**
- randomForest, partykit (machine learning)
- segmented (piecewise regression)
- ConvergenceClubs (Phillips-Sul analysis)
- mgcv (GAM imputation)
- maps, sf (spatial visualization)

## Author

**Mohamed Htitich, PhD**

- Email: mohamed.htitich@gmail.com
- GitHub: [@MohamedHtitich1](https://github.com/MohamedHtitich1)
- Research: Social Progress Imperative (2021-2025)

## License

MIT © Mohamed Htitich
