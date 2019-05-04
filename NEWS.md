# bayestestR 0.1.2

## Breaking changes

- `rnorm_perfect()` deprecated in favour of `distribution_normal()`
- `map_estimate()` now returns a single value instead of a dataframe and the `density` parameter has been removed. The MAP density value is now accessible via `attributes(map_output)$MAP_density`

## New functions / features

- `p_direction()`: new argument `raw = FALSE` to compute pd based on AUC
- `area_under_curve()`: compute AUC
- `rcauchy_perfect()`, `rpois_perfect()` and `rt_perfect()` functions have been added
- `bayesfactor()` function has been added
- Started adding plotting methods (currently in the [`see`](https://github.com/easystats/see) package) for `p_direction()` and `hdi()`

## Minor changes

- `p_direction()`: improved printing
- Improved testing
- `rope()` for model-objects now returns the HDI values for all parameters as attribute in a consistent way

## Bug fixes

- `hdi()` returned multiple class attributes (#72)
- `plot.equivalence_test()` did not work properly for *brms*-models (#76).

# bayestestR 0.1.0

- CRAN initial publication and [0.1.0 release](https://github.com/easystats/bayestestR/releases/tag/v0.1.0)
- Added a `NEWS.md` file to track changes to the package
