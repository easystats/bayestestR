# Print tables in different output formats

Prints tables (i.e. data frame) in different output formats.

## Usage

``` r
# S3 method for class 'describe_posterior'
display(object, format = "markdown", ...)

# S3 method for class 'describe_posterior'
print(x, digits = 2, caption = "Summary of Posterior Distribution", ...)

# S3 method for class 'describe_posterior'
print_html(x, digits = 2, caption = "Summary of Posterior Distribution", ...)

# S3 method for class 'describe_posterior'
print_md(x, digits = 2, caption = "Summary of Posterior Distribution", ...)
```

## Arguments

- object, x:

  An object returned by one of the package's function, for example
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md),
  [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md),
  or [`eti()`](https://easystats.github.io/bayestestR/reference/eti.md).

- format:

  String, indicating the output format. Can be `"markdown"` `"html"`, or
  `"tt"`. `format = "tt"` creates a `tinytable` object, which is either
  printed as markdown or HTML table, depending on the environment. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

- ...:

  Arguments passed down to
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  or
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  (e.g., `digits`), or to
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

- digits:

  Integer, number of digits to round the table output. Defaults to 2.

- caption:

  Character, caption for the table. If `NULL`, no caption is added. By
  default, a caption is created based on the object type.

## Value

If `format = "markdown"`, the return value will be a character vector in
markdown-table format. If `format = "html"`, an object of class
`gt_tbl`. If `format = "tt"`, an object of class `tinytable`.

## Details

[`display()`](https://easystats.github.io/insight/reference/display.html)
is useful when the table-output from functions, which is usually printed
as formatted text-table to console, should be formatted for pretty
table-rendering in markdown documents, or if knitted from rmarkdown to
PDF or Word files. See
[vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
for examples.

## Examples

``` r
# \donttest{
d <- data.frame(replicate(4, rnorm(20)))
result <- describe_posterior(d)

# markdown format
display(result)
#> 
#> 
#> Table: Summary of Posterior Distribution
#> 
#> |Parameter | Median|       95% CI |    pd |         ROPE | % in ROPE|
#> |:---------|------:|:-------------|:------|:-------------|---------:|
#> |X1        |   0.45|[-1.85, 1.11] |55.00% |[-0.10, 0.10] |        0%|
#> |X2        |  -0.18|[-1.88, 2.20] |55.00% |[-0.10, 0.10] |    16.67%|
#> |X3        |  -0.37|[-1.45, 2.40] |65.00% |[-0.10, 0.10] |     5.56%|
#> |X4        |   0.38|[-1.33, 1.97] |75.00% |[-0.10, 0.10] |    16.67%|

# gt HTML
display(result, format = "html")


  


Summary of Posterior Distribution
```
