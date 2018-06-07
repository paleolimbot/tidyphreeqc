
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyphreeqc
===========

The goal of easyphreeqc is to provide a more useful interface to the existing phreeqc package.

Installation
------------

You can install easyphreeqc from github with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/tidyphreeqc")
```

Example
-------

Running PHREEQC is accomplished using the `phr_run()` function, which calls the program and generates the output. The function accepts character vectors of input, which can be generated using intput helper functions such as `phr_solution()`, `phr_selected_output()`, `phr_equilibrium_phases()`, and `phr_reaction_temperature()` (or roll your own input using `phr_input_section()`).

``` r
library(tidyphreeqc)
phr_run(
  phr_solution(pH = 7, temp = 25)
)
#> <phr_run_output>
#> PHREEQC run with 0 selected output(s)
#> as_tibble():
#> # A tibble: 0 x 0
```

To get the results as a data frame, we need to supply a `phr_selected_output()` to the input file.

``` r
phr_run(
  phr_solution(pH = 7, temp = 25),
  phr_selected_output(pH = TRUE, temp = TRUE, activities = c("OH-", "H+", "O2"))
)
#> <phr_run_output>
#> PHREEQC run with 1 selected output(s)
#> as_tibble():
#> # A tibble: 1 x 13
#>   selected_output   sim state   soln dist_x  time  step    pH    pe
#>   <chr>           <int> <chr>  <int>  <dbl> <dbl> <int> <dbl> <dbl>
#> 1 n1                  1 i_soln     1     NA    NA    NA  7.00  4.00
#> # ... with 4 more variables: `temp(C)` <dbl>, `la_OH-` <dbl>,
#> #   `la_H+` <dbl>, la_O2 <dbl>
```

To find the distribution of a few solutions, you can generate a list of solutions using `phr_solution_list()`.

``` r
phr_run(
  phr_solution_list(pH = 5:8, temp = 12:25),
  phr_selected_output(pH = TRUE, temp = TRUE, activities = c("OH-", "H+", "O2"))
)
#> <phr_run_output>
#> PHREEQC run with 1 selected output(s)
#> as_tibble():
#> # A tibble: 56 x 13
#>    selected_output   sim state   soln dist_x  time  step    pH    pe
#>    <chr>           <int> <chr>  <int>  <dbl> <dbl> <int> <dbl> <dbl>
#>  1 n1                  1 i_soln     1     NA    NA    NA  5.00     0
#>  2 n1                  1 i_soln     2     NA    NA    NA  6.00     0
#>  3 n1                  1 i_soln     3     NA    NA    NA  7.00     0
#>  4 n1                  1 i_soln     4     NA    NA    NA  8.00     0
#>  5 n1                  1 i_soln     5     NA    NA    NA  5.00     0
#>  6 n1                  1 i_soln     6     NA    NA    NA  6.00     0
#>  7 n1                  1 i_soln     7     NA    NA    NA  7.00     0
#>  8 n1                  1 i_soln     8     NA    NA    NA  8.00     0
#>  9 n1                  1 i_soln     9     NA    NA    NA  5.00     0
#> 10 n1                  1 i_soln    10     NA    NA    NA  6.00     0
#> # ... with 46 more rows, and 4 more variables: `temp(C)` <dbl>,
#> #   `la_OH-` <dbl>, `la_H+` <dbl>, la_O2 <dbl>
```

Databases
---------

Some elements (for example, mercury) aren't included in the base database. There are a number of databases included in the PHREEQC package, that you can choose by specifying the `db` argument of `phr_run()`. One that includes mercury is the "minteq" database.

``` r
phr_run(
  phr_solution(pH = 7, temp = 25, Hg = 0.1),
  phr_selected_output(
    activities = c("Hg", "Hg2+2", "Hg(OH)2", "Hg(OH)2", "HgOH+", "Hg(OH)3-")
  ),
  db = "minteq"
)
#> <phr_run_output>
#> PHREEQC run with 1 selected output(s)
#> as_tibble():
#> # A tibble: 1 x 14
#>   selected_output   sim state   soln dist_x  time  step    pH    pe la_Hg
#>   <chr>           <int> <chr>  <int>  <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#> 1 n1                  1 i_soln     1     NA    NA    NA  7.00  4.00 -4.00
#> # ... with 4 more variables: `la_Hg2+2` <dbl>, `la_Hg(OH)2` <dbl>,
#> #   `la_HgOH+` <dbl>, `la_Hg(OH)3-` <dbl>
```

Pourbaix diagrams
-----------------

``` r
result <- phr_run(
  phr_solution_list(pH = 0:14, pe = -14:22, Hg = 0.1),
  phr_selected_output(
    activities = c("Hg", "Hg2+2", "Hg(OH)2", "Hg(OH)2", "HgOH+", "Hg(OH)3-")
  ),
  db = "minteq"
)

result
#> <phr_run_output>
#> PHREEQC run with 1 selected output(s)
#> as_tibble():
#> # A tibble: 300 x 14
#>    selected_output   sim state   soln dist_x  time  step    pH    pe la_Hg
#>    <chr>           <int> <chr>  <int>  <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#>  1 n1                  1 i_soln     1     NA    NA    NA  14.0 -13.0 -3.92
#>  2 n1                  1 i_soln     2     NA    NA    NA  13.0 -12.0 -3.99
#>  3 n1                  1 i_soln     3     NA    NA    NA  14.0 -12.0 -3.92
#>  4 n1                  1 i_soln     4     NA    NA    NA  12.0 -11.0 -4.00
#>  5 n1                  1 i_soln     5     NA    NA    NA  13.0 -11.0 -3.99
#>  6 n1                  1 i_soln     6     NA    NA    NA  14.0 -11.0 -3.92
#>  7 n1                  1 i_soln     7     NA    NA    NA  11.0 -10.0 -4.00
#>  8 n1                  1 i_soln     8     NA    NA    NA  12.0 -10.0 -4.00
#>  9 n1                  1 i_soln     9     NA    NA    NA  13.0 -10.0 -3.99
#> 10 n1                  1 i_soln    10     NA    NA    NA  14.0 -10.0 -3.92
#> # ... with 290 more rows, and 4 more variables: `la_Hg2+2` <dbl>,
#> #   `la_Hg(OH)2` <dbl>, `la_HgOH+` <dbl>, `la_Hg(OH)3-` <dbl>
```

``` r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
#> ✔ tibble  1.4.2     ✔ dplyr   0.7.4
#> ✔ tidyr   0.7.2     ✔ stringr 1.3.0
#> ✔ readr   1.1.1     ✔ forcats 0.2.0
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
result_long <- result %>%
  as_tibble() %>%
  gather(key = "species", value = "log_activity", starts_with("la_")) %>%
  mutate(species = str_remove(species, "^la_"))

result_long %>%
  ggplot(aes(x = pH, y = pe, col = log_activity)) +
  geom_point() +
  facet_wrap(~species)
```

![](README-pourbaix-1.png)

``` r

result_long %>%
  filter(species != "Hg") %>%
  group_by(pH, pe) %>%
  summarise(
    dominant_species = species[which.max(log_activity)],
    dominant_log_act = max(log_activity),
    dominance = max(log_activity) - max(setdiff(log_activity, max(log_activity)))
  ) %>%
  ggplot(aes(pH, pe, col = dominant_species, alpha = dominance)) +
  geom_point()
```

![](README-pourbaix-2.png)
