STAT 545B Assignment B-1
================
Sarah Masri
2023-10-29

## Load Packages

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rigr) )
suppressPackageStartupMessages(library(testthat))
```

## Exercise 1: Make a Function (25 points)

In this exercise, I will be making a function and fortifying it. The
function’s purpose is to summarize the count of specific groups in a
given dataset. One might be interested in the number of classes with in
a single field, as well as the number of classes at the intersection of
several fields.

## Exercise 2: Document your Function (20 points)

The following code chunk include the function proposed in Exercise 1, as
well as relevant roxygen2 tags.

``` r
#' @title Count Groups
#' @description This function provides the counts of entries belonging to classes in a variable or variables of a dataset.
#' @param df A dataframe with at least 1 column and 1 row.
#' @param groupby_cols A string or list of strings specifying the column name(s) that the function will use to group the data. Must have length at least 1. 
#' @return A tibble which summaries the count of entries within each group or combination of groups (if the data is grouped by more than 1 column). 
count_groups <- function(df, groupby_cols) {
  df_cols <- colnames(df)
  
  ## Check that df has at least one row and one column
  if(nrow(df) == 0 | ncol(df) == 0) {stop("Please use a non-empty dataframe.")}
  
  ## Check that groupby_cols is non empty
  if (length(groupby_cols) == 0) {stop("groupby_cols is empty. Please provide at least one column name.")}
  
  ## Check that groupby_cols is a single string or list of string
  if (class(groupby_cols) != "character") {stop("At least one variable in groupby_cols is not a string.")}
  
  ## Check that each string in groupby_cols are valid column names
  if(!all(groupby_cols %in% df_cols)) {stop("The following strings provided are not valid column names: ", toString(groupby_cols[which(!groupby_cols %in% df_cols)]))}

  count_tbl <- df %>% group_by_at(groupby_cols) %>%  summarize(n = n())
  count_tbl
}
```

## Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples.

We use the `fev` dataset in the `rigr` package. Before showing a few
examples of `count_groups`, a preview of the data is shown below.

``` r
head(fev)
```

    ##   seqnbr subjid age   fev height    sex smoke
    ## 1      1    301   9 1.708   57.0 female    no
    ## 2      2    451   8 1.724   67.5 female    no
    ## 3      3    501   7 1.720   54.5 female    no
    ## 4      4    642   9 1.558   53.0   male    no
    ## 5      5    901   9 1.895   57.0   male    no
    ## 6      6   1701   8 2.336   61.0 female    no

The following example summarises the number of smokers vs. non-smokers.
One may expect two rows (`smoke` and `n` for count) and two columns (for
smokers and non-smokers).

``` r
count_groups(fev, "smoke")
```

    ## # A tibble: 2 × 2
    ##   smoke     n
    ##   <chr> <int>
    ## 1 no      589
    ## 2 yes      65

The next example groups by two columns in the dataset: sex and smoking
status. Since each variable has two classes (female/male and no/yes
respectively), then one may expect $2^2 = 4$ rows and three rows (`sex`,
`smoke`, and `n` for count).

``` r
count_groups(fev, c("sex", "smoke"))
```

    ## `summarise()` has grouped output by 'sex'. You can override using the `.groups`
    ## argument.

    ## # A tibble: 4 × 3
    ## # Groups:   sex [2]
    ##   sex    smoke     n
    ##   <chr>  <chr> <int>
    ## 1 female no      279
    ## 2 female yes      39
    ## 3 male   no      310
    ## 4 male   yes      26

The last working example groups by three columns in the dataset: sex,
smoking status, and age. This example shows that as the number of
variables used to group the data increases, so does the number of rows.
This summary table is particularly large since the age variables ranges
from 3 to 19. Although it may not be particularly helpful for this
function to be used with nmany variables with a large number of classes
each, the function still works and maintains its general functionality.

``` r
count_groups(fev, c("sex", "smoke", "age"))
```

    ## `summarise()` has grouped output by 'sex', 'smoke'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 51 × 4
    ## # Groups:   sex, smoke [4]
    ##    sex    smoke   age     n
    ##    <chr>  <chr> <int> <int>
    ##  1 female no        3     1
    ##  2 female no        4     6
    ##  3 female no        5    14
    ##  4 female no        6    15
    ##  5 female no        7    29
    ##  6 female no        8    46
    ##  7 female no        9    44
    ##  8 female no       10    30
    ##  9 female no       11    39
    ## 10 female no       12    25
    ## # ℹ 41 more rows

The final examples are cases where an error is expected. This is due to
the fact that “smoker” and “age_group” are not valid column names in the
`fev` dataset.

``` r
count_groups(fev, c("sex", "smoker", "age"))
```

    ## Error in count_groups(fev, c("sex", "smoker", "age")): The following strings provided are not valid column names: smoker

``` r
count_groups(fev, c("sex", "smoker", "age_group"))
```

    ## Error in count_groups(fev, c("sex", "smoker", "age_group")): The following strings provided are not valid column names: smoker, age_group

## Exercise 4: Test the Function (25 points)

We write formal tests for the `count_groups` function with at least
three non-redundant uses of an `expect_()` function from the `testthat`
package, contained in a `test_that()` function (or more than one). They
should all pass.

#### Test 1:

Check that invalid (empty) dataframes will throw an error.

``` r
test_that("Invalid dataframe", expect_error(count_groups(data.frame(), "column1")))
```

    ## Test passed 🌈

#### Test 2:

groupby_names that are not valid column names **and** a string will
throw an error.

``` r
test_that("Invalid groupby_names", {
  expect_error(count_groups(fev, c()))                    ## empty list not valid
  expect_error(count_groups(fev, "column1"))              ## 'column1' is not a valid colname
  expect_error(count_groups(fev, c("sex", "smoking")))    ## 'smoking' is not a valid colname, but 'sex' is
  expect_error(count_groups(fev, c(1, 2)))                ## groupby_names are not strings
          })
```

    ## Test passed 🌈

#### Test 3:

Check expected dimensions of summary tables. Number of rows should be
the number of combinations of classes between variables provided. If the
number of variables in groupby_names is k, then the expected number of
columns is k+1.

``` r
test_that("Summary tables have correct dimensions", {
  expect_equal(dim(count_groups(fev, c("sex"))), c(2, 2))
  expect_equal(dim(count_groups(fev, c("sex", "smoke"))), c(4, 3))
  })
```

    ## Test passed 🌈

#### Test 4:

For a set of groupby_names, the information given by count_groups should
be equivalent regardless of the order in groupby_names.

``` r
test_that("Order of group-by variables does not matter", {
  expect_equal(sort(count_groups(fev, c("sex", "smoke"))$n), sort(count_groups(fev, c("smoke", "sex"))$n))
  expect_equal(sort(count_groups(fev, c("sex", "smoke", "age"))$n), sort(count_groups(fev, c("age", "smoke", "sex"))$n))
  })
```

    ## Test passed 🥳
