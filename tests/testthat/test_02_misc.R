## Misc Testing ##

# Remove everything
rm(list = ls())

## Context
testthat::context("Miscellaneous")

# Libraries
library("distreg.vis")

### -- Mult_trans --- ###

# Transform predictions of multinomial data
# multinomial_p1 <- multinomial_p %>%
#   sample_n(1)
#
# m_one <- distreg.vis:::mult_trans(multinomial_p1, multinomial_model)
# m_two <- distreg.vis:::mult_trans(multinomial_p, multinomial_model)
# expect_equal(class(m_one), "data.frame")
# expect_equal(class(m_two), "data.frame")

### -- Shiny & Javascript/CSS --- ###
cssfile <-
  system.file("srcjs/solarized-dark.css", package = "distreg.vis")
jsfile <-
  system.file("srcjs/highlight.pack.js", package = "distreg.vis")
expect_true(file.exists(cssfile))
expect_true(file.exists(jsfile))

### -- fac_check -- ###
DF <-
  structure(
    list(
      race = structure(
        c(1L, 1L, 1L, 1L, 1L),
        .Label = c("1. White",
                   "2. Black", "3. Asian", "4. Other"),
        class = c("ordered", "factor")
      ),
      year = c(2006L, 2006L, 2006L, 2006L, 2006L),
      education = structure(
        1:5,
        .Label = c(
          "1. < HS Grad",
          "2. HS Grad",
          "3. Some College",
          "4. College Grad",
          "5. Advanced Degree"
        ),
        class = c("ordered", "factor")
      ),
      health = structure(
        c(1L,
          1L, 1L, 1L, 1L),
        .Label = c("1. <=Good", "2. >=Very Good"),
        class = c("ordered",
                  "factor")
      ),
      age = c(42L, 42L, 42L, 42L, 42L),
      intercept = c(TRUE,
                    TRUE, TRUE, TRUE, TRUE)
    ),
    .Names = c("race", "year", "education",
               "health", "age", "intercept"),
    row.names = c("P1", "P2", "P3",
                  "P4", "P5"),
    class = "data.frame"
)
DF <- distreg.vis:::fac_check(DF)
expect_false("ordered" %in% unlist(sapply(DF, class)))
