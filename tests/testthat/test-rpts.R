testthat::test_that("Indicator reports do not throw errors", {
  for(i in NEesp::species_key$Species){
    test <- NEesp::render_ind_report(i)
    expect_false(class(test) == "try-error")
  }
  })

testthat::test_that("Regression reports do not throw errors", {
    test <- NEesp::render_all_reg()
    expect_false(class(test) == "try-error")
})

