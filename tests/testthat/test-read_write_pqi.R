test_that("pqi_write accepts phr_input only", {

  fil <- tempfile("data")

  expect_error(phr_write_pqi("A", fil))
  expect_error(phr_write_pqi(1, fil))
  expect_error(phr_write_pqi(phr_solution(pH = 5.5), fil))

  phr_write_pqi(phr_input(phr_solution(pH = 5.5)), fil)
  expect_true(file.exists(fil))
  unlink(fil)
})

test_that("detect_keyword lives up to its name", {

  expect_true(phr_detect_keyword("MIX"))
  expect_true(phr_detect_keyword("SOLUTION"))
  expect_true(phr_detect_keyword("SIT"))
  expect_false(phr_detect_keyword("sit"))
  expect_false(phr_detect_keyword("HOFFNUNG"))
})

test_that("detect_keyword outputs a logical of input length", {

  expect_length(phr_detect_keyword("MIX"), 1)
  expect_length(phr_detect_keyword(c("SURFACE", "TRANSPORT")), 2)

  test_keywords <- c("EXCHANGE", "PHASES", "EQUILIBRIUM_PHASES")

  expect_length(phr_detect_keyword(test_keywords), 3)
  expect_length(phr_detect_keyword(test_keywords), length(test_keywords))
  expect_true(is.logical(phr_detect_keyword(test_keywords)))
})

test_that("read_pqi works as intended", {

  # Creating some .pqi to test on
  sol <- phr_solution(
    pH = 12,
    pe = 4,
    Na = 2,
    Cl = c(1, "charge"),
    units = "mmol/kgw"
  )

  pH_def <- phr_pH_fix_definition()

  eqp <- phases <- phr_equilibrium_phases(
    Calcite = c(0, 0.001),
    Fix_pH = c("-3", "HCl", "10")
  )

  sel <- phr_selected_output(
    pH = TRUE,
    totals = c("Na", "Cl"),
    equilibrium_phases = c("Calcite")
  )

  # Writing the test-input to file
  fil <- tempfile("data")
  phr_write_pqi(
    phr_input(
      sol,
      pH_def,
      eqp,
      sel
    ),
    path = fil
  )

  expect_s3_class(phr_read_pqi(fil), "phr_input")
  expect_length(phr_read_pqi(fil), length(phr_input(sol, pH_def, eqp, sel)))
  expect_error(phr_read_pqi(c("THIS", "IS", "NO", "VALID", "INPUT")))
  expect_error(phr_tidy_PHREEQC(
    c("PARTIALLY", "CORRECT", "INPUT", "IS", "NO", "SOLUTION")
  ))

  unlink(fil)
})
