context("use_db")

test_that("use_db functions actually set the database", {
  phr_use_db_phreeqc()
  current_db <- phr_get_current_db()
  expect_equal(attr(current_db, "db_name"), "phreeqc")
  expect_equal(as.character(current_db), phreeqc::phreeqc.dat)

  # check save=FALSE and TRUE
  phr_use_db_minteq(save = FALSE)
  expect_equal(as.character(phr_get_current_db()), phreeqc::phreeqc.dat)
  phr_use_db_minteq(save = TRUE)
  expect_equal(as.character(phr_get_current_db()), phreeqc::minteq.dat)

  # try setting all databases
  phr_use_db_amm()
  expect_equal(as.character(phr_get_current_db()), phreeqc::Amm.dat)
  phr_use_db_ex15()
  expect_equal(as.character(phr_get_current_db()), phreeqc::ex15.dat)
  phr_use_db_minteq()
  expect_equal(as.character(phr_get_current_db()), phreeqc::minteq.dat)
  phr_use_db_minteq.v4()
  expect_equal(as.character(phr_get_current_db()), phreeqc::minteq.v4.dat)
  phr_use_db_pitzer()
  expect_equal(as.character(phr_get_current_db()), phreeqc::pitzer.dat)
  phr_use_db_sit()
  expect_equal(as.character(phr_get_current_db()), phreeqc::sit.dat)
  phr_use_db_wateq4f()
  expect_equal(as.character(phr_get_current_db()), phreeqc::wateq4f.dat)

  # reset to phreeqc.dat
  phr_use_db_phreeqc(save = TRUE)
})

test_that("trying to set invalid database strings throws an error", {
  expect_error(phr_use_db(NULL), "ERROR:")
  expect_error(phr_use_db(""), "ERROR:")
  expect_error(phr_use_db("thingerthinger\nthinger"), "ERROR:")
})
