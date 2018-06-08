
#' Use various phreeqc databases
#'
#' For details on the actual databases, see \link[phreeqc]{Amm.dat},
#' \link[phreeqc]{ex15.dat}, \link[phreeqc]{iso.dat},
#' \link[phreeqc]{llnl.dat}, \link[phreeqc]{minteq.dat},
#' \link[phreeqc]{minteq.v4.dat}, \link[phreeqc]{pitzer.dat},
#' and \link[phreeqc]{sit.dat}. The default (loaded on namespace load)
#' is phreeqc.
#'
#' @param db_string A character vector version of the database.
#' @param save Pass TRUE to save the database as the default
#' @param name The (optional) name of the database being used.
#'
#' @return db_string, invisibly
#' @export
#'
#' @examples
#' phr_use_db_phreeqc()
#' # equivalent to
#' phr_use_db(phreeqc::phreeqc.dat)
#'
#' phr_use_db_amm()
#' phr_use_db_ex15()
#' phr_use_db_minteq()
#' phr_use_db_minteq.v4()
#' phr_use_db_pitzer()
#' phr_use_db_sit()
#' phr_use_db_wateq4f()
#'
phr_use_db <- function(db_string, save = TRUE, name = NA) {
  phreeqc::phrLoadDatabaseString(db_string)
  if(save) {
    db_state$.current_db <- db_string
    db_state$.current_db_name <- as.character(name)[1]
  }
  invisible(db_string)
}

#' @rdname phr_use_db
#' @export
phr_get_current_db <- function() {
  db <- db_state$.current_db
  attr(db, "db_name") <- db_state$.current_db_name
  db
}

# keep track of the current db being used, set default db to be used
db_state <- new.env(parent = emptyenv())
phr_use_db(phreeqc::phreeqc.dat, save = TRUE, name = "phreeqc")

#' @rdname phr_use_db
#' @export
phr_use_db_amm <- function(save = TRUE) {
  phr_use_db(phreeqc::Amm.dat, save = save, name = "Amm")
}

#' @rdname phr_use_db
#' @export
phr_use_db_ex15 <- function(save = TRUE) {
  phr_use_db(phreeqc::ex15.dat, save = save, name = "ex15")
}

#' @rdname phr_use_db
#' @export
phr_use_db_iso <- function(save = TRUE) {
  phr_use_db(phreeqc::iso.dat, save = save, name = "iso")
}

#' @rdname phr_use_db
#' @export
phr_use_db_llnl <- function(save = TRUE) {
  phr_use_db(phreeqc::llnl.dat, save = save, name = "llnl")
}

#' @rdname phr_use_db
#' @export
phr_use_db_minteq <- function(save = TRUE) {
  phr_use_db(phreeqc::minteq.dat, save = save, name = "minteq")
}

#' @rdname phr_use_db
#' @export
phr_use_db_minteq.v4 <- function(save = TRUE) {
  phr_use_db(phreeqc::minteq.v4.dat, save = save, name = "minteq.v4")
}

#' @rdname phr_use_db
#' @export
phr_use_db_pitzer <- function(save = TRUE) {
  phr_use_db(phreeqc::pitzer.dat, save = save, name = "pitzer")
}

#' @rdname phr_use_db
#' @export
phr_use_db_sit <- function(save = TRUE) {
  phr_use_db(phreeqc::sit.dat, save = save, name = "sit")
}

#' @rdname phr_use_db
#' @export
phr_use_db_wateq4f <- function(save = TRUE) {
  phr_use_db(phreeqc::wateq4f.dat, save = save, name = "wateqf")
}

#' @rdname phr_use_db
#' @export
phr_use_db_phreeqc <- function(save = TRUE) {
  phr_use_db(phreeqc::phreeqc.dat, save = save, name = "phreeqc")
}
