
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
#'
#' @return NULL, invisibly
#' @export
#'
#' @examples
#' use_db_phreeqc()
#' # equivalent to
#' use_db(phreeqc::phreeqc.dat)
#'
#' use_db_amm()
#' use_db_ex15()
#' use_db_minteq()
#' use_db_minteq.v4()
#' use_db_pitzer()
#' use_db_sit()
#' use_db_wateq4f()
#'
use_db <- function(db_string, save = TRUE) {
  phreeqc::phrLoadDatabaseString(db_string)
  if(save) {
    db_state$current_db <- db_string
  }
  invisible(NULL)
}

# keep track of the current db being used
db_state <- new.env(parent = emptyenv())
db_state$current_db <- phreeqc::phreeqc.dat

#' @rdname use_db
#' @export
use_db_amm <- function(save = TRUE) {
  use_db(phreeqc::Amm.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_ex15 <- function(save = TRUE) {
  use_db(phreeqc::ex15.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_iso <- function(save = TRUE) {
  use_db(phreeqc::iso.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_llnl <- function(save = TRUE) {
  use_db(phreeqc::llnl.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_minteq <- function(save = TRUE) {
  use_db(phreeqc::minteq.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_minteq.v4 <- function(save = TRUE) {
  use_db(phreeqc::minteq.v4.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_pitzer <- function(save = TRUE) {
  use_db(phreeqc::pitzer.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_sit <- function(save = TRUE) {
  use_db(phreeqc::sit.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_wateq4f <- function(save = TRUE) {
  use_db(phreeqc::wateq4f.dat, save = save)
}

#' @rdname use_db
#' @export
use_db_phreeqc <- function(save = TRUE) {
  use_db(phreeqc::phreeqc.dat, save = save)
}
