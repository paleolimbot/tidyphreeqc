
#' Run PHREEQC
#'
#' @param ... Input components
#' @param db A database name, NA to use the default, NULL to use current phreeqc package db.
#' @param x Output from phr_run
#'
#' @return A list with components
#' @export
#'
#' @examples
#' phr_run(phreeqc::ex2)
#'
phr_run <- function(..., db = NA) {
  # concatenate the input, convert to atomic character using as.character()
  input_list <- phr_input(...)
  input <- as.character(input_list)

  # NA means use the current default, NULL means do nothing (useful for multiple calls)
  if(is.null(db)) {
    # do nothing
  } else if(identical(db, NA)) {
    # use current default as saved
    phr_use_db(phr_get_current_db(), save = FALSE)
  } else if(!is.null(db)) {
    # call use_db_ ...
    match.fun(paste0("phr_use_db_", db))(save = FALSE)
  }

  # capture string output as a file
  out_filename <- tempfile()
  phreeqc::phrSetOutputFileName(out_filename)
  phreeqc::phrSetOutputFileOn(TRUE)

  # call phreeqc
  phreeqc::phrRunString(input)

  # return output as classed list
  structure(
    list(
      input = input,
      output_filename = out_filename,
      selected_output = lapply(.Call("getSelOutLst", PACKAGE = "phreeqc"), tibble::as_tibble)
    ),
    class = "phr_run_output"
  )
}

#' @rdname phr_run
#' @export
as.data.frame.phr_run_output <- function(x, ...) {
  as.data.frame(unclass(tibble::as_tibble(x, ...)))
}

#' @rdname phr_run
#' @export
#' @importFrom tibble as_tibble
as_tibble.phr_run_output <- function(x, ...) {
  dplyr::bind_rows(x$selected_output, .id = "selected_output")
}

#' @rdname phr_run
#' @export
print.phr_run_output <- function(x, ...) {
  cat("<phr_run_output>\n")
  cat("PHREEQC run with", length(x$selected_output), "selected output(s)\n")
  cat("as_tibble():\n")
  print(tibble::as_tibble(x))
}
