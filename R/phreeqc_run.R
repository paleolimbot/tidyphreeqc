
#' Run PHREEQC
#'
#' @param ... Input components
#' @param db A database name, NA to use the default, NULL to use current phreeqc package db.
#' @param dump Optional flag to include dump information. Defaults to NA, which detects whether
#'   a DUMP block is included in the input.
#'
#' @return A list with components
#' @export
#'
#' @examples
#' phr_run(phreeqc::ex2)
#'
phr_run <- function(..., db = NA, dump = NA) {
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

  # use dump strings if dump is TRUE, NA means autodetect a dump block
  if(identical(dump, NA)) {
    dump <- any(grepl("DUMP", input))
  }

  if(dump) {
    phreeqc::phrSetDumpStringsOn(TRUE)
    # if there is no DUMP block, add one
    input_list <- new_phr_input(
      c(
        input_list,
        phr_input(phr_input_section("DUMP", components = list("-file" = "thing.dmp", "-all" = "")))
      )
    )
    input <- as.character(input_list)
  } else {
    phreeqc::phrSetDumpStringsOn(FALSE)
  }

  # call phreeqc
  phreeqc::phrRunString(input)

  # return output as classed list
  structure(
    list(
      input = input,
      output_filename = out_filename,
      selected_output = lapply(.Call("getSelOutLst", PACKAGE = "phreeqc"), tibble::as_tibble),
      dump = if(dump) {
        phreeqc::phrGetDumpStrings()
      }
    ),
    class = "phr_run_output"
  )
}

#' Show output associated with a PHREEQC run
#'
#' @param x The result of \link{phr_run}
#' @param ... Passed to/from other methods.
#'
#' @export
#'
#' @examples
#' phr_solution() %>% phr_run() %>% phr_print_output()
#' phr_solution() %>% phr_run(dump = TRUE) %>% phr_print_dump()
#' phr_solution() %>% phr_run(phr_selected_output(activities = "H+")) %>% as_tibble()
#'
phr_print_output <- function(x) {
  cat(paste(readLines(x$output_filename), collapse = "\n"))
}

#' @rdname phr_print_output
#' @export
phr_print_dump <- function(x) {
  cat(paste(x$dump, collapse = "\n"))
}

#' @rdname phr_print_output
#' @export
print.phr_run_output <- function(x, ...) {
  cat("<phr_run_output>\n")
  cat("PHREEQC run with", length(x$selected_output), "selected output(s)\n")
  cat(sprintf("Raw output at '%s'", x$output_filename))
  cat("\n")
  if(!is.null(x$dump)) {
    cat("Dump available from phr_print_dump()\n")
  }
  if(length(x$selected_output) > 0) {
    cat("as_tibble():\n")
    print(tibble::as_tibble(x))
  }
}

#' @rdname phr_print_output
#' @export
as.data.frame.phr_run_output <- function(x, ...) {
  as.data.frame(unclass(tibble::as_tibble(x, ...)))
}

#' @rdname phr_print_output
#' @export
#' @importFrom tibble as_tibble
as_tibble.phr_run_output <- function(x, ...) {
  dplyr::bind_rows(x$selected_output, .id = "selected_output")
}
