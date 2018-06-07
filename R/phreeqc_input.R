
#' Create a PHREEQC input
#'
#' @param x a phr_input object
#' @param ... A list of phr_input or sections.
#'
#' @export
#'
phr_input <- function(...) {
  in_list <- list(...)
  input_list <- lapply(in_list, as_phr_input)
  new_phr_input(do.call(c, unclass(input_list)))
}

#' @rdname phr_input
#' @export
new_phr_input <- function(x) {
  structure(x, class = "phr_input")
}

#' @rdname phr_input
#' @export
as_phr_input <- function(x, ...) {
  UseMethod("as_phr_input")
}

#' @rdname phr_input
#' @export
as_phr_input.default <- function(x, ...) {
  new_phr_input(list(as_phr_input_section(x, ...)))
}

#' @rdname phr_input
#' @export
as_phr_input.phr_input <- function(x, ...) {
  x
}

#' @rdname phr_input
#' @export
as_phr_input.phr_input_section <- function(x, ...) {
  new_phr_input(list(x))
}

#' @rdname phr_input
#' @export
as.character.phr_input <- function(x, ...) {
  do.call(c, lapply(x, as.character))
}

#' @rdname phr_input
#' @export
print.phr_input <- function(x, ...) {
  cat("<phr_input>\n")
  cat(paste(as.character(x), collapse = "\n"))
  invisible(x)
}


#' Create input sections for phreeqc
#'
#' @param type The keyword name to use (e.g. SOLUTION)
#' @param number,.number Number of the component
#' @param name,.name Name of the component
#' @param components,... Key/value pairs corresponding to lines of input
#' @param x An object created by phr_input_section
#'
#' @return A character vector with an element for each line
#' @export
#'
#' @examples
#' # pure water solution
#' phr_solution(pH=7, temp=25)
#'
phr_input_section <- function(type, number = NA, name = "", components = list()) {
  # check type input
  if(!is.character(type) || (length(type) != 1)) {
    stop("'type' must be a character vector of length 1")
  }

  # check name input
  name <- as.character(name)
  if(!is.character(name) || (length(name) != 1)) {
    stop("'name' must be a character vector of length 1")
  }

  # check number input
  number <- as.integer(number)
  if(!is.integer(number) || (length(number) != 1)) {
    stop("'number' must be a numeric vector of length 1")
  }

  # number or name as "NA" means don't include
  if(is.na(number)) {
    number <- ""
  }

  if(is.na(name)) {
    name <- ""
  }

  # check for non-atomics
  lapply(components, function(val) {
    if(!is.atomic(val)) stop("Only atomic vectors are allowed in phr_input_section")
  })

  # make sure names(components) is not null
  if(is.null(names(components))) {
    names(components) <- rep("", length(components))
  }

  new_phr_input_section(
    list(
      type = type,
      number = number,
      name = name,
      components = components
    )
  )
}

#' @rdname phr_input_section
#' @export
new_phr_input_section <- function(x) {
  structure(x, class = c(paste0("phr_input_section_", x$type), "phr_input_section"))
}

#' @rdname phr_input_section
#' @export
phr_solution <- function(.number = 1, .name = "", ...) {
  phr_input_section("SOLUTION", number = .number, name = .name, components = list(...))
}

#' @rdname phr_input_section
#' @export
phr_selected_output <- function(.number = 1, ...) {
  # names for SELECTED_OUTPUT are preceeded by a "-"
  components = list(...)
  if(length(components) > 0) {
    names(components) <- paste0("-", names(components))
  }

  # values of "TRUE" should be just the name, values of "FALSE" should be omitted
  # values of NA should be "", NULL doesn't make sense
  components <- lapply(components, function(val) {
    if(identical(val, TRUE)) {
      "true"
    } else if(identical(val, FALSE)) {
      "false"
    } else if(identical(val, NA)) {
      ""
    } else if(is.null(val)) {
      stop("value of NULL is ambiguous in call to selected_output")
    } else {
      val
    }
  })

  # remove NULL values
  components <- components[!vapply(components, is.null, logical(1))]

  # call phr_input_section
  phr_input_section("SELECTED_OUTPUT", number = .number, name = "", components = components)
}

#' @rdname phr_input_section
#' @export
phr_equilibrium_phases <- function(.number = 1, .name = NA, ...) {
  phr_input_section("EQUILIBRIUM_PHASES", number = .number, name = .name, components = list(...))
}

#' Specify a reaction temperature gradient
#'
#' @param .number The number of the gradient
#' @param low The low temperature (degrees C)
#' @param high The high temperature (degrees C)
#' @param steps The number of steps to use between low and high
#'
#' @return A character vector summarising the input
#' @export
#'
#' @examples
#' phr_reaction_temperature(low = 25, high = 75, steps = 51)
#'
#' # example 2 from the manual
#' phr_run(phr_solution(), # pure water
#'         phr_equilibrium_phases(Gypsum = c(0, 1), Anhydrite = c(0, 1)),
#'         phr_reaction_temperature(low = 25, high = 50, steps = 10),
#'         phr_selected_output(temperature = TRUE, si = c("Gypsum", "Anhydrite")))
#'
phr_reaction_temperature <- function(.number = 1, low = 0, high = 100, steps = 100) {
  phr_input_section(
    "REACTION_TEMPERATURE", number = .number,
    components = list(
      sprintf("%f %f in %d steps", low, high, steps)
    )
  )
}

#' @rdname phr_input_section
#' @export
as.character.phr_input_section <- function(x, ...) {
  # create header line, removing "" strings
  header <- c(x$type, x$number, x$name)
  header <- header[nchar(header) > 0]

  # paste args together
  lines <- c(
    paste(header, collapse = " "),
    paste0("    ", vapply(seq_along(x$components), function(i) {
      vals <- c(names(x$components)[i], phr_escape_values(x$components[[i]]))
      # remove zero-length strings
      vals <- vals[nchar(vals) > 0]
      # paste values together
      paste(vals, collapse = "    ")
    }, character(1)))
  )

  lines
}

#' @rdname phr_input_section
#' @export
as.character.phr_input_section_character <- function(x, ...) {
  unclass(x)
}

#' @rdname phr_input_section
#' @export
print.phr_input_section <- function(x, ...) {
  cat("<phr_input_section>\n")
  cat(paste(as.character(x), collapse = "\n"))
  invisible(x)
}

#' @rdname phr_input_section
#' @export
is_phr_input_section <- function(x) {
  inherits(x, "phr_input_section")
}

#' @rdname phr_input_section
#' @export
as_phr_input_section <- function(x, ...) {
  UseMethod("as_phr_input_section")
}

#' @rdname phr_input_section
#' @export
as_phr_input_section.phr_input_section <- function(x, ...) {
  x
}

#' @rdname phr_input_section
#' @export
as_phr_input_section.character <- function(x, ...) {
  structure(x, class = c("phr_input_section_character", "phr_input_section"))
}

# phreeqc needs quotes backslash escaped
phr_escape_values <- function(vals) {
  gsub('"', '\\"', as.character(vals), fixed = TRUE)
}

#' Create a large number of solutions
#'
#' @param ... Arguments to \link{phr_solution}, all unique combinations of which will be used.
#' @param pH the pH of the solution
#' @param pe the pe of the solution
#'
#' @return A \link{phr_input}
#' @export
#'
#' @examples
#' phr_solution_list(pH = c(5:7), pe = c(0:10), Hg = 0.1)
#'
phr_solution_list <- function(pH = 7, pe = 0, ...) {
  list_in <- list(pH = pH, pe = pe, ...)
  df <- purrr::cross_df(
    list_in,
    .filter = function(pH, pe, ...) !((pe > -pH) & (pe < -pH + 20.775))
  )
  df$.number <- seq_len(nrow(df))
  new_phr_input(purrr::pmap(df, phr_solution))
}
