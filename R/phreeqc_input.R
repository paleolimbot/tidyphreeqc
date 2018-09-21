
#' Create a PHREEQC input
#'
#' @param x a phr_input object
#' @param ... A list of phr_input or sections.
#'
#' @export
#'
phr_input <- function(...) {
  in_list <- list(...)
  in_list <- in_list[!vapply(in_list, is.null, logical(1))]
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
#' @param number Number of the component
#' @param name Name of the component
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

  # remove NULL if these values are in components. This allows sightly easier
  # passing of default arguments in helper functions
  components <- components[!vapply(components, is.null, logical(1))]

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
  cat("\n")
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
