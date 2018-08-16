
#' Sepcify a solution
#'
#' @param .number Number of the component
#' @param .name Name of the component
#' @param ... Further arguments
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-41.htm#50528257_30253}
#'
#' @return A \link{phr_input_section}
#' @export
#'
#' @examples
#' phr_solution() # pure water
#' phr_solution(pH = 5.5, pe = 0)
#'
phr_solution <- function(.number = 1, .name = "", ...) {
  phr_input_section("SOLUTION", number = .number, name = .name, components = list(...))
}

#' Specify selected output
#'
#' @param .number Number of the component
#' @param ... Further arguments
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-38.htm#50528257_20239}
#'
#' @return A \link{phr_input_section}
#' @export
#'
#' @examples
#' phr_selected_output(pH = TRUE, pe = TRUE, alkalinity = TRUE, activities = c("H+", "OH-"))
#'
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
      stop("value of NULL is ambiguous in call to phr_selected_output")
    } else {
      val
    }
  })

  # remove NULL values
  components <- components[!vapply(components, is.null, logical(1))]

  # call phr_input_section
  phr_input_section("SELECTED_OUTPUT", number = .number, name = "", components = components)
}

#' Create a large number of solutions
#'
#' @param ... Arguments to \link{phr_solution}, all unique combinations of which will be used.
#' @param pH the pH of the solution
#' @param pe the pe of the solution
#' @param temp the temperature of the solution (degrees C)
#'
#' @return A \link{phr_input}
#' @export
#'
#' @examples
#' phr_solution_list(pH = c(5:7), pe = c(0:10), Hg = 0.1)
#'
phr_solution_list <- function(pH = 7, pe = 4, temp = 25, ...) {
  list_in <- list(pH = pH, pe = pe, temp = temp, ...)
  df <- purrr::cross_df(
    list_in,
    .filter = function(pH, pe, temp, ...) {
      # this filters out solutions that will not converge, causing the entire simulation
      # to fail
      H2_intercept <- 0
      logK_O2 <- -113.374 * 1000 / ((temp + 273.15) * 1.9872 * log(10))
      O2_intercept <- (logK_O2 / -4) - 0
      !((pe > -pH + H2_intercept) & (pe < -pH + O2_intercept))
    }
  )
  df$.number <- seq_len(nrow(df))
  new_phr_input(purrr::pmap(df, phr_solution))
}

#' Specify a reaction temperature, pressure gradient
#'
#' @param .number The number of the gradient
#' @param low The low temperature, pressure (degrees C, atm)
#' @param high The high temperature, pressure (degrees C, atm)
#' @param steps The number of steps to use between low and high
#' @param values Explicit values to use (takes precedence over low, high, steps)
#'
#' @return A character vector summarising the input
#' @export
#'
#' @seealso
#' Temp: \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-35.htm#50528257_75016}
#' Pressure: \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-34.htm#50528257_65966}
#'
#' @examples
#' phr_reaction_temperature(low = 25, high = 75, steps = 51)
#' phr_reaction_temperature(values = c(12, 15, 25))
#'
#' # example 2 from the manual
#' phr_run(
#'   phr_solution(), # pure water
#'   phr_equilibrium_phases(Gypsum = c(0, 1), Anhydrite = c(0, 1)),
#'   phr_reaction_temperature(low = 25, high = 50, steps = 10),
#'   phr_selected_output(temperature = TRUE, si = c("Gypsum", "Anhydrite"))
#' )
#'
phr_reaction_temperature <- function(.number = 1, low = 0, high = 100, steps = 10, values = NULL) {
  if(is.null(values)) {
    phr_input_section(
      "REACTION_TEMPERATURE", number = .number,
      components = list(
        sprintf("%f %f in %d steps", low, high, steps)
      )
    )
  } else {
    phr_input_section(
      "REACTION_TEMPERATURE", number = .number,
      components = list(values)
    )
  }
}

#' @rdname phr_reaction_temperature
#' @export
phr_reaction_pressure <- function(.number = 1, low = 0.5, high = 1.5, steps = 10, values = NULL) {
  if(is.null(values)) {
    phr_input_section(
      "REACTION_PRESSURE", number = .number,
      components = list(
        sprintf("%f %f in %d steps", low, high, steps)
      )
    )
  } else {
    phr_input_section(
      "REACTION_PRESSURE", number = .number,
      components = list(values)
    )
  }
}

#' Specify a list of phases for which equilibria should be established
#'
#' @param .number The number of the component
#' @param .name The name of the component
#' @param ... key/value pairs in the form Component = c(low, high), where low and high
#'   correspond to saturation indicies.
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-13.htm#50528257_61207}
#'
#' @return A \link{phr_input_section}
#' @export
#'
#' @examples
#' # example 2 from the manual
#' phr_run(
#'   phr_solution(), # pure water
#'   phr_equilibrium_phases(Gypsum = c(0, 1), Anhydrite = c(0, 1)),
#'   phr_reaction_temperature(low = 25, high = 50, steps = 10),
#'   phr_selected_output(temperature = TRUE, si = c("Gypsum", "Anhydrite"))
#' )
#'
phr_equilibrium_phases <- function(.number = 1, .name = NA, ...) {
  phr_input_section("EQUILIBRIUM_PHASES", number = .number, name = .name, components = list(...))
}
