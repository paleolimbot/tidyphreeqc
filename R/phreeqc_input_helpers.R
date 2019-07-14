
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


#' Define a solution master species
#'
#' Most master species are defined in the databases. Some are not (e.g., Hg in the default database),
#' and require specification in the input. This is also a good way to specify components of
#' a solution that do not dissociate (e.g., Cyanide in the minteq database). Solution master
#' species must have an associated \link{phr_solution_species} identity reaction (e.g., Hg+2 = Hg+2).
#' The identity reaction must have a log_k of 0, but can specify other parameters such as gamma
#' (relationship between activity, concentration, and ionic strength).
#'
#' @param element_name An element name (e.g., As). Can also have a valence state (e.g., Hg(2)).
#'   If it does have a valence state, the original element must already be defined as a
#'   phr_solution_master_species. This is the name that will get used to specify concentrations in
#'   \link{phr_solution}. Can be any name that starts with a capital letter (e.g., "EDTA").
#' @param master_species The representative species of the element. This is the name that will end up in
#'   a \link{phr_solution_species} reaction (or any other reaction).
#' @param alkalinity The contribution of this species to Alkalinity.
#' @param gram_formula_weight Weight used for unit conversion from the master species.
#'   Usually this is the gram formula weight of the element (safest bet), not of the dominant aequeous species.
#'   The exception of this appears to be SO4 in the databases.
#'   Use NA to calculate automatically; use NULL to omit.
#' @param element_gram_formula_weight Weight used for unit conversion from the element.
#'   Use NA to calculate automatically; use NULL to omit.
#'
#' @return A \link{phr_input_section}.
#' @export
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/html/final-57.html}
#'
#' @examples
#' # define Hg as a species assuming it is Hg+2 in solution.
#' phr_input(
#'   phr_solution_master_species("Hg", "Hg+2"),
#'   phr_solution_species("Hg+2 = Hg+2"),
#'   phr_solution(Hg = 0.2, units = "mol/L")
#' ) %>%
#'   phr_run() %>%
#'   phr_print_output()
#'
#' # Species that are partially included already are harder to implement.
#' # To implement a concentration of "Perchlorate" (ClO4-), it's necessary to
#' # include a balanced reaction to convert the previously defined
#' # primary master species Cl- to ClO4. Realistically, this would be
#' # done using Cl2, which could be created using Na+ and Cl-. Basically,
#' # this is a bit of a rabbit hole. Log K values should be included with any
#' # non-identity reaction to ensure the result is somewhat realistic. You could
#' # use very low log_k values to suggest that maybe this reaction is unlikely.
#' phr_input(
#'   phr_solution_master_species("Cl(0)", "Cl2"),
#'   phr_solution_master_species("Cl(-1)", "Cl-"),
#'   phr_solution_master_species("Cl(7)", "ClO4-"),
#'   phr_solution_species("Cl2 = Cl2"),
#'   phr_solution_species("2Cl- + 2H2O = Cl2 + H2 + 2OH-", log_k = -20),
#'   phr_solution_species("ClO4- = ClO4-"),
#'   phr_solution_species("0.5Cl2 + 4H2O = ClO4- + 8H+ + 7e-", log_k = -20),
#'   phr_solution("Cl(7)" = 0.2, units = "mol/L")
#' ) %>%
#'   phr_run() %>%
#'   phr_print_output()
#'
#' # A better way to do this would be to implement Perchlorate as a
#' # primary master species, since this doesn't make any assumptions about
#' # how it might react with water.
#' phr_input(
#'   phr_solution_master_species(
#'     "Perchlorate", "Perchlorate-",
#'     element_gram_formula_weight = chemr::mass("ClO4-")
#'   ),
#'   phr_solution_species("Perchlorate- = Perchlorate-"),
#'   phr_solution("Perchlorate" = 0.2, units = "mol/L")
#' ) %>%
#'   phr_run() %>%
#'   phr_print_output()
#'
phr_solution_master_species <- function(element_name, master_species, alkalinity = 0,
                                        gram_formula_weight = element_gram_formula_weight,
                                        element_gram_formula_weight = NA) {

  if(identical(element_gram_formula_weight, NA)) {
    el_novalence <- stringr::str_remove(element_name, "\\(.*?\\)$")
    element_gram_formula_weight <- chemr::elmass(el_novalence)
    stopifnot(is.finite(element_gram_formula_weight))
  }
  if(identical(gram_formula_weight, NA)) {
    gram_formula_weight <- chemr::mass(master_species)
  }

  phr_input_section(
    "SOLUTION_MASTER_SPECIES",
    components = list(c(element_name, master_species, alkalinity, gram_formula_weight, element_gram_formula_weight))
  )
}

#' Define a surface master species
#'
#' Defines a surface and a binding site on that surface (separated by an underscore). An identity
#' reaction with a log_k of 0 must be included in a \link{phr_surface_species} block.
#'
#' @param binding_site_name A name for the binding site, starting with a capital letter and including
#'   only lowercase letters afterward (e.g., Mysurface). An underscore will denote an additional binding site on the
#'   surface (e.g., Mysurface_one).
#' @param master_species The formula for the binding site. This is generall the binding site name
#'   followed by OH (e.g., MysurfaceOH), in what the documentation calls the "OH-form of the binding site).
#'
#' @return A \link{phr_input_section}.
#' @export
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/html/final-61.html}
#'
#' @examples
#' phr_input(
#'   phr_surface_master_species("Mysurface", "MysurfaceOH"),
#'   phr_surface_species("MysurfaceOH = MysurfaceOH")
#' )
#'
phr_surface_master_species <- function(binding_site_name, master_species) {
  phr_input_section(
    "SURFACE_MASTER_SPECIES",
    components = list(c(binding_site_name, master_species))
  )
}

#' Define a solution/surface species
#'
#' @param reaction The formation reaction of the species, e.g.: \code{CO3-2 + H+ = HCO3-}.
#' @param log_k The equilibrium coefficient of the reaction at 25 degrees C. Alternatively can be specified
#'   for all temperatures using \code{analytical_expression}.
#' @param delta_h The enthalpy of reaction, in kJ/mol (or with defined units). Used to determine the
#'   temperature dependence of K according to the Van't-Hoff equation, if \code{analytical_expression}
#'   is not provided.
#' @param analytical_expression Identifier for coefficients for an analytical expression for the
#'   temperature dependence of log K. Must be a vector of six numeric values:
#'   \code{A1 + A2*T + A3/T + A4*log10(T) + A5 / T^2 + A6*T^2}.
#' @param ... Further arguments in the input block
#'
#' @return A \link{phr_input_section}
#' @export
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-43.htm}
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/html/final-62.html}
#'
#' @examples
#' # default units are kJ/mol for log_k
#' sp1 <- phr_solution_species("CO3-2 + H+ = HCO3-", log_k = 10.329, delta_h = -14.899)
#' # can also specify units
#' sp2 <- phr_solution_species("CO3-2 + H+ = HCO3-", log_k = 10.329, delta_h = "-3.561 kcal")
#' # log_k can also be specified through an analytical expression
#' sp3 <- phr_solution_species(
#'  "CO3-2 + H+ = HCO3-",
#'  analytical_expression = c(107.8871, 0.03252849, -5151.79, -38.92561, 563713.9, 0)
#' )
#'
#' list(kJ = sp1, kcal = sp2, analytic = sp3, default = NULL) %>%
#'   lapply(
#'     phr_run,
#'     phr_solution("C" = "1 as CO3-2", temp = 90, units = "mol/L"),
#'     phr_selected_output(activities = "HCO3-")
#'   )
#'
phr_solution_species <- function(reaction, log_k = NULL, delta_h = NULL, analytical_expression = NULL, ...) {
  phr_species(
    "SOLUTION_SPECIES",
    reaction = reaction,
    log_k = log_k,
    delta_h = delta_h,
    analytical_expression = analytical_expression,
    ...
  )
}

#' @rdname phr_solution_species
#' @export
phr_surface_species <- function(reaction, log_k = NULL, delta_h = NULL, analytical_expression = NULL, ...) {
  phr_species(
    "SURFACE_SPECIES",
    reaction = reaction,
    log_k = log_k,
    delta_h = delta_h,
    analytical_expression = analytical_expression,
    ...
  )
}

phr_species <- function(name, reaction, log_k = NULL, delta_h = NULL, analytical_expression = NULL, ...) {

  stopifnot(
    is.null(analytical_expression) ||
      ((is.numeric(analytical_expression)) && (length(analytical_expression) == 6))
  )

  dots <- list(...)
  if(length(dots) > 0) {
    # all named args in ...
    stopifnot(!is.null(names(dots)), all(names(dots) != ""))
    # log_k and delta_h reqire no prefixing, but dots do
    names(dots) <- paste0("-", names(dots))
  }

  phr_input_section(
    name,
    components = c(
      list(
        reaction,
        "-log_k" = log_k,
        "-delta_h" = delta_h,
        "-analytical_expression" = analytical_expression
      ),
      dots
    )
  )
}

#' Specify a surface
#'
#' Surface specification happens in three places: \link{phr_surface_master_species}, which
#' defines the binding site names, \link{phr_surface_species}, which defines the log_k values
#' for the binding of solution species to the site, and this function, which defines the number
#' and density of binding sites (defined in \link{phr_surface_master_species}) that exist in
#' the solution specified by the 'equilibrate' parameter. It is worth noting that "hydrous
#' ferric oxide" (Hfo) are included in the main (and several other) databases, including
#' log_k values for many metals with this surface. These are split into the strong (Hfo_s)
#' and weak (Hfo_w) sites.
#'
#' @param .number,.name Identifiers for this surface
#' @param ... One or more phr_surface_item() objects or key/value pairs added to the block. Surface items
#'   should be in the form binding_site_name = phr_surface_item(...) (where binding_site_name
#'   is defined in \link{phr_surface_master_species}).
#' @param equilibrate A \link{phr_solution} number with which this surface should equilibrate. Defaults to 1.
#' @param sites_units Either absolute (sites speciried in moles) or density (sites specified in sites/ng).
#' @param sites The number of sites in moles (if sites_units is "absolute"), or the density of sites in
#'   sites / square nanometer (if sites_units is "density").
#' @param specific_area_per_gram Area of the surface per gram, in square meters / gram
#' @param grams The number of grams, for specific area calculation
#' @param diffusion_coefficient The diffusion coefficient in square meters / second, which
#'   applies in a TRANSPORT calculation to make the surface transport in space (like a colloid
#'   rather than an immobile surface).
#'
#' @seealso
#' \url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/phreeqc3-html/phreeqc3-45.htm}
#'
#' @return A \link{phr_input_section}
#' @export
#'
#' @examples
#' # specify a hydrous ferric oxide surface and see if there is any sorption by Zn
#' # similar to example 8 in the documentation
#' phr_input(
#'   phr_solution(pH = 8, Zn = 0.1),
#'   phr_surface(
#'     Hfo_sOH = phr_surface_item(sites = 5e-6, specific_area_per_gram = 600, grams = 0.09),
#'     # no need to specify mass of this binding site, because it shares a surface
#'     # with Hfo
#'     Hfo_wOH = phr_surface_item(sites = 2e-4),
#'   ),
#'   phr_selected_output(molalities = c("Zn+2", "Hfo_sOZn+", "Hfo_wOZn+"))
#' ) %>%
#'   phr_run()
#'
phr_surface <- function(.number = 1, .name = NA, ..., equilibrate = 1, sites_units = "absolute") {

  dots <- list(..., equilibrate = equilibrate, sites_units = sites_units)

  # all named parameters
  stopifnot(
    length(dots) > 0,
    !is.null(names(dots)),
    all(names(dots) != "")
  )

  # items are members of dots that are subclassed by
  is_item <- vapply(dots, inherits, "phr_surface_item", FUN.VALUE = logical(1))
  items <- dots[is_item]
  params <- dots[!is_item]

  # phreeqc needs booleans as 'true' or 'false'
  params <- lapply(params, function(x) {
    if(is.logical(x)) {
      tolower(as.character(x))
    } else {
      x
    }
  })
  names(params) <- paste0("-", names(params))

  phr_input_section(
    "SURFACE",
    number = .number,
    name = .name,
    components = c(items, params)
  )
}

#' @rdname phr_surface
#' @export
phr_surface_item <- function(sites, specific_area_per_gram = NULL, grams = NULL,
                             diffusion_coefficient = NULL) {
  # grams must be specified if specific area per gram is specified
  if(is.null(specific_area_per_gram)) stopifnot(is.null(grams))

  structure(
    list(
      sites = sites,
      specific_area_per_gram = specific_area_per_gram,
      grams = grams,
      diffusion_coefficient = diffusion_coefficient
    ),
    class = "phr_surface_item"
  )
}

# this lets us pass the object to phr_input_section rather than have it be parsed first
#' @export
as.character.phr_surface_item <- function(x, ...) {
  if(is.null(x$diffusion_coefficient)) {
    as.character(c(x$sites, x$specific_area_per_gram, x$grams))
  } else {
    as.character(
      c(x$sites, x$specific_area_per_gram, x$grams, paste("-Dw", x$diffusion_coefficient))
    )
  }
}

#' Insert END block
#'
#' This function has no arguments. It creates an END block
#' that ends the data input for a simulation. After this keyword is read by
#' PHREEQC, the calculations described by the input for the
#' simulation are performed and the results printed.
#' Additional simulations may follow in the input data set,
#' each in turn will be terminated with an END keyword or the end of the file.
#'
#'@seealso
#'\url{https://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/html/final-37.html#pgfId-254414}
#'
#'@return A \link{phr_input_section}
#'@export
#'@examples
#'phr_end()
#'
#'
phr_end <- function() {
  phr_input_section("END")
}
