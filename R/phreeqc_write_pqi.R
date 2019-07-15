#'Write phr_input_sections to file
#'
#'@description write your phr_input generated in tidyphreeqc to
#'.pqi files readily interpretable by the widely used PHREEQC-Interactive GUI
#'provided by the USGS to share your models with colleagues.
#'@param x An object of class "phr_input". See \link{phr_input} for details
#'@param path Path to file.
#'
#'@details The path supplied to the function might typically look something
#'like this: "~/path/to/my_program.pqi" but note that the ".pqi" ending is
#'strictly optional and any (or no) file-ending is accepted. However, in order
#'to be automatically recognised by the PHREEQC-Interactive GUI as a
#'PHREEQC-input file, the ".pqi" ending is necessary.
#'
#'@examples
#'# create some phr_input_sections
#'sol <- phr_solution(pH = 8, pe = 2, Na = 1, Cl = 1, units = "mol/l")
#'phase <- phr_equilibrium_phases(Halite = c(10, 1))
#'
#'fil <- tempfile("data")
#'phr_write_pqi(phr_input(sol, phase), path = fil)
#'if(interactive()) file.show(paste0(fil))
#'unlink(fil) # tidy up
#'@export

phr_write_pqi <- function(x, path){

  # Test if user supplied input is valid
  if (class(x) != "phr_input") {
    stop("x must be an object of class phr_input. Type ?phr_input for details!")
  }

  if (all(purrr::map_lgl(x, is_phr_input_section)) == FALSE) {
    # This may be a bit redundant now, given that phr_input(...) converts its
    # arguments automatically, but better safe than sorry I guess.
    stop("x must be an object of class phr_input. Type ?phr_input for details!")
  }

  # Convert phr_input_section#s for printing
  output <- purrr::map(x, paste0)

  # Because the call to "write" must come with append = TRUE, no old files
  # of the same name are permitted
  if (file.exists(path)){
    print("File given as path already exists and will be overwritten")
    unlink(path)
  }

  # The actual work is done here
  purrr::walk(output,
              write,
              file = path,
              ncolumns = 1,
              append = TRUE,
              sep = "\n")
}
