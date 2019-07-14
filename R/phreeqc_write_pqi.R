#'Write phr_input_sections to file
#'
#'@description write your phr_input sections generated in tidyphreeqc to
#'.pqi files readily interpretable by the widely used PHREEQC-Interactive GUI
#'provided by the USGS to share your models with colleagues.
#'@param ... One or more phr_input_sections
#'@param .file Path to file
#'@param .addEND Logical. Shall a PHREEQC "END" command be appended add the end of the function call? This can be usefull if you are planning to write several models into a single file.
#'@param .overwrite Logical. If .file already exists, shall it be overwritten (the default) or sould new input be appended to the file?
#'
#'@examples
#'# create some phr_input_sections
#'sol <- phr_solution(pH = 8, pe = 2, Na = 1, Cl = 1, units = "mol/l")
#'phase <- phr_equilibrium_phases(Halite = c(10, 1))
#'
#'fil <- tempfile("data.pqi")
#'phr_write_pqi(sol, phase, .file = fil, .addEND = TRUE)
#'if(interactive()) file.show(paste0(fil,".pqi"))
#'unlink(fil) # tidy up
#'
#'@export
#'


phr_write_pqi <- function(..., .file, .addEND = FALSE, .overwrite = TRUE){

  userInput <- list(...)

  # Test if user supplied input is valid
  if (all(purrr::map_lgl(userInput, is_phr_input_section)) == FALSE) {
    stop("Objects supplied to (...) must be of class phr_input_section")
  }

  # Handle file endings
  if(stringr::str_detect(.file, ".pqi$")) {
    userPath = .file
  } else {
    userPath = paste0(.file, ".pqi")
  }

  # With the overwrite option there are 4 different possible scenarios:
  # 1) file does exist and shall be overwritten --> Warning A)
  # 2) file does exist and shall not be overwritten --> Warning B)
  # 3) file does not exist and shall be overwritten --> no problem, no warning
  # 4) file does not exist and shall not be overwritten --> well... you couldn't do it anyway so no problem
  if(.overwrite) {
    if (file.exists(userPath)){
      unlink(userPath)
      # Warning A)
      warning(paste0("WARNING: file\n", userPath, "\ndid already exist.\n",
                     "It has been overwritten."))
    }
  } else {
    if (file.exists(userPath)){
      # Warning B)
      warning(paste0("WARNING: file\n", userPath, "\ndoes already exist.\n",
                     "New information will be appended to file."))
    }
  }

  # Convert phr_input_section#s for printing
  output <- purrr::map(userInput, paste0)

  # The actual work is done here
  purrr::walk(output,
              write,
              file = userPath,
              ncolumns = 1,
              append = TRUE,
              sep = "\n")

  # Adding an "END" to allow for the storage of different PHREEQC Programs
  # in a single .pqi file
  if (.addEND){
    write("\nEND\n", file = userPath, append = TRUE, sep = "\n")
  }
}


