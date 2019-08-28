
#' Write phr_input_sections to file
#'
#' Write a [phr_input()] generated in tidyphreeqc to
#' .pqi files readily interpretable by the widely used PHREEQC-Interactive GUI
#' provided by the USGS to share your models with colleagues.
#'
#' The path supplied to the function might typically look something
#' like this: "~/path/to/my_program.pqi" but note that the ".pqi" ending is
#' strictly optional and any (or no) file-ending is accepted. However, in order
#' to be automatically recognised by the PHREEQC-Interactive GUI as a
#' PHREEQC-input file, the ".pqi" ending is necessary.
#'
#' @param x An object of class "phr_input". See [phr_input()] for details
#' @param path Path to file.
#'
#' @return `x`, invisibly
#'
#' @examples
#' # create some phr_input_sections
#' sol <- phr_solution(pH = 8, pe = 2, Na = 1, Cl = 1, units = "mol/l")
#' phase <- phr_equilibrium_phases(Halite = c(10, 1))
#'
#' fil <- tempfile("data")
#' phr_write_pqi(phr_input(sol, phase), path = fil)
#' unlink(fil) # tidy up
#' @export
phr_write_pqi <- function(x, path){

  # Test if user supplied input is valid
  if (!inherits(x, "phr_input")) {
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
  purrr::walk(
    output,
    write,
    file = path,
    ncolumns = 1,
    append = TRUE,
    sep = "\n"
  )

  invisible(x)
}

#' Read .pqi-files into tidyphreeqc
#'
#' Load .pqi-files produced by the PHREEQC-interactive GUI into
#' the tidyphreeqc format [phr_input()]
#'
#' @param path Path to file.
#'
#' @return An object of class [phr_input()]
#' @examples
#' # create some phr_input_sections
#' sol <- phr_solution(pH = 8, pe = 2, Na = 1, Cl = 1, units = "mol/l")
#' phase <- phr_equilibrium_phases(Halite = c(10, 1))
#'
#' fil <- tempfile("data")
#' phr_write_pqi(phr_input(sol, phase), path = fil)
#' phr_read_pqi(path = fil)
#' unlink(fil) # tidy up
#'
#' @export
phr_read_pqi <- function(path){
  pqi_raw <- readLines(path)
  phr_tidy_PHREEQC(pqi_raw)
}

#' Detect keywords
#'
#' This function is called by [phr_tidy_PHREEQC()].
#' It is not meant to be exported or to be used anywhere else, yet.
#'
#' @param x A string, usually a line from a .pqi-file
#'
#' @noRd
#' @return A logical vector indicating whether or not the input was a keyword
#'
phr_detect_keyword <- function(x) {

  # check if line begins with four spaces (NOT a keyword)
  catcher <- vector("logical", length = length(x))
  for (i in seq_along(catcher)) {
    if (stringr::str_detect(x[i], "^    ")) {
      catcher[[i]] <- FALSE
    } else {
      # test against keywords dataset
      catcher[[i]] <- purrr::map_lgl(
        tidyphreeqc::keywords,
        grepl,
        x = x[i]
      ) %>%
        any()
    }
  }

  catcher
}

#' Translate PHREEQC to tidyphreeqc
#'
#' Take PHREEQC-code and fill it into [phr_input_section()]s.
#' This function is called by [phr_tidy_PHREEQC()].
#' It is not meant to be exported or to be used anywhere else, yet.
#'
#' @param x A list of strings that constitute a PHREEQC input block
#'
#' @noRd
#' @return A [phr_input_section()]
#'
phr_parse_PHREEQC <- function(x) {
  input_vector_list_raw <- strsplit(x, split = " ")

  input_vector_list_clean <- purrr::map(
    input_vector_list_raw,
    function(x) {
      x[!(x == "")]
    }
  )

  keyword <- input_vector_list_clean[[1]][1]

  # Database calls are handled fundamentally different in R
  # and are for now largely ignored
  if (keyword == "DATABASE") {

    db <- strsplit(x[1], split = " ")[1]
    do.call("cat", db)

  } else if (keyword == "END") {
    # Special case "END"
    phr_end()

  } else {
    headline <- input_vector_list_clean[[1]]

    # This deals with keywords not associated with index numbers (e.g. TRANSPORT)
    if (length(headline > 1)) {
      number <- headline[2] %>% as.integer()
    } else {
      number <- ""
    }

    phr_input_section(
      type = keyword, number = number,
      components = input_vector_list_clean[2:length(input_vector_list_clean)]
    )
  }
}

#' Deal with input from a .pqi-file
#'
#' This function is the workhorse behind [phr_read_pqi()].
#' It takes in the list of character vectors read from the .pqi file,
#' determines when an where that list is to be split along the different
#' phreeqc keywords and then shoves the pieces into
#' [phr_parse_PHREEQC()] to force meaning upon them.
#' It is not meant to be exported or to be used anywhere else, yet.
#'
#' @param x list of strings that constitute a .pqi-file
#'
#' @noRd
#' @return A [phr_input()]
#'
phr_tidy_PHREEQC <- function(x){

  # Find the keywords
  pqi <- tibble::enframe(x)
  pqi$is_key <- phr_detect_keyword(pqi$value)

  n_keywords <- pqi$is_key[pqi$is_key == TRUE] %>% length()

  if (!(n_keywords >= 1)) {
    stop("Could not identify any PHREEQC keywords.")
  }

  indices_keywords <- which(pqi$is_key)
  block_ends <- c(indices_keywords[2:n_keywords]-1, nrow(pqi))

  # split the raw input into pieces, one keyword at a time
  pqi_blocks <- vector("list", length = n_keywords)
  for (i in seq(1:n_keywords)){
    pqi_blocks[[i]] <- pqi$value[seq(indices_keywords[i], block_ends[i])]
  }

  # Translate the blocks of PHREEQC strings into phreeqc_input_sections
  sections <- purrr::map(
    pqi_blocks,
    phr_parse_PHREEQC
  )

  result <- do.call(phr_input, sections)

  result
}
