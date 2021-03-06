% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phreeqc_run.R
\name{phr_print_output}
\alias{phr_print_output}
\alias{phr_print_dump}
\alias{print.phr_run_output}
\alias{as.data.frame.phr_run_output}
\alias{as_tibble.phr_run_output}
\title{Show output associated with a PHREEQC run}
\usage{
phr_print_output(x)

phr_print_dump(x)

\method{print}{phr_run_output}(x, ...)

\method{as.data.frame}{phr_run_output}(x, ...)

\method{as_tibble}{phr_run_output}(x, ...)
}
\arguments{
\item{x}{The result of \link{phr_run}}

\item{...}{Passed to/from other methods.}
}
\description{
Show output associated with a PHREEQC run
}
\examples{
phr_solution() \%>\% phr_run() \%>\% phr_print_output()
phr_solution() \%>\% phr_run(dump = TRUE) \%>\% phr_print_dump()
phr_solution() \%>\% phr_run(phr_selected_output(activities = "H+")) \%>\% as_tibble()

}
