% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phreeqc_input.R
\name{phr_input}
\alias{phr_input}
\alias{new_phr_input}
\alias{as_phr_input}
\alias{as_phr_input.default}
\alias{as_phr_input.phr_input}
\alias{as_phr_input.phr_input_section}
\alias{as.character.phr_input}
\alias{print.phr_input}
\title{Create a PHREEQC input}
\usage{
phr_input(...)

new_phr_input(x)

as_phr_input(x, ...)

\method{as_phr_input}{default}(x, ...)

\method{as_phr_input}{phr_input}(x, ...)

\method{as_phr_input}{phr_input_section}(x, ...)

\method{as.character}{phr_input}(x, ...)

\method{print}{phr_input}(x, ...)
}
\arguments{
\item{...}{A list of phr_input or sections.}

\item{x}{a phr_input object}
}
\description{
Create a PHREEQC input
}
