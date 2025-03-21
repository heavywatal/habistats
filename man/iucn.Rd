% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iucn.R
\name{ls_shapes}
\alias{ls_shapes}
\alias{read_shapes}
\title{Read and process habitat data from IUCN}
\usage{
ls_shapes(directory, recurse = TRUE)

read_shapes(files)
}
\arguments{
\item{directory}{Directory containing shape files.}

\item{recurse}{Logical, whether to search subdirectories.}

\item{files}{Vector of file paths to shape files.}
}
\description{
\code{\link[=ls_shapes]{ls_shapes()}} lists shape files in a directory.

\code{\link[=read_shapes]{read_shapes()}} reads shape files and combines them into a single data frame.
Note that "geometry" column in \code{sf} data.frame cannot be removed and affects performance.
Use \code{as.data.frame()} for efficient data.frame operations.
}
\seealso{
\url{https://www.iucnredlist.org/resources/spatial-data-download}
}
