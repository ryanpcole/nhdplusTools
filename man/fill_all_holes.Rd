% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_watershed_delineation_nhdplushr.R
\name{fill_all_holes}
\alias{fill_all_holes}
\title{fill_all_holes}
\usage{
fill_all_holes(x)
}
\arguments{
\item{x}{An sfc object with polygons that need their holes filled}
}
\value{
An sfc object with holes filled
}
\description{
Borrowed this from smoothr package - https://github.com/mstrimas/smoothr/blob/main/R/fill-holes.r and modified it to not use the threshold
}
