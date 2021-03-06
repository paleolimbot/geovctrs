% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geovctr.R, R/sf-compat.R
\name{is_geovctr}
\alias{is_geovctr}
\alias{as_geovctr}
\alias{as_geovctr.character}
\alias{as_geovctr.data.frame}
\alias{restore_geovctr}
\alias{restore_geovctr.default}
\alias{restore_geovctr.data.frame}
\alias{expect_geovctr}
\alias{as_geovctr.sfc}
\alias{as_geovctr.sf}
\alias{restore_geovctr.sfc}
\alias{restore_geovctr.sf}
\title{Test for geovctrs}
\usage{
is_geovctr(x)

as_geovctr(x, ...)

\method{as_geovctr}{character}(x, ...)

\method{as_geovctr}{data.frame}(x, ...)

restore_geovctr(x, result, ...)

\method{restore_geovctr}{default}(x, result, ...)

\method{restore_geovctr}{data.frame}(x, result, ...)

expect_geovctr(x)

\method{as_geovctr}{sfc}(x, ...)

\method{as_geovctr}{sf}(x, ...)

\method{restore_geovctr}{sfc}(x, result, ...)

\method{restore_geovctr}{sf}(x, result, ...)
}
\arguments{
\item{x}{A (possibly) geovctr}

\item{...}{Passed to the constructor}

\item{result}{The result of a transformation operation}
}
\description{
A geovctr is a geometry-like collection of objects. In the geovctrs
package, \code{\link[=wkt]{wkt()}}, \code{\link[=wkb]{wkb()}}, \code{\link[=geo_collection]{geo_collection()}},
\code{\link[=geo_xy]{geo_xy()}}, \code{\link[=geo_rect]{geo_rect()}}, and \code{\link[=geo_segment]{geo_segment()}} are all geovctrs.
Extension packages can either use these types or
implement the \code{\link[=as_geovctr]{as_geovctr()}} generic
to take advantage of a wide range of processing functions, including
coercion, plotting, and summary information.
}
\details{
This package is intended to allow for a variety of in-memory
representations of geometry, as there are many examples where
simple geometries can be efficiently parameterized without
resorting to storing every coordinate of every vertex
(built-in examples include \code{\link[=geo_xy]{geo_xy()}}, \code{\link[=geo_segment]{geo_segment()}}, and
\code{\link[=geo_rect]{geo_rect()}}). These types do, however, have unambiguous
representations as geometries, and thus should be able to be
used wherever a geometry is appropriate.

For an object to be a "geovctr", it must:
\itemize{
\item Be a vctr (\code{\link[vctrs:vec_assert]{vctrs::vec_is()}} must be \code{TRUE}). This ensures that it will
work with \code{\link[tibble:tibble]{tibble::tibble()}} and other tidyverse functions such as
\code{\link[tidyr:nest]{tidyr::unnest()}} and \code{\link[dplyr:group_by]{dplyr::group_by()}} / \code{\link[dplyr:mutate]{dplyr::mutate()}} /
\code{\link[dplyr:summarise]{dplyr::summarise()}}.
\item Have \code{is_geovctr()} return \code{TRUE}. This makes it work automatically with
functions like \code{\link[=geo_bbox]{geo_bbox()}} that have a default
implementation for something that can be coerced to \code{\link[=wkt]{wkt()}},
\code{\link[=wkb]{wkb()}}, or \code{\link[=wksxp]{wksxp()}}.
\item Have the ability to be coerced to \code{\link[=wkt]{wkt()}}, \code{\link[=wkb]{wkb()}}, and \code{\link[=wksxp]{wksxp()}}
using \code{\link[=as_wkt]{as_wkt()}}, \code{\link[=as_wkb]{as_wkb()}}, and \code{\link[=as_wksxp]{as_wksxp()}}.
These casts power the default implementations of
functions like \code{\link[=geo_bbox]{geo_bbox()}}. In addition, a \code{\link[vctrs:vec_cast]{vctrs::vec_cast()}}
method should be provided so that row-binding and other vector
operations work with functions that might return a simpler type.
\item Have the ability to be combined with \code{\link[=wkt]{wkt()}}, \code{\link[=wkb]{wkb()}}, and \code{\link[=wksxp]{wksxp()}}
using \code{\link[=vec_c]{vec_c()}} in both directions. This helps support processing functions
that return a class to be combined with the output of other functions.
This might require a \code{\link[vctrs:vec_ptype]{vctrs::vec_ptype()}} implementation for
a class.
}

You can test these expectations for a given object using \code{\link[=expect_geovctr]{expect_geovctr()}}.

A secondary class of object is one that \emph{could}  be interpreted as a geovctr,
but in most cases can't be. One example of this is a character vector,
which \emph{could} be well-known text, but probably isn't. However, when the
user passes it to a function like \code{\link[=geo_bbox]{geo_bbox()}}, it
probably \emph{is} well-known text. Similarly, a \code{data.frame} or
\code{\link[tibble:tibble]{tibble::tibble()}} probably doesn't contain a geometry column,
but when passed to a function that operates on geometries,
it's likely that it does. The geovctrs package supports these
objects with the \code{\link[=as_geovctr]{as_geovctr()}} generic, which means you can
pass these objects anywhere you would pass a first-class
geometry vector.
}
\examples{
is_geovctr(wkt())
is_geovctr(NULL)

as_geovctr(wkt("POINT (30 10)"))
as_geovctr("POINT (30 10)")
as_geovctr(tibble::tibble(geometry = wkt("POINT (30 10)")))

}
