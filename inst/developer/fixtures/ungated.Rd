% Fixture for inst/developer/run-examples.R (Rpkg#214), not a package page.
% An example that calls the warehouse with no gate at all: the closed-gate
% pass must fail on it and name this file. It mentions irw_fetch() in prose
% and \usage as well, which must not matter either way.
\name{ungated}
\alias{ungated}
\title{Fixture: an un-gated warehouse example}
\usage{irw_fetch("environment_ltm")}
\description{Calls \code{irw_fetch()} with no gate.}
\examples{
\donttest{
irw_fetch("environment_ltm")
}
}
