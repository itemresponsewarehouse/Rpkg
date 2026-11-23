% Fixture for inst/developer/run-examples.R (Rpkg#214), not a package page.
% The gate as roxygen writes `@examplesIf` into \dontshow: the closed-gate
% pass must accept it.
\name{gated-examplesif}
\alias{gated-examplesif}
\title{Fixture: a warehouse example gated by @examplesIf}
\description{Calls \code{irw_fetch()} behind the \code{@examplesIf} gate.}
\examples{
\dontshow{if (irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))) withAutoprint(\{ # examplesIf}
\donttest{
irw_fetch("environment_ltm")
}
\dontshow{\}) # examplesIf}
}
