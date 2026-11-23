% Fixture for inst/developer/run-examples.R (Rpkg#214), not a package page.
% The inline gate around one snippet, as in man/irw_covariates.Rd: the
% closed-gate pass must accept it.
\name{gated-inline}
\alias{gated-inline}
\title{Fixture: a warehouse example gated inline}
\description{Runs offline code, then calls \code{irw_fetch()} behind an inline gate.}
\examples{
x <- 1 + 1
\donttest{
if (irw_has_credentials() || nzchar(Sys.getenv("IRW_RUN_EXAMPLES"))) {
  df <- irw_fetch("environment_ltm")
}
}
}
