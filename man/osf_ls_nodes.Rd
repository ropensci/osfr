% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osf_ls_nodes.R
\name{osf_ls_nodes}
\alias{osf_ls_nodes}
\title{List projects or components}
\usage{
osf_ls_nodes(x, pattern = NULL, n_max = 10)
}
\arguments{
\item{x}{an \code{\link{osf_tbl_node}} or \code{\link{osf_tbl_user}}}

\item{pattern}{character string used to filter for results that contain the
substring \code{"pattern"} in their name. Note this is a fixed, case-insensitive
search.}

\item{n_max}{Number of results to return from OSF.}
}
\value{
an \code{\link{osf_tbl_node}}
}
\description{
List the projects or components associated with a user or in the top-level of
an OSF project or component.
}
\examples{
\dontrun{
user <- osf_retrieve_user("me")
osf_ls_nodes(user)
}
}