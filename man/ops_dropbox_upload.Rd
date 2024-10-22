% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ops_dropbox.R
\name{ops_dropbox_upload}
\alias{ops_dropbox_upload}
\title{Upload a file to dropbox}
\usage{
ops_dropbox_upload(path, dropbox_path, token, with_response = FALSE)
}
\arguments{
\item{path}{The location of the file to upload}

\item{dropbox_path}{The location to upload the file to in the app or user's
dropbox folder. Do not prepend with "/".
For example: a_folder/on/dropbox/file.ext. The complete filename is required
including the file extension.}

\item{token}{The dropbox app token}

\item{with_response}{Should the response from the Dropbox API be returned?
If TRUE, returns an R list converted from the JSON API response from the
dropbox API.}
}
\description{
You will need to setup an app before using this function. Set scopes
appropriately for your use case, then generate a token (in that order).
}
\examples{
\dontrun{
  ops_dropbox_upload(
    path = "/full/path/to/file.ext",
    dropbox_path = "test/file.ext",
    token = token,
    with_response = FALSE
  )
}
}
