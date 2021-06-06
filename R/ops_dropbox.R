#' Upload a file to dropbox
#'
#' You will need to setup an app before using this function. Set scopes
#' appropriately for your use case, then generate a token (in that order).
#'
#'
#' @param path The location of the file to upload
#' @param dropbox_path The location to upload the file to in the app or user's
#' dropbox folder. Do not prepend with "/".
#' For example: a_folder/on/dropbox/file.ext. The complete filename is required
#' including the file extension.
#' @param token The dropbox app token
#' @param with_response Should the response from the Dropbox API be returned?
#' If TRUE, returns an R list converted from the JSON API response from the
#' dropbox API.
#' @importFrom jsonlite toJSON
#' @examples
#' \dontrun{
#'   ops_dropbox_upload(
#'     path = "/full/path/to/file.ext",
#'     dropbox_path = "test/file.ext",
#'     token = token,
#'     with_response = FALSE
#'   )
#' }
#' @export
ops_dropbox_upload <- function(path, dropbox_path, token, with_response = FALSE) {
  curl_request <- sprintf(
    paste0(
      "curl -s -X POST https://content.dropboxapi.com/2/files/upload",
      " -H 'Authorization: Bearer %s'",
      " -H 'Dropbox-API-Arg:",
      jsonlite::toJSON(
        list(
          'path' = paste0('/', dropbox_path),
          'mode' = 'overwrite'),
        auto_unbox = TRUE),
      paste0(
        "' -H 'Content-Type: application/octet-stream' --data-binary '@%s'")),
    token, path
  )


  r <- ops_tc(
    system(paste0(curl_request), intern = TRUE),
    stop(paste0("The file: ", path, " failed to upload to dropbox.",
                " There was a problem with the curl request"))
  )

  if ("error_summary" %in% names(r)) {

    stop(paste0("The file: ", path, " failed to upload to dropbox.",
                " There was a problem with the Dropbox API request. We ",
                "recommend you run the query with_response=T for more details")
    )
  }

  if (with_response) {
    return(jsonlite::fromJSON(r))
  }


}

