.krvote2_env <- new.env(parent = emptyenv())

#' Set the service key for the krvote2 package
#' @export
set_krvote2_key <- function(key) {
 .krvote2_env$serviceKey <- key
}

get_krvote2_key <- function() {
 key <- .krvote2_env$serviceKey
 if (is.null(key)) {
  stop("No service key set. Please use set_krvote2_key() to set your service key.")
 }
 key
}
