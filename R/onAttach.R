### welcome message
#' @importFrom utils packageDescription
.onAttach <- function(lib, pkg) {
  meta <- utils::packageDescription("christmas")
  attachmsg <- paste0("\nThis is christmas ",
                      meta$Version,
                      ". For details, use:\n",
                      "> help(package = 'christmas')\n")
  packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
}
