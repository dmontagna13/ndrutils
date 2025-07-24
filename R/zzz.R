## R/zzz.R ---------------------------------------------------------------
.onAttach <- function(libname, pkgname) {          # DO NOT EXPORT
  ver <- utils::packageVersion(pkgname)

  these.messages <- tibble(num = c(1, 2, 3, 4),
                           message = c("###### You're doing a great job! #######",
                                       "######### Today is a new day! ##########",
                                       "##### Look Mom, I'm in a computer! #####",
                                       "########## One step at a time ##########"))
  this.msg.num <- sample(these.messages$num, 1)

  this.message <- dplyr::filter(these.messages, num == this.msg.num) %>%
    pull(message)

  packageStartupMessage(
    paste0(
      "########################################\n######## ndrutils ", ver, " ready. #########\n", this.message, "\n########################################\n"
    )
  )
}
