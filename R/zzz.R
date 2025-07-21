## R/zzz.R ---------------------------------------------------------------
.onAttach <- function(libname, pkgname) {          # DO NOT EXPORT
  ver <- utils::packageVersion(pkgname)

  packageStartupMessage(
    paste0(
      "########################################\n######## ndrutils ", ver, " ready. #########\n###### You're doing a great job! #######\n########################################\n"
    )
  )
}
