## R/zzz.R ---------------------------------------------------------------
.onAttach <- function(libname, pkgname) {          # DO NOT EXPORT
  ver <- utils::packageVersion(pkgname)

  these.messages <- data.frame(num = c(1, 2, 3, 4),
                           message = c("###### You're doing a great job! #######",
                                       "######### Today is a new day! ##########",
                                       "##### Look Mom, I'm in a computer! #####",
                                       "########## One step at a time ##########"))
  this.msg.num <- sample(these.messages$num, 1)

  this.message.df <- dplyr::filter(these.messages, num == this.msg.num)
  this.message <- this.message.df$message

  utils::globalVariables(c(
    # dplyr/data columns
    "genotype","well","field","unique.cell","object.id",
    "object.type","x.coord","y.coord","corr.intensity",
    "comp_id","comp_x","comp_y","comp_size",
    "center_x_px","center_y_px","readout","readout_name","readout_label",
    "readout.type","percent","value","dev",
    # data.table bits that R CMD check complains about
    "cluster",".",
    # other package functions using these:
    "plate","dispensed.well","well.contents","single.fluid.concentration",
    "dmso.percent","val","surface","x","y","num"
  ))

  packageStartupMessage(
    paste0(
      "########################################\n######## ndrutils ", ver, " ready. #########\n", this.message, "\n########################################\n"
    )
  )
}
