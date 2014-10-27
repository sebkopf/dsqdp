#' run any updates on the data structure
#' 
#' this function figures out what the current version is and runs any
#' potentially necessary update functions on the old data structure
#' @return the new version number
#' @note not sure this is the best way of doing the data structure updates
#' 
update_dsqdp <- function(data_file, compounds_lib) {
  # check version of current data file
  if ( !file.exists(data_file)) stop("ERROR: could not find data file at ", data_file)
  load(data_file)
  
  # compare versions
  if (g.currentVersion > packageVersion('dsqdp')) {
    go_github <- gconfirm(paste("The data in this DSQDP workspace was generated in version", g.currentVersion, "but the installed 'dsqdp' package is only version", packageVersion('dsqdp'), "- please update to the newer version before loading this workspace. Do you want to go to the DSQDP GitHub page for help on how to update?"))
    browseURL('https://github.com/sebkopf/dsqdp')
    stop("codebase too old for data")
  } else if ( g.currentVersion < packageVersion('dsqdp') ) {
    message("\tChecking for updates to the data structure (version ", g.currentVersion, 
            ")... new version of DSQDP package detected (version ", packageVersion('dsqdp'),
            ")... running updates...")
    new_version <- do.call(
      paste0("update_dsqdp_to_", packageVersion('dsqdp')), args=list(g.currentVersion, data_file, compounds_lib))
    message("\tUpdates complete, ready to go.")
    return(new_version)
  } else {
    message("\tChecking for updates to the data structure (version ", g.currentVersion, 
            ")... identical DSQDP package detected (version ", packageVersion('dsqdp'),
            "), all ready to go.")
    return(packageVersion('dsqdp'))
  }
}

update_dsqdp_to_3.0 <- function (version, data_file, compounds_lib) {
  if (version < 2.1)
    update_dsqdp_to_2.1(version)
  
  message("\tUpdating DSQDP from version 2.1 to version 3.0...")
  message("\tBacking up data file with previous version to backups/DSQDP_v2.1.RDATA")
  file.copy(from = data_file, to = "backups/DSQDP_v2.1.RDATA")
  
  message("\tGenerating external compounds library at '", compounds_lib, "' from '", data_file, "'...")
  
  # load data
  load(data_file, .GlobalEnv)
  
  # export compounds
  save_compounds(compounds_lib)
  
  # save compounds checksum
  message("\tUpdating data file...")
  g.compounds_checksum <<- calc_compounds_checksum(compounds_lib)
  g.currentVersion <<- 3.0
  save_dsqdp_data(data_file)
  
  message("\t*** NOTE: the compounds library is now stored in the comma-separate-value (csv) file 'libs/compounds.csv' to make it easier to share. ***")
  return(3.0)
}

update_dsqdp_to_2.1 <- function(version) {
  if (version < 2)
    update_dsqdp_to_2()
  
  message("\tUpdating DSQDP from version 2.0 to version 2.1... finished.")
  return(2.1)
}

update_dsqdp_to_2 <- function() {
  stop("Sorry, upgrading really old data files (before version 2.0) is no longer supported. Please contact Sebastian for help with this (send your whole working director as a zip file).")
}