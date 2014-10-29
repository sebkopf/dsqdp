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
  if (version < 2)
    update_dsqdp_to_2(version, data_file)
  
  message("\tUpdating DSQDP from version 2.0 to version 3.0...")
  make_update_backup(2.0, data_file)
  
  message("\t\tGenerating external compounds library at '", compounds_lib, "' from '", data_file, "'...")
  
  # load data
  load(data_file, .GlobalEnv)
  
  # export compounds
  save_compounds(compounds_lib)
  
  # save compounds checksum
  message("\t\tUpdating data file...")
  g.compounds_checksum <<- calc_compounds_checksum(compounds_lib)
  g.currentVersion <<- 3.0
  save(g.peakLists, g.datasetLists, g.datasets, g.currentVersion, g.compounds_checksum, file = data_file)
  
  message("\t\t*** NOTE: the compounds library is now stored in the comma-separate-value (csv) file 'libs/compounds.csv' to make it easier to share. ***")
  return(g.currentVersion)
}

update_dsqdp_to_2 <- function(version, data_file) {
  if (version < 1.4)
    update_dsqdp_to_1.4(version, data_file)
  
  message("\tUpdating DSQDP from version 1.4 to version 2.0...")
  make_update_backup(1.4, data_file)
  
  # update
  load(data_file, .GlobalEnv)
  message("\t\tUpdating data file...")
  g.datasetLists<<-list("<< Last Session >>"=g.datasets$ID[which(g.datasets$Sel=="Yes")], "<< All >>"=c(Inf), "<< None >>"=c(0))
  g.compounds$Order<<-1:nrow(g.compounds)
  g.currentVersion<<-2.0
  save(g.compounds, g.peakLists, g.datasetLists, g.datasets, g.currentVersion, file = data_file)
  
  return(g.currentVersion)
}

update_dsqdp_to_1.4 <- function(version, data_file) {
  if (version < 1.3)
    update_dsqdp_to_1.3(version, data_file)
  
  message("\tUpdating DSQDP from version 1.3 to version 1.4...")
  make_update_backup(1.3, data_file)
  
  # update
  load(data_file, .GlobalEnv)
  message("\t\tUpdating data file...")
  DSQDP.addDataSetField("Order",NA)
  g.datasets$Order<<-1:length(g.datasets$Order)
  g.currentVersion<<-1.4
  save(g.compounds, g.peakLists, g.datasets, g.currentVersion, file = data_file)
  
  return(g.currentVersion)
}


update_dsqdp_to_1.3 <- function(version, data_file) {
  if (version < 1.1)
    update_dsqdp_to_1.1(version, data_file)
  
  message("\tUpdating DSQDP from version 1.1 to version 1.3...")
  make_update_backup(1.1, data_file)
  
  # update
  load(data_file, .GlobalEnv)
  message("\t\tUpdating data file...")
  DSQDP.addDataSetField("WaterdD",NA)
  g.currentVersion<<-1.3
  save(g.compounds, g.peakLists, g.datasets, g.currentVersion, file = data_file)
  
  return(g.currentVersion)
}

update_dsqdp_to_1.1 <- function(version, data_file) {
  if (version < 1)
    update_dsqdp_to_1(version, data_file)
  
  message("\tUpdating DSQDP from version 1.0 to version 1.1...")
  make_update_backup(1.0, data_file)
  
  # update
  load(data_file, .GlobalEnv)
  message("\t\tUpdating data file...")
  
  DSQDP.addDataSetField("IsodatFile", "") #IsotdatFile
  
  # add DPlus field for data plus data
  if (length(g.datasets$ID) > 0) {
    for (i in 1:length(g.datasets$ID)) {
      g.datasets$Dplus[[i]]<<-data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), 
                                         dD_VSMOW=numeric(), PeakNr=integer(), Modified=character(), RefPeak=logical(), RefName=character(),
                                         Amp2=numeric(), Amp3=numeric(), BGD2=numeric(), BGD3=numeric(), 
                                         rArea2=numeric(), rArea3=numeric(), rR3H2v2H2=numeric(), Filename=character(), stringsAsFactors=FALSE)
    }
  } else {
    g.datasets$Dplus <<- list()
  }
  
  # loop through peak list to add FID, DPLUS Retention times
  for (i in 1:length(g.peakLists$name)) { 
    plist<-g.peakLists$data[[i]]
    names(plist)<-c("ID","TIC")
    plist$DPLUS<-plist$FID<-plist$TIC
    g.peakLists$data[[i]]<<-plist
  }
  g.currentVersion<<-1.1
  save(g.compounds, g.peakLists, g.datasets, g.currentVersion, file = data_file)
  
  return(g.currentVersion)
}


update_dsqdp_to_1 <- function(version, data_file) {
  stop("Sorry, upgrading really old data files (before version 1.0) is no longer supported. Please contact Sebastian for help with this (send your whole working director as a zip file).")
}


#' make a backup copy of data file
make_update_backup <- function(prev_version, data_file) {
  message("\t\tBacking up data file with previous version to backups/DSQDP_v", prev_version, ".RDATA")
  file.copy(from = data_file, to = file.path("backups", paste0("DSQDP_v", prev_version, ".RDATA")))
}

