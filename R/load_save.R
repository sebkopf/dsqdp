
##### Save

#' Save data and compounds library of the current dsqdp instance
#' @param data_file name of data file
#' @param compounds_lib name of compounds library file
#' @param backup whether to save these in the backup directory
#' @export
save_dsqdp <- function(
  data_file = default_data_file(backup), 
  compounds_lib = default_lib_file(backup),
  backup = FALSE) {
  
  default_data_file <- function(backup) {
    if (backup) file.path("backups", paste0("DSQDP_data_",format(Sys.time(),format="%Y%m%d_%H%M%S"),".RData"))
    else "DSQDP.RDATA"
  }
  
  default_lib_file <- function(backup) {
    if (backup) file.path("backups", paste0("DSQDP_compounds_",format(Sys.time(),format="%Y%m%d_%H%M%S"),".csv"))
    else file.path("libs", "compounds.csv")
  }
  
  if (backup)
    message("Backing up workspace...")
  else
    message("Saving workspace...")
  
  save.image()
  g.compounds_checksum <<- save_compounds(compounds_lib)
  save_dsqdp_data(data_file)
}

#' Save all the important global variables into an R data dump 
#' @param target file for the export
save_dsqdp_data <- function (target_file) {
  stopifnot(exists("g.datasets"))
  save(g.peakLists, g.datasetLists, g.datasets, g.currentVersion, g.compounds_checksum, file = target_file)
  stopifnot(file.exists(target_file))
  message("\tData saved successfully to '", target_file, "'.")
}

#' Save compounds
#' 
#' Save currently loaded compounds list to a comma-separated-value (csv) file.
#' @param target file for the export
#' @return returns the checksum
save_compounds <- function (target_file) {
  stopifnot(exists("g.compounds"))
  write.csv(g.compounds, file = target_file, row.names = FALSE)
  stopifnot(file.exists(target_file))
  message("\tCompounds library saved successfully to '", target_file, "'.")
  return (calc_compounds_checksum(target_file))
}

##### Load

#' Load dsqdp data and compounds library into current workspace
#' @param data_file name of the data file to load, default 'DSQDP.RDATA'
#' @param compounds_lib name of the compounds library to load, default 'libs/compounds.csv'
#' @return logical - whether the check sum is the same or not
#' @export
load_dsqdp <- function(data_file = "DSQDP.RADATA", compounds_lib = file.path("libs", "compounds.csv")) {
  
  message("\nLoading data and library...")
  
  # load both
  load_dsqdp_data(data_file)
  g.compounds <<-load_compounds(compounds_lib)
  
  # compare checksum
  chksum <- identical(g.compounds_checksum, calc_compounds_checksum(compounds_lib))
  if (!chksum)
    warning("*** It looks like the compounds library that is loaded might not be identical to the one used previously with this data set. Careful in case any compound IDs might no longer be correctly assigned. ***")
  
  return(chksum)
}

#' Load data from a DSQDP data file and compound library file.
#' @param data_file path to the data file to load
#' @param compounds_lib path to the compounds library to load
#' @return nothing - loads dirctly into workspace global variables!
load_dsqdp_data <- function(data_file) {
  stopifnot(file.exists(data_file))
  # load data
  load(data_file, .GlobalEnv)
  message("\tLoading ", data_file, " data file into active workspace... done.")
}

#' Load compounds
#' @param source file for the import
#' @return compounds data frame
load_compounds <- function (source_file) {
  stopifnot(file.exists(source_file))
  # load library
  compounds <- read.csv(source_file, stringsAsFactors = F)
  message("\tLoading ", source_file, " compounds library... done.")
  return(compounds)
}

#' Calculate checksum from compounds import
calc_compounds_checksum <- function(source_file) {
  stopifnot(file.exists(source_file))
  digest(suppressMessages(load_compounds(source_file)), algo = "crc32")
}


###################
### GLOBAL VARS ###
###################

# update data sets by setting (or overwriting) an existing dataset field
DSQDP.addDataSetField<-function(field, defaultVal) {
  g.datasets[[field]]<<-rep(defaultVal, times=length(g.datasets$ID))
}

# remove an existing data set field
DSQDP.removeDataSetField<-function(field) {
  g.datasets[[field]]<<-NULL
}



