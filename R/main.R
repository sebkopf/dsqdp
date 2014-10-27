#' DSQ Data Processor (DSQDP)
#' 
#' User interface to facilitate aggregating data from TIC and FID traces from the Excalibur GC-MS software.
#' 
#' @name dsqdp-package
#' @aliases dsqdp
#' @docType package
#' @title dsqdp package
#' @author Sebastian Kopf
#' @seealso \code{\link{dsqdp.start}}
#' @examples
#' \dontrun{\code{dsqdp.start()}}
#' 
#' @include utils.R
#' @include load_save.R
#' @include gui.R
#' @include widgets.R
#' @include compounds.R
#' @include peaklists.R
#' @include datasets.R
#' @include imports.R
#' @include exports.R
#' @include plots.R
#' @include chromedit.R
NULL

#' Start the DSQDP Data Processor
#' 
#' @param data_file name of the data file to load, default 'DSQDP.RDATA'
#' @param compounds_lib name of the compounds library to load, default 'libs/compounds.csv'
#' @export
dsqdp.start <- function(
      data_file = 'DSQDP.RDATA', 
      compounds_lib = file.path('libs', 'compounds.csv')) {
  
  # load RGtk2
  options("guiToolkit"="RGtk2")
  
  # select working directory
  wd <- gfile("Select your DSQDP workspace folder.", type="selectdir") 
  # NOTE: gfile has a bug on "cancel", but couldn't find a way to fix or catch the error (dialog does not dispose!)
  
  # load working directory
  if ( !is.na(wd) && file.exists (wd) ) {
    setwd(wd)
    message("Loading DSQDP workspace:", getwd())
  } else {
    gmessage("Working directory does not exist.")
    stop("can't find working directory '", wd , "'")
  }
  
  # folder check
  folders <- c("backups", "chroms", "spectra", "graphs", "exports", "libs")
  sapply(folders, check_for_folder)
  
  # check for data
  if ( !file.exists(data_file) ) {
    message("\tLooking for ", data_file, "... file NOT found, initializing new ", data_file, "...")
    
    # copy both the default DSQDP.RDATA and default compounds.lib as well as a chromatogram
    file.copy(
      from = system.file("extdata", "new_DSQDP.RDATA", package="dsqdp"),
      to = data_file)
    file.copy(
      from = system.file("extdata", "default_compounds.csv", package="dsqdp"),
      to = compounds_lib)
    file.copy(
      from = system.file("extdata", "test_chrom_dataset_1_FID.RData", package="dsqdp"),
      to = file.path("chroms", "chrom_dataset_1_FID.RData"))
    
    
    if (!file.exists(data_file) || !file.exists(compounds_lib))
      stop("could not copy new DSQDP.RDATA or compounds library from package sources")

    # inform user
    gmessage(paste("Welcome to starting DSQDP for the first time in this workspace. To reload the data stored in this workspace in the future, please go back into this exact same workspace directory on startup: ", getwd()))
  } else {
    message("\tLooking for ", data_file, "... file found.")
  }
  
  # check for any necessary version updates
  new_version <- update_dsqdp(data_file, compounds_lib)
  
  # check for compounds library
  if ( !file.exists(compounds_lib) ) {
    gmessage(paste0("Looking for the compounds library in '", compounds_lib, "' but could not find the file. Make sure the library exists and has the right file name and then restart the DSQDP!"))
    stop("ERROR: could not find compounds library at ", compounds_lib)
  }
  
  # load data and compounds library
  if (!load_dsqdp(data_file, compounds_lib))
    gmessage("Warning: It looks like the compounds library that is loaded might not be identical to the one used previously with this data set. Careful in case any compound IDs might no longer be correctly assigned.")
  g.currentVersion <<- new_version
  
  message("\nLaunching DSQDP... please wait...\n\n")
  launch_dsqdp()
}

#' Start the DSQDP Data Processor with an Rscript
#' 
#' Same as dsqdp.start except it makes sure to keep the terminal alive with
#' a loop until the program actually exits
#' @export
dsqdp.start_from_script <- function(...) {
  dsqdp.start(...)
  
  # run loop until the program is finished
  message("\n\nDSQDP running modally. Have fun!")
  while (DSQDP.running) {
    
  }
}
