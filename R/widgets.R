# load widgets from a data object
widgets.load<-function(widgets, data) {
  sapply(names(widgets), function(i) {
    if (i%in%names(data)) { #field exists in data
      if (class(widgets[[i]])[[1]]=="gTable") { # gtable style widgets
        if (class(data[[i]])=="list") # single record
          widgets[[i]][] <- data.frame(data[[i]], stringsAsFactors=FALSE)  
        else # multiple records
          widgets[[i]][] <- data[[i]]	
      } else # all other widgets
        svalue(widgets[[i]]) <- data[[i]]
    } else
      print(paste("WARNING: trying to load widget", i, "but no corresponding field found in dataset."))
  })
}

# get widgets into list
widgets.getValues<-function(widgets) {
  return (sapply(names(widgets), function(var) {
    tryCatch(list(widget.getValue(widgets[[var]])),
             warning=function(w) { return(NA) } ) # coerce with NA values      
  }))
}

# get widget value (returns gTable always as data frame)
widget.getValue<-function(widget) {
  if (class(widget)[[1]]=="gTable") #in case it's a gTable(i.e. data frame), have to access info slightly differently
    if (class(widget[])=="list") # single record is returned as a list rather than a dataframe
      return (data.frame(widget[], stringsAsFactors=FALSE))
  else # multiple records in table are returned properly as data frame
    return (widget[]) 
  else
    return(svalue(widget))
}

# get widgets into dataframe (WARNING: will collapse any widgets that are tables down to NULL, if you want to preserve those, use widgets.getValues instead to get a list)
widgets.getValuesAsDF<-function(widgets) {
  return (data.frame(sapply(names(widgets), function(var) {list(svalue(widgets[[var]]))}), stringsAsFactors=TRUE))
}

#################
# PLOT NOTBEOOK #
#################

#FOR TESTING PURPOSES
#win<-gwindow("blub")
#pn.GUI(gframe(cont=win, horizontal=FALSE), win)

# make GUI forplot notbook
# new plot objs = list() object defining what kind of parameters are on a plot object by default
# - the load handlers are just passed the currently selected plot object for doing whatever they want with it
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Rightclick", "MouseMotion"
# --> pass like this plotEventHandlers=list(droptarget=fun, Clicked=fun)
pn.GUI<-function(container, window, newPlotObj=NULL, 
                 newPlotObjLoadHandler=NULL, plotObjLoadHandler=NULL, plotEventHandlers=list(),
                 enablePlotLabel=TRUE, enableMenuButtons=TRUE, startWithTab=TRUE){
  
  pn<-list() # plots notebook object
  pn$win<-window
  pn$enablePlotLabel<-enablePlotLabel
  
  # actions to interact with the plots
  #FIXME: figure out how to make keyboard accelerators work (should be key.accel="Control-n" and parent=win for gaction but always fails, not sure why)
  #NOTE: as of august 2013, the keyboard accelerators were not implemented for RGtk2
  pn$actions<-list(
    aNewPlot = list(label="New Plot", icon="gtk-page-setup", handler=function(...) pn.newPlotTab(pn, tabObj=newPlotObj, eventHandlers=plotEventHandlers, loadHandler=newPlotObjLoadHandler, label=paste("Plot", length(pn$plot.nb)+1, sep="")) ),
    aClosePlot = list(label="Close Plot", icon="gtk-cancel", handler=function(...) pn.deletePlotTab(pn, loadHandler=plotObjLoadHandler)), 
    aSavePlot = list(label="Save Plot", icon="gtk-save-as", handler=function(...) pn.savePlotGUI(pn, index=svalue(pn$plot.nb))), 
    aPrintPlot = list(label="Print Plot", icon="gtk-print", handler=function(...) pn.printPlot(pn, index=svalue(pn$plot.nb))), 
    aSaveAll = list(label="Save All", icon="gtk-harddisk", handler=function(...) pn.savePlotGUI(pn)))
  if (enableMenuButtons) {
    pn$buttons.grp<-ggroup(cont=container, horizontal=TRUE)
    addSpring(pn$buttons.grp)
    for (act in pn$actions)
      gbutton(action=gaction(act$label, icon=act$icon, handler=act$handler), cont=pn$buttons.grp)
  }
  
  # plots notebook
  pn$plot.nb <- gnotebook(cont=container, expand=TRUE)
  pn$plot.nb.changedHandler<-addHandlerChanged(pn$plot.nb, handler=function(h,...) pn.selectPlotTab(pn, h$pageno, loadHandler=plotObjLoadHandler))
  if (startWithTab)
    pn.newPlotTab(pn, tabObj=newPlotObj, label="Plot1", loadHandler=newPlotObjLoadHandler, eventHandlers=plotEventHandlers)
  
  return(pn)
}

# save handler
# save the plot with the provided index
# if none is provided, save all plots
pn.savePlotGUI<-function(pn, index=NULL){
  if (is.null(index)) { # save all plots
    f=gfile("Select the folder where to save all the plots.", type="selectdir", cont=pn$win)
  } else { # save index plot
    f=gfile("Select where to save this graph.", type="save", cont=pn$win, 
            initialfilename = paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[index],".pdf", sep=""),
            filter = list("PDF Files" = list(patterns=c("*.pdf")), "All files" = list(patterns = c("*"))))
  }
  
  if (!is.na(f)){
    grp<-ggroup(cont=(w<-gwindow("Save plot as pdf", width=200, height=100, spacing=30)), horizontal=FALSE, expand=TRUE)
    dlggrp<-glayout(container=grp, spacing=10)
    dlggrp[1,1]<-glabel("Width [inches]:",con=dlggrp)
    dlggrp[1,2]<-(width <- gedit(8,container=dlggrp, coerce.with=as.numeric))
    
    dlggrp[2,1]<-glabel("Height [inches]:",con=dlggrp)
    dlggrp[2,2]<-(height <- gedit(6,container=dlggrp, coerce.with=as.numeric))
    
    #dlggrp[3,1]<-glabel("Unit:",con=dlggrp)
    #dlggrp[3,2]<-(units <- gcombobox(c("in","cm","mm"),container=dlggrp))
    
    gbutton("save", cont=grp, handler=function(h,...) {
      if (is.null(index)) { # save all
        for (i in 1:length(pn$plot.nb)) 
          pn.savePlot(pn, i, file.path(f, paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[i],".pdf", sep="")), width=svalue(width), height=svalue(height))
      } else { # save just the current
        if (length(grep("\\.pdf$", f))==0) f<-paste(f,".pdf",sep="") # ensure .pdf ending
        pn.savePlot(pn, index, f, width=svalue(width), height=svalue(height))
      }
      pn.reactivatePlot(pn) # reactivate previously active plot
      dispose(w)
    })
  }
}

# save the plot with the given index
pn.savePlot<-function(pn, index, file, width=8, height=6) {
  pn.activatePlot(pn, index)
  dev.copy2pdf(file=file, width=width, height=height) # copy graph
}

# print the plot with the given index
pn.printPlot<-function(pn, index, width=8, height=6) {
  if (exists("win.print")) { # on windows, go print
    pn.activatePlot(pn, index)
    win.print(width=width, height=height) # launches print interface
    pn.activatePlot(pn, index) # reactivate graphics device
  } else 
    gmessage("Sorry, direct printing is not yet supported on Linux/MacOS.\nPlease save the plot as a pdf and print from there.")
}

# make new plot tab
# provide more detailed plot object if keeping other parametrs is desired
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Changed", "Rightclick", "MouseMotion", "RightlickMousePopupmenu" 
# --> pass like this plotEventHandlers=list(droptarget=fun, clicked=fun)
pn.newPlotTab<-function(pn, tabObj=NULL, label="Plot", loadHandler=NULL, eventHandlers=list()) {
  # block handlers
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
  
  # make new tab
  grp<-ggroup(cont=pn$plot.nb, horizontal=FALSE, label=label)
  if (pn$enablePlotLabel)
    addHandlerKeystroke(gedit(label, cont=grp), handler=function(h,...) {pn.changePlotTabName(pn, svalue(h$obj))})
  gg<-ggraphics(cont=grp)
  blockHandler(obj=gg) # disable automatic 2nd mouse button popup handler (for save and copy)
  
  # event handlers
  if (!is.null(eventHandlers$droptarget))
    adddroptarget(gg, targetType="object", handler=eventHandlers$droptarget)
  if (!is.null(eventHandlers$Clicked))
    addHandlerClicked(gg, handler=eventHandlers$Clicked)
  if (!is.null(eventHandlers$Changed))
    addHandlerChanged(gg, handler=eventHandlers$Changed)
  if (!is.null(eventHandlers$Rightclick))
    addHandlerRightclick(gg, handler=eventHandlers$Rightclick)
  if (!is.null(eventHandlers$MouseMotion))
    addHandlerMouseMotion(gg, handler=eventHandlers$MouseMotion)
  if (!is.null(eventHandlers$RightlickMousePopupmenu))
    add3rdMousePopupmenu(obj=gg, menulist=eventHandlers$RightlickMousePopupmenu)
  
  # make new object
  if (is.null(tabObj))
    tabObj<-list() # new object
  tabObj$gg<-gg # store the graphics object
  
  if (length(pn$plot.nb) == 1) 
    tag(pn$plot.nb, "tabs")<-list()
  tag(pn$plot.nb, "tabs")[[length(pn$plot.nb)]]<-tabObj # add new object
  
  # load
  if (!is.null(loadHandler))
    do.call(loadHandler, list(obj=tabObj))
  
  # unblock handlers
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
}

# change plot tab name (not stored in object)
pn.changePlotTabName<-function(pn, label) {
  names(pn$plot.nb)[svalue(pn$plot.nb)]<-label
}

# delete plot
# (deletes specific plot if index is passed in otherwise just the currently selected ones)
pn.deletePlotTab<-function(pn, index=NULL, loadHandler=NULL) {
  if (!is.null(index))
    svalue(pn$plot.nb)<-index
  else
    index<-svalue(pn$plot.nb)
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # block changed handler
  dispose(pn$plot.nb) #remove plot
  tag(pn$plot.nb, "tabs")[[index]]<-NULL # remove plot object
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # unblock changed handler
  pn.selectPlotTab(pn, svalue(pn$plot.nb), loadHandler=loadHandler)
}

# set plot tab specifrically
pn.setPlotTab<-function(pn, plotI, loadHandler=NULL) {
  svalue(pn$plot.nb)<-plotI
  pn.selectPlotTab(pn, plotI, loadHandler=loadHandler)
}

#select plot
pn.selectPlotTab<-function(pn, plotI, loadHandler=NULL) {
  if (plotI<=length(tag(pn$plot.nb, "tabs"))) { # make sure this is not when adding a new plot #FIXME
    pn.activatePlot(pn, plotI)
    if (!is.null(loadHandler))
      do.call(loadHandler, list(obj=pn.getPlotTabParam(pn, plotI)))
  }
}

# activate graphics widget of a plot index
pn.activatePlot<-function(pn, plotI) {
  gg<-pn.getPlotTabParam(pn, plotI, params="gg") # set ggraphics visible
  visible(gg)<-TRUE
}

# reactivate currently selected graphics widget
pn.reactivatePlot<-function(pn) {
  pn.activatePlot(pn, svalue(pn$plot.nb))
}

#get plot properti(es) for a tab
#params as c("test", "test2")
pn.getPlotTabParam<-function(pn, index, params=NULL) {
  if (is.null(params))
    return (tag(pn$plot.nb, "tabs")[[index]])
  else if (length(params)==1)
    return (tag(pn$plot.nb, "tabs")[[index]][[params]])
  else
    return (tag(pn$plot.nb, "tabs")[[index]][params])
}

# get all plot tab objs
pn.getAllPlotTabObjs<-function(pn) return (tag(pn$plot.nb, "tabs"))

# get all plot tab names
pn.getAllPlotTabNames<-function(pn) return(names(pn$plot.nb))

# get selected plot tab name
pn.getSelectedPlotTabName<-function(pn) return(names(pn$plot.nb)[svalue(pn$plot.nb)])

# get them for the selected tab
pn.getSelectedPlotTabParam<-function(pn, params=NULL) return (pn.getPlotTabParam(pn, svalue(pn$plot.nb), params=params))

# set plot properti(es)
# params as list
pn.setPlotTabParam<-function(pn, index, params) {
  for (var in names(params))
    tag(pn$plot.nb, "tabs")[[index]][var]<-params[var]
}

# set them for the selected tab
pn.setSelectedPlotTabParam<-function(pn, params) pn.setPlotTabParam(pn, svalue(pn$plot.nb), params)

# utility function for storing user information within the current tab (with id "plotinfo")
# info = list of parameters
pn.storeInfo<-function(pn, info, reset=FALSE) {
  if (reset)
    plotinfo<-list()
  else
    plotinfo<-pn.getSelectedPlotTabParam(pn, params="plotinfo")
  for (name  in names(info))
    plotinfo[name]<-info[name]
  pn.setSelectedPlotTabParam(pn, list(plotinfo=plotinfo)) # save plot parameter
}

# utility function for retrieving all user information from the current tab
pn.getAllInfo<-function(pn) return(pn.getSelectedPlotTabParam(pn, params="plotinfo"))

# utility function for retrieving parts of the user information from the current tab
pn.getInfo<-function(pn, fields) return(pn.getAllInfo(pn)[fields])




###################################
# code for generic data functions #
# Copyright 2013 Sebastian Kopf   #
# seb.kopf@gmail.com              #
###################################

###############
# excel files #
###############

# reads a data frame from an excel sheet with the headers provided in the startRow
# trueColNames - gives the data frame the real names from the columns
excel.readDF <- function(file, sheet = 1, startRow = 1, stringsAsFactors=FALSE, trueColNames = TRUE) {
  df <- read.xlsx2(file, sheet, startRow=startRow, stringsAsFactors=stringsAsFactors, header=TRUE) 
  if (trueColNames) {
    dfcols <- read.xlsx(file, sheet, rowIndex=startRow, header=FALSE, stringsAsFactors=stringsAsFactors) 
    names(df) <- gsub("\\s*$", "", dfcols, perl=T) # trailing whitespaces removed
  }
  return(df)
}

# write data frames to an excel sheet
# file = file name/path
# df = either
#   - a data frame (data.frame(x=.., y=...))
#   - a list of data frames with the list IDs as sheet names (list("Sheet 1" = data.frame(x=.., y=...)))
excel.writeDF <- function(file, df) {
  wb <- createWorkbook(type="xlsx")
  csStd <- CellStyle(wb) + Font(wb)
  csBold <- CellStyle(wb) + Font(wb, isBold=TRUE) 
  if (identical(class(df), "data.frame")) { # single data frame
    sheet <- createSheet(wb, sheetName="Sheet1")
    addDataFrame(df, sheet, startRow=1, startColumn=1, colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
  } else if (identical(class(df), "list")) { # multiple data frames
    for (dfi in 1:length(df)) {
      sheetName <- names(df)[dfi]
      if (identical(sheetName, "")) # no sheet name given
        sheetName <- paste0("Sheet", dfi)
      sheet  <- createSheet(wb, sheetName=sheetName)
      addDataFrame(df[[dfi]], sheet, startRow=1, startColumn=1, colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
    }
  }
  saveWorkbook(wb, file) 
}

#################
# data transfer #
#################

# function to output a dataframe to the clipboard (can then be easily copied into excel for example)
cp.copyDF<-function(df) {
  if (exists("writeClipboard")) # windows
    clipboard <- "clipboard"
  else # unix/MacOS
    clipboard <- pipe("pbcopy", "w")
  
  write.table(df, file=clipboard, sep="\t", row.names=FALSE)
  
  if (!exists("writeClipboard")) # unix
    close(clipboard)
}

# function to paste clipboard to data frame
cp.pasteDF<-function(header=TRUE, sep="\t", skip=0, comment.char="#", row.names=NULL, quote=""){
  return(read.clipboard(sep=sep, stringsAsFactors=FALSE, header=header, 
                        skip=skip, comment.char=comment.char, row.names=NULL, quote=quote))
}

#############################
# reading binary data files #
#############################
# NOTE: using the 010 Editor software, figuring out structures will be easier!

# read whole binary file and return the raw data
bin.readFile<-function(path) {
  con<-file(path, "rb")
  rawdata<-readBin(con, raw(), n=file.info(path)$size)
  close(con)
  return(rawdata)
}

# assemble read structure for binary file
# either pass in vectors and it will assemble a dta frame from it or pass in structure to amend
bin.struct<-function(what, length, size = 1, id = NA, struct = NULL) {
  df<-data.frame(id=id, what=what, length=length, size=size, stringsAsFactors=FALSE)
  if (!is.null(struct))
    df<-rbind(struct, df)
  return (df)
}

# provides the total length (in bytes) of a structure
bin.structLength<-function(struct) {
  return(sum(struct$length*struct$size))
}

# read from byte stream (data has to be a byte stream, structure has to be a structure data frame)
# returns a list coded by the ids in the structure
bin.parseStruct<-function(data, struct, offset = 0, saveUnknown = TRUE) {
  results <- list()
  pos <- offset + 1
  size <- length(data)
  for (i in 1:nrow(struct)) {
    id<-struct$id[i]
    if (!is.na(id) || saveUnknown) { # only process if real data or saving unknowns
      
      # different reads
      if (struct$what[i] == "raw")
        read<-paste(readBin(data[pos:size], "raw", n=struct$length[i], size=struct$size[i]), collapse=" ")
      else if (struct$what[i] == "character" && struct$size[i] == 1)
        read<-rawToChar(readBin(data[pos:size], "raw", n=struct$length[i], size=1))
      else if (struct$what[i] == "character" && struct$size[i] == 2)
        read<-paste(readBin(data[pos:size], "character", n=struct$length[i], size=2), collapse="")
      else
        read<-readBin(data[pos:size], struct$what[i], n=struct$length[i], size=struct$size[i])
      
      # saving results
      if (is.na(id))
        id <- "unknown"
      if (!is.null(results[[id]]))
        results[[id]]<-c(results[[id]], list(read))
      else
        results[[id]]<-read
    }
    pos <- pos + struct$size[i] * struct$length[i]
  }
  return(results)
}

# optimized method for repeat reading of the same structure (say to read an entire data array)
# WARNING: only supported for structures where each element is of length=1 (otherwise, mayhem!)
bin.multiParseStruct<-function(data, struct, rep, offset = 0) {
  datalength <- bin.structLength(struct)
  subdata<-data[(offset+1):(offset+rep*datalength)]
  structpos<-0
  df<-data.frame(read = 1:rep)
  for (i in 1:nrow(struct)) {
    byteselect<-rep(FALSE, datalength)
    byteselect[(structpos+1):(structpos<-structpos+struct$size[i])]<-TRUE
    if (!is.na(struct$id[i]))
      df[struct$id[i]]<-readBin(subdata[which(rep(byteselect, times=rep))], struct$what[i], size=struct$size[i], n=rep)
  }
  return (df)
}

# find all ascii strings in a data stream
# FIXME: it appears that after each string, there are 3x null character (i.e. 00 00 00) --> use this to make finding strings better! (couldn't quite figure out how to recognize 00 characters)
bin.findAllAscii<-function(data, minlength=10) {
  regexp<-paste("[\u0020-\u007e]{", minlength, ",}", sep="")
  text<-data.frame(
    byteStart = grepRaw(regexp, data, all=TRUE), #get ANSII strings
    value = ldply(grepRaw(regexp, data, all=TRUE, value=TRUE), 
                  function(x) rawToChar(x))$V1, encoding='ASCII', stringsAsFactors=FALSE)
  text$byteEnd<-text$byteStart + nchar(text$value) - 1
  text$byteLength<-text$byteEnd - text$byteStart + 1
  text$strLength<-text$byteLength
  return (text)
}

# find all unicode strings in a binary data stream
bin.findAllUnicode<-function(data, minlength=5) {
  regexp<-paste("([\u0020-\u007e][^\u0020-\u007e]){", minlength, ",}", sep="")
  text<-data.frame(
    byteStart = grepRaw(regexp, data, all=TRUE), #get Unicode strings
    value = ldply(grepRaw(regexp, data, all=TRUE, value=TRUE), 
                  function(x) rawToChar(x[c(TRUE, FALSE)]))$V1,
    #paste(readBin(x, "character", n=length(x)/2, size=2), collapse=""))$V1, 
    encoding='Unicode', stringsAsFactors=FALSE)
  text$byteEnd<-text$byteStart + nchar(text$value) * 2 - 1
  text$byteLength<-text$byteEnd - text$byteStart + 1
  text$strLength<-text$byteLength/2
  return (text)
}

# find all text in a binary data stream
bin.findAllText<-function(data, asciiL=10, unicodeL=5) {
  text<-rbind(bin.findAllAscii(data, minlength=asciiL), bin.findAllUnicode(data, minlength=unicodeL))
  text<-text[order(text$byteStart),] # sort all text
  text$byteGap<-diff(sort(c(text$byteStart, text$byteEnd, length(data))))[c(FALSE,TRUE)] # add byte gap
  return(text)
}

# clean up text by removing randomly found strings that are clearly not proper targets
bin.cleanText<-function(text, removeText="Arial", removePattern = "[&{}!^@?#]", unlessByteLength = 26, unlessText = "Is Ref.?", printRemoved = TRUE) {
  rem<-union(
    which(text$value==removeText),
    intersect(grep(removePattern, text$value), which(text$byteLength < unlessByteLength & !(text$value%in%unlessText))))
  if (printRemoved) {
    cat("\nRemoved:\n")
    print(text[rem,"value"])
  }
  text<-text[-rem,]
  # re calculate gaps between occuring strings
  text$byteGap<-diff(sort(c(text$byteStart, text$byteEnd, max(text$byteEnd))))[c(FALSE,TRUE)] # add byte gap
  return (text)
}

####################################
# data frame convenience functions #
####################################

# get indices of columns by name
df.getColIs <- function(df, cols) {
  indices <- sapply(cols, function(col) which(names(df) == col)[1])
  if (!is.empty(which(naIs <- is.na(indices))))
    warning(paste("Some column names were not found:", cols[naIs] ))
  return(indices[!naIs])
}

# delete certain columns from data frame (by name or indices)
df.delCols<-function(df, cols) {
  if (mode(cols)=="numeric") #referring to indices
    return (df[-cols])
  else
    return (df[!names(df) %in% cols]) #referring to names 
}

# rename certain columns
# if "from" is omitted, "to" is expected to contain names for all columns
df.nameCols<-function(df, to, from) {
  if (missing(from)) 
    names(df) <- to
  else {
    for (i in 1:length(from)){
      if (!is.empty(col<-which(names(df)==from[i])))
        names(df)[col]<-to[i]
    }
  }
  return (df)
}

# change the data type of several columns
# type
# - integer
# - numeric
# - excelDate
# - excelTimestamp
df.changeDataType<-function(df, cols, type="numeric") {
  for (i in df.getColIs(df, cols)) {
    if (identical(type, "excelDate"))
      df[,i] <- as.Date(as.integer(df[,i]) - 25569, origin="1970-01-01") # convert from excel date
    else if (identical(type, "excelTimestamp"))
      df[,i] <- as.POSIXct((as.numeric(df[,i])-25569)*86400, tz="GMT", origin="1970-01-01")
    else  
      mode(df[,i])<-type
  }
  return (df)
}

# convert all numeric columns in a data frame to true numeric
df.convertNumerics<-function(df) {
  for (i in which(sapply(df, is.numeric))) df[[i]]<-as.numeric(df[[i]])
  return(df)
}

# change date/time to time diff (makes a column with the name of units)
# baseDT
#	- if omitted, take first entry as reference
#	- if int, take this element of the dataset as BT
#	- actualy date time --> make by strToDateTime("<date>","<time>")
# valid units --> "hours", "days" (probably some others too)
df.dateTimeToDiff<-function(df, baseDT, col=c("date","time"), units="hours", format="%m/%d/%y %H:%M") {
  datetime<-strToDateTime(do.call(paste,c(df[col])), format=format)
  if(missing(baseDT)) baseDT<-datetime[1]
  if(mode(baseDT)=="numeric") baseDT<-datetime[baseDT]
  df[units]<-timeDiff(datetime,baseDT,units=units)
  return(df)
}

# add experimental info to reshaped data set
# col = name of new column, e.g. "strain" (or old column if want to overwrite, resetcol if you want to reset the whole thing)
# info = list coded with the new information
#   e.g. list("PA14"=c(1,4,5), "MAI-1"=c(2,3)), where 1-5 are the ids of the different experimental conditions (after doing df.reshapeAvgs or reshapeRawData)
# regexp (default: FALSE), how to look for these values, by default it looks by fixed string, set regexp to TRUE to evaluate a regular expression, NOTE: additional parameters are passed to the regexp function
# resetcol ( default: TRUE) --> resets the column entirely, otherwise only valuse specified by the matching conditions are set
# asnumeric --> decide whether the newly created values are numerics
df.addInfoCol<-function(df, col, info, id="exp", regexp=FALSE, resetCol=FALSE, asNumeric=FALSE, ...) {
  if (resetCol==TRUE || !(col%in%names(df)))
    df[col]<-NA # make new column / overwrite existing data in it
  for (i in 1:length(info)) {
    value<-names(info)[i]
    idvals<-info[[i]]
    for (idval in idvals) {
      if (regexp)
        matches<-grep(idval, df[[id]],...)
      else
        matches<-which(df[[id]]==idval)
      if (!is.empty(matches)) df[matches,col]<-value
    }
  }
  if (asNumeric)
    df[[col]]<-as.numeric(df[[col]])
  return(df)
}


################################
# generic data variable access #
################################

# get index by id
var.getIndexByID<-function(var, id, idField="ID") {
  if (!is.null(id) && !is.empty(sel<-which(var[[idField]]==id)))
    return(sel) #selected index in dataset
  return (NULL)
}

# get entry by id
var.getEntryByID<-function(var, id, idField="ID", fields=NULL) {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    if(class(var) == "data.frame") { # data.frame
      if (is.null(fields)) 
        return (var[index,]) # return record
      else
        return (var[index, fields]) # return specific fields
    } else if (class(var) == "list") { #list
      if (is.null(fields)) 
        return (lapply(var,"[[",index))
      else if (length(fields) == 1) # return just this one field
        return (var[[fields]][[index]])
      else # multiple records
        return (lapply(var[fields],"[[",index))
    } else
      stop("data structure type not supported")
  }
  return (NULL)
}

# new object with fields whatever is passed in
# returns a list
var.new<-function(...) {
  return (list(...))
}

# new object with fields whatever is passed in
# returns as data frame
var.newAsDF<-function(...){
  return (data.frame(var.new(...), stringsAsFactors=FALSE))
}

# add entry in variable
# returns var
# supports reordering if desired (just supply currentOrderN to insert after this position)
# FIXME: ordering not tested/supported(?) for lists
var.add<-function(var, newId, data, idField="ID", orderField="Order", orderN=NULL) {
  if ( !(newId%in%var[[idField]])) {
    # reordering:
    if (!is.null(orderN)) { # insert at this orderN
      var<-var.updateSorting(var, orderN, orderField=orderField) # update all orderNs
      data[[orderField]]<-orderN
    } 
    # insert new entry
    if (class(var) == "data.frame") {
      newIndex<-nrow(var)+1
      var[newIndex,idField]<-newId
    } else if (class(var) == "list") {
      newIndex<-length(var[[idField]])+1
      var[[idField]][[newIndex]]<-newId
    }
    var<-var.update(var, newId, data, idField=idField)
    return (var)
  } else 
    stop(paste("ERROR: failed to add data set with new ID", newId, "because this ID already exists in the dataset."))
}

# update entry in variable (by default identified with idField but can go via index)
# returns updated variable
# WARNING: this might only work well with data frame fields that are NOT FACTORS
var.update<-function(var, id, data, idField="ID") {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    for (field in names(data))
      if (field%in%names(var)) { # update varialbe
        # get new value (either from list or data frame)
        if (class(data)=="data.frame")
          newValue<-data[1,field]
        else if (class(data)=="list")
          newValue<-data[[field]]
        
        # set value depending on what you're updating (list or data frame)
        if (class(var)=="data.frame")
          var[index,field]<-newValue
        else if (class(var)=="list")
          var[[field]][[index]]<-newValue
        
      } else
        print(paste("WARNING: trying to update table field", field, "with value", data[[field]], "but field does not exist in variable.")) 
  }
  return (var)
}

# delete entry
# returns updated variable
var.delete<-function(var, id, idField="ID") {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    if (class(var)=="data.frame") # delete record from data.frame
      var<-var[-var.getIndexByID(var, id, idField=idField),] 
    else if (class(var)=="list") { # delete record from list
      for (field in names(var)) {
        if (class(var[[field]])=="list") #in case it's a sublist, need to set elements to NULL
          var[[field]][[index]]<-NULL
        else # in case it's a direct array (e.g. numeric, character, logical)
          var[[field]]<-var[[field]][-index]
      }
    }
  }
  return(var) 
}

################
# data sorting #
################

# update order in a variable
# updateN.from - the ordering number from which on (inclusive, >=) all sorting numbers should be updated 
# updateN.to - if not NULL (default), the ordering number until which (inclusive, <=) all ordering numbers should be updated
#            - if NULL, goes till the end (nrow(table[]))
# returns the updated variable
# can changeBy 1 or -1 depending on need
var.updateSorting<-function(var, updateN.from, updateN.to=NULL, orderField="Order", changeBy=1) {
  if (is.null(updateN.to))
    updateN.to<-max(var[[orderField]], na.rm=TRUE)
  if ( !is.null(updateN.from) && !is.empty(indices<-which(var[[orderField]]>=updateN.from & var[[orderField]]<=updateN.to)) ) 
    var[[orderField]][indices]<-var[[orderField]][indices]+changeBy
  return (var)
}

