future plans for dsqdp
=====

There's several ideas on the table for improving the user interface in version 4, but most of these will likely be rolled into a much more comprehensive implemtation of [CSIDE](http://www.github.com/sebkopf/cside). For now, just keeping these around so we don't loose track (some are already partially implemented but not to their full potential):

## V4 GUI overhaul

This vision has the peak matching right in the chromparser (extended chromparse) and separate screens for managing the compounds (or maybe just separate tabs?)

 - top level menu item for switching to compounds database (manage compounds, warning when editing names! also has spectra, etc. - also warning when editing!)
 - top level menu item for switching to peak lists (can copy between FID/TIC/DPLUS w/ conversion factor, edit lists, create new ones based on a dataset)
 - top level menu item for switching to datasets (just datatsets, can search them, edit them (main params, not even the peak table itself), graph them - just 2 columns for more graphing space!) --> compounds dropdown for IS (only defined ones allowed) and for specific peak normalization (only the ones contained in any of the selected datasets are allowed)
 - implement the peak matching in chromparse
   --> chromparser opens as usual for pasted chroms (opens with pseudo graph if pasted peaks - pseudograph has both imitated chrom with peak heights and a bar graph with the areas below)
   --> but in addition below the graph is the whole peak list updated in real time as changes are made to the chrom (try to fit whatever identified peaks there are always before re-integration again afterwards)
   --> peak lists and compounds passed to chromparse, displayed next to table as usual (just dropdown for peak lists?, searchable list for compounds), can assign as needed with live update on graph
   --> once all is done, can save and table is displayed in main screen
   --> for editing, click edit on main screen, opens chromparse back up but with peak integration locked (can unlock with warning that this is tricky and might loose peaks)
   --> also allow adding mass spectra to the chrom (which will display in a small graphing window on the right for easy comparison with the compounds library)
 - for ISODAT also implement this for direct isodat files reading (see if integrations all work okay in comparision)
   --> have plotting tab at top to allow switching from chrom (real chrom or stylized height / area barchart dual graph) to isotopic plot (showing rR and dD)
 graphing options for individual datasets
   --> just chromatogram with labels OR area/heigh hybrid bar and peak schematic chart
   --> just normalization options here
   --> same for D+ but there also option to plot other params (or multiple, all implemented with checkboxes)
 this way allow having multiple datasets open at the same time (different windows for each, not a modal dialog, make sure to manage the graphing devices properly!)
 CONSIDER alternatively also to have an editable big table (more like a spreadsheet) where all the auxiliary information about a dataset is stored (and can be edit and amended much like an excel spreadsheet so people can add columns they like, etc) - careful that ID remains unique?
   --> potentially even set this up with tabs so that people can really use it in a spreadsheet like manner where they can add whatever extra properties they want
   --> also allow easy copy and pasting of multiple columns, rows, etc
   --> also implement UNDO (every important for something like this)
 also in this table allow multiple selections (And then on autoplot, plot them all together or if single select, just one)

 FIXME: feature - implement custom attributes for datasets (e.g. selecting C and then being able to plot off of that)
 FIXME: implement mass spectra comparisons
   NOTE: store mass spectra in special files for both compounds and for peaks in peak list (if double clicking on peak in peak list you can get the comparison with the currently selected compound)
   NOTE: implement proper understanding of double clicks (write how to guide)
 FIXME: implement structures for compounds --> use notation used in NIST library (locations of atoms, together with indicators for bonds) - stored in chemical structures examples
 FIXME: HIGH PRIORITY
   - implement better plotting that combines both datasets
   --> have a 3rd plotting tab (for plotting both)
   --> checkboxes for Area/Height for DSQ and rest for D+, as well as scaling factor (radiobutton for DSQ vs D+) and number to scale the RT by
   --> this will make facet grid with all the diffrent selections atop of each each other (enables names?)
   FIXME: ideally this has the ability o stack the actual chromatograms (not possible to get from D+ I think)
 FIXME: IMPLEMENT ME!!
   - multi graph allowing selection of several parameters to be plotted in a ggplot grid layout
   - implement isodat chromatogram import and also allow comparison of chromatograms (with stretching and all)