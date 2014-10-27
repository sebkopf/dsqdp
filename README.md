DSQ Data Processor (dsqdp)
=====

This package provides a simple user interface to facilitate aggregating data from TIC and FID traces from the Excalibur GC-MS software.

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces (http://www.gtk.org/). GTK needs to be installed prior to using this package but the installation is fairly straight forward (there is a known problem with some versions of GTK on Mac OS, details in the [installation instructions](https://gist.github.com/sebkopf/9405675)). If R and GTK are already installed and running on your system, you can go straight to [installing the dsqdp package](#install-dsqdp-package).

![Screenshot of the DSQ Data Processor](/inst/doc/screenshot.png?raw=true)

##Install dsqdp package

The **devtools** package provides a super convenient way of installing the **dsqdp** package directly from GitHub. To install **devtools**, run the following from the R command line:
```coffee
install.packages('devtools', depen=T) # development tools
```

Then simply install the latest version of the DSQ Data Processor directly from GitHub by running the following code (if it is the first time you install the **dsqdp** package, all missing dependencies will be automatically installed as well -> **ggplot2, plyr, psych, scales, grid, gWidgets, RGtk2**, etc. as well as their respective dependencies, which might take a few minutes):

```coffee
library(devtools)
install_github('dsqdp', 'sebkopf')
```

##Run dsqdp
Once installed, you can now run the DSQ Data Processor in any R workspace (command line R, RStudio, iPython Rmagic, etc.):

```coffee
library(dsqdp)
dsqdp.start()
```

Or directly from the terminal via Rscript (will start in the current directory by default but you can adjust it by changing the path in the ```setwd()``` call):

```coffee
Rscript -e 'setwd("."); library(dsqdp); dsqdp.start_from_script()'
```
