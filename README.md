DSQ Data Processor (dsqdp)
=====

This package provides a simple user interface to facilitate aggregating data from TIC and FID traces from the Excalibur GC-MS software.

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces (http://www.gtk.org/). GTK needs to be installed prior to using this package but the installation is fairly straight forward (there is a known problem with some versions of GTK on Mac OS, details in the [installation instructions](https://gist.github.com/sebkopf/9405675)). If R and GTK are already installed and running on your system, you can go straight to [installing the dsqdp package](#install-dsqdp-package).

![Screenshot of the DSQ Data Processor](/inst/doc/screenshot.png?raw=true)

##Install dsqdp package

The **devtools** package provides a super convenient way of installing the **dsqdp** package directly from GitHub. To install **devtools**, run the following from the R command line:
```coffee
install.packages('devtools', depen=T) 
```

This package compiles R libraries from source using the gcc compiler, which is usually already installed on Unix-based systems. If not, it is most easily acquired by installing [Apple's XCode command line tools](https://developer.apple.com/downloads/) (requires an Apple ID, make sure to install for your version of Mac OS X). On Windows, it requires installing the [RTools from CRAN](http://cran.r-project.org/bin/windows/Rtools/). You can confirm that you have it all up and running by checking that ```find_rtools()``` in the R command line returns ```TRUE```:

```coffee
library(devtools)
find_rtools()
```
Then simply install the latest version of the DSQ Data Processor directly from GitHub (make sure [GTK is installed](https://gist.github.com/sebkopf/9405675) first!) by running the following code (if it is the first time you install the **dsqdp** package, all missing packages will be automatically installed as well as their respective dependencies, which might take a few minutes):

```coffee
library(devtools)
install_github("sebkopf/dsqdp")
```

#### Updating to a newer version

To update an older installation of the **dsqdp* package to the newest version, just restart R and rerun ```install_github("sebkopf/dsqdp")```. If the newer version requires changes to the data structure of previous projects, they will be automatically updated when you run dsqdp in the old working directory for the first time. A backup of the old data structure will be stored in the ```backups``` subfolder just in case anything goes wrong.

##Run dsqdp

### In R
Once installed, you can now run the DSQ Data Processor in any R workspace (command line R, RStudio, iPython Rmagic, etc.):

```coffee
library(dsqdp)
dsqdp.start()
```

### From command line

Or directly from command line, a link or another script via Rscript (will start in the current directory by default but you can adjust it by changing the path in the ```setwd()``` call):

#### Unix-based systems (Linux, MacOSX)

In the terminal, type:
```coffee
Rscript -e "setwd('.'); library(dsqdp); dsqdp.start_from_script()"
```

#### Windows

In the command line (note that unless RScript.exe is in your PATH, you need to adjust it to point to the right directory):
```coffee
"C:\Program Files\R\R-3.1.1\bin\Rscript.exe" -e "setwd('.'); library(dsqdp); dsqdp.start_from_script()"
```

NOTE: when setting a starting directory in this command line/Desktop link call, such as for example ```C:\Files\```, make sure to escape the backslashes, i.e. use ```setwd('C:\\Files\\')```
