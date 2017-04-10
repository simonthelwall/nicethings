[![Travis-CI Build Status](https://travis-ci.org/simonthelwall/nicethings.svg?branch=master)](https://travis-ci.org/)
# nicethings

This collects together various functions that I find useful. 
The primary purpose is to write convenience functions for putting nicely formatted values such as regression estimates, *p* values and percentages into Rmarkdown documents without long, nested inline code strings with `round()` and other calculations. 

I dithered around calling this `stmisc` or `nicethings` and settled with `nicethings` as is sounds, well, *nicer*. 

# Installation and requirements
## Requirements

* R
* stringr

## Installation from package file

Please email me for the zip file of the package. 
Once you have received the file, please do the following:

1.  Save the zip file to a convenient location but do not unzip
2.  Open RStudio
3.  Run the following line: `install.packages(file.choose(), repos=NULL)`
4.  This will launch a file chooser where you can select the zip file you have just downloaded.

This should install the package to the R package library.

If installation is successful you will then be able to load the package with `library(nicethings)`

## Building from source

1. Download a copy of the project
1. Open in RStudio
1. Click `Build & Reload` on the build tab

# Contributing
I'd love to receive your suggestions for features.
Please see the Contribution guide for more information.
