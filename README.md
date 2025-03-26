# {kew.metrics}

Prototype Shiny application to show Kew biodiversity metrics.

## Installation

This package is not available from CRAN, but the latest development version of this package can be
installed from GitHub using the [{remotes}](https://remotes.r-lib.org/) package:

```r
# install.packages("remotes")
remotes::install_github("stevenpbachman/kew_metrics")
```

Alternatively, if you have [{pak}](https://pak.r-lib.org/), you can instead use

```r
pak::pak("stevenpbachman/kew_metrics")
```

Note that this package has a dependency on [{rWCVPdata}](https://github.com/matildabrown/rWCVPdata),
along with a suggested dependency on [{rWCVP}](https://github.com/matildabrown/rWCVP),
which are also not available on CRAN, and may need to be installed separately if the
[{remotes}](https://remotes.r-lib.org/) package is unable to find it.

## Running the application

### As a user

With the application installed, you should be able to run the application locally by calling the
`run()` method provided by the function.

```r
kew.metrics::run()
```

### As a developer

If you have cloned this repo and are currently developing on the application, you can run the
application with your local changes using the _app.R_ file at the root of this project repo.

In RStudio, this can be through the "Run App" button at the top of the text editor when the _app.R_
is open, or by running the following in an R console where the working directory is pointing to the
root of this repository:

```r
shiny::runApp()
```

The developer method calls `pkgload::load_all()` before starting the Shiny application, allowing for
the local changes to take effect before the application starts.
