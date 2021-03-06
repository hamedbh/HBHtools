#' Keeping Installed Packages Consistent Across Environments
#'
#' Useful function that can read in any csv output from installed.packages() and
#' attempt twice to install from CRAN any packages not locally installed (as
#' often the first attempt will fail because of missed dependencies). It will
#' then source from Bioconductor and attempt to install the remaining packages
#' from there, finally returning a character vector of the packages that could
#' not be installed from either of those sources. These will often need to
#' be installed using devtools::install_github().
#'
#' @param pkg_file The file created from installed.packages() with default path.
#' @export



# Function to test each package in the list, install only if not already present
install_new_pkgs <- function(pkg_file = "HBH_R_pkgs.csv") {
        # Check whether I'm on a Mac, and if so set .libPaths() correctly
        if(!("Linux" %in% Sys.info()['sysname'])) {
                .libPaths("~/Library/R/3.x/library")
        }

        # Now setwd() depending on system, to ensure path to package list will be
        # correct
        if("Linux" %in% Sys.info()['sysname']) {
                setwd("/home/rstudio/HBHtools")
        } else {
                setwd("~/github/HBHtools")
        }
        # Read in the list of packages as a character vector
        # Iterate through package list and attempt to install only those not
        # already present
        pkg_list <- data.table::fread(pkg_file)$Package

        purrr::walk(pkg_list, function(pkg) {
                if(!pkg %in% installed.packages()) {
                        install.packages(pkg)
                }
        })

        # Cut the list down to those that did not install correctly. Run that list
        # through a new version of the function that stops for confirmation after each
        # package.
        remaining_pkgs <- pkg_list[!pkg_list %in% installed.packages()]

        purrr::walk(remaining_pkgs, function(pkg) {
                if(!pkg %in% installed.packages()) {
                        install.packages(pkg)
                        readline(prompt = paste0(pkg,
                                                 " has finished, press RETURN to continue."))
                }
        })

        source("https://bioconductor.org/biocLite.R")
        biocLite(pkg_list[!pkg_list %in% installed.packages()])

        return(pkg_list[!pkg_list %in% installed.packages()])
}

#' Update Saved List of Installed Packages
#'
#' Function to merge the list of locally installed packages with the saved list
#' of packages.
#'
#' @param tools_dir Defaults to NULL, can pass a directory explicitly if needed
#' @export

write_R_pkg_list <- function(tools_dir = NULL) {

        new_pkgs <- data.table::data.table(installed.packages())

        # Set working directory correctly depending on whether an argument
        # was passed explicitly, otherwise based on which system I'm using.
        if(!is.null(tools_dir)) {
                setwd(tools_dir)
        } else {
                ifelse("Linux" %in% Sys.info(),
                       setwd("/home/rstudio/HBHtools/"),
                       setwd("~/github/HBHtools/"))
        }

        ref_pkgs <- data.table::fread("HBH_R_pkgs.csv")

        bound_pkgs <- data.table::rbindlist(list(new_pkgs, ref_pkgs))
        data.table::setorder(bound_pkgs, Package, Version)
        merged_pkgs <- bound_pkgs[bound_pkgs[,
                                             .I[1],
                                             by = Package]$V1
                                  ]

        file.rename("HBH_R_pkgs.csv", paste0("./pkg_lists/HBH_R_pkgs_",
                                             Sys.Date(),
                                             "_",
                                             length(list.files("pkg_lists/")) + 1,
                                             ".csv"))
        data.table::fwrite(merged_pkgs, "HBH_R_pkgs.csv")

}
