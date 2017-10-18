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
                       setwd("/home/rstudio/tools/"),
                       setwd("~/github/tools/"))
        }

        ref_pkgs <- fread("HBH_R_pkgs.csv")

        bound_pkgs <- rbindlist(list(new_pkgs, ref_pkgs))
        setorder(bound_pkgs, Package, Version)
        merged_pkgs <- bound_pkgs[bound_pkgs[,
                                             .I[1],
                                             by = Package]$V1
                                  ]

        file.rename("HBH_R_pkgs.csv", paste0("./pkg_lists/HBH_R_pkgs_",
                                             Sys.Date(),
                                             "_",
                                             length(list.files("pkg_lists/")) + 1,
                                             ".csv"))
        write_csv(merged_pkgs, "HBH_R_pkgs.csv")

}
