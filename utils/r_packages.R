## R package installation and configuration

install.packages(c("dplyr", 
                   "haven", 
                   "ggplot2",
                   "gt",
                   "IRdisplay",
                   "IRkernel",
                   "tidyr"),
                 repos = "https://cran.rstudio.com/")

IRkernel::installspec(name = 'ir40', displayname = 'R 4.0')