## R package installation and configuration

install.packages(c("dplyr",
                   "haven",
                   "ggplot2",
                   "gt",
                   "IRdisplay",
                   "IRkernel",
                   "tidyr"),
                 repos = "https://cloud.r-project.org/")

IRkernel::installspec(name = 'ir40', displayname = 'R 4.0')
