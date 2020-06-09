## R package installation and configuration

install.packages("stringi",
                 configure.args=c("--disable-cxx11"),
                 repos = "https://cloud.r-project.org/")

install.packages(c("stringi",
                   "dplyr",
                   "haven",
                   "ggplot2",
                   "gt",
                   "IRdisplay",
                   "IRkernel",
                   "tidyr"),
                 repos = "https://cloud.r-project.org/")

IRkernel::installspec(name = 'ir40', displayname = 'R 4.0')
