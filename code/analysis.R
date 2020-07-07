suppressMessages(library(AER))
library(dplyr, warn.conflicts = FALSE)
library(haven)

## Set options

## Specify analytic sample
sample <- "main"
weights <- "unweighted"

# Specify main regressions
mainSpec <- "IV"
mainWeight <- "unweighted"

# Define variable sets
source("./code/DefineVarsets.R")

## Add index to variable sets
for (vset in vset_list) {
    if (vset == "secondary") {
        assign(paste("vs_", vset, sep = ""),
               eval(parse(text = paste(
                   "varset_", vset, sep = ""
               ))))
    } else{
        assign(paste("vs_", vset, sep = ""), c(eval(parse(
            text = paste("varset_", vset, sep = "")
        )),
        paste("index_", vset, sep = "")))
    }
}

## Define helper functions
importData <- function(sample, weights) {
    df <- read_stata(bzfile("./data/final_data.dta.bz2"))
    df <-
        df %>% filter(eval(parse(text = paste(
            "sample_", sample, sep = ""
        ))) == 1)
    if (weights == "unweighted") {
        df[, paste("weight_", sample, sep = "")] = 1
    }
    df
}

setEquation <- function(vset, yvar) {
    if (yvar %in% c(
        "planned_use",
        "clicked_politics_email",
        "clicked_timelimit_email",
        "time_to_reactivation",
        "index_postexp",
        "index_voting"
    )) {
        eq <- paste(yvar, "~", "D", "|", "T", sep = " ")
    } else if (yvar %in% c("fb_deact_good")) {
        eq <-
            paste(
                yvar,
                "~",
                paste("index_", vset, "_b", sep = ""),
                "+ D",
                "|",
                paste("index_", vset, "_b", sep = ""),
                "+ T",
                sep = " "
            )
    } else if (yvar %in% c("mobile_minutes_wins", "vote_2018") |
               grepl("secondary", vset) | grepl("index", yvar)) {
        eq <-
            paste(
                yvar,
                "~",
                paste(yvar, "_b", sep = ""),
                "+ D",
                "|",
                paste(yvar, "_b", sep = ""),
                "+ T",
                sep = " "
            )
    } else {
        eq <-
            paste(
                yvar,
                "~",
                paste(yvar, "_b", sep = ""),
                "+",
                paste("index_", vset, "_b", sep = ""),
                "+ D",
                "|",
                paste(yvar, "_b", sep = ""),
                "+",
                paste("index_", vset, "_b", sep = ""),
                "+ T",
                sep = " "
            )
    }
}


## Run normalized regressions.
df <- importData(sample, weights)

## Rename normalized variables to run regressions.
for(vset in vset_list) {
    for (yvar in eval(parse(text = paste("varset_", vset, sep = "")))) {
        for (time in c("", "_b")) {
            names(df)[names(df) == paste(yvar, time, sep = "")] <-
                paste(yvar, time, "_nn", sep = "")
            names(df)[names(df) == paste(yvar, time, "_nm", sep = "")] <-
                paste(yvar, time, sep = "")
        }
    }
}

## Run regressions.
for(vset in vset_list){
    for (yvar in eval(parse(text = paste("vs_", vset, sep = "")))){
        eq <- setEquation(vset, yvar)
        assign(paste(yvar, "_IV", sep=""),
               ivreg(eq, data=df, weights=weight_main, method="robust"))
    }
}

