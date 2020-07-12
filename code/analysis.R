suppressMessages(library(AER))
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(gt)
library(haven)
library(purrr, warn.conflicts = FALSE)

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
for(vset in vset_list) {
    for (yvar in eval(parse(text = paste("vs_", vset, sep = "")))) {
        eq <- setEquation(vset, yvar)
        nam <- paste(yvar, "_IV", sep = "")
        assign(nam, ivreg(eq, data = df, weights = weight_main))
        assign(paste(nam, "_coeftest", sep = ""),
               coeftest(eval(parse(text = nam)), vcov = vcovHC(eval(
                   parse(text = nam)
               ), "HC3")))
        assign(paste(nam, "_coefci", sep = ""),
               coefci(eval(parse(text = nam)), vcov = vcovHC(eval(
                   parse(text = nam)
               ), "HC3")))
    }
}

## Generate the main figures of the paper
genFigDF <- function(vset_list) {
    figDF <- data.frame()
    for (vset in vset_list) {
        for (yvar in eval(parse(text = paste("vs_", vset, sep = "")))) {
            figDF <-
                rbind(data.frame(
                        variableLabel = eval(parse(text = paste(
                            "name_", yvar,
                            sep = ""
                        ))),
                        treatmentEffect = eval(parse(
                            text = paste(yvar, "_IV_coeftest",
                                         sep = "")
                        ))["D", "Estimate"],
                        stdError = eval(parse(
                            text = paste(yvar, "_IV_coeftest",
                                         sep = "")
                        ))["D", "Std. Error"],
                        pValue = eval(parse(
                            text = paste(yvar, "_IV_coeftest",
                                         sep = "")
                        ))["D", "Pr(>|t|)"],
                        confIntLow = eval(parse(
                            text = paste(yvar, "_IV_coefci",
                                         sep = "")
                        ))["D", "2.5 %"],
                        confIntHigh = eval(parse(
                            text = paste(yvar, "_IV_coefci",
                                         sep = "")
                        ))["D", "97.5 %"],
                        index = eval(parse(
                            text = paste("indexname_", vset,
                                         sep = "")
                        )),
                        stringsAsFactors = TRUE
                    ), figDF
                )
        }
    }
    
    figDF
}

figTwoDF <- genFigDF(c("sub_news", "social", "sub_time"))
figThreeDF <- genFigDF(c("voting", "polarize", "news"))
figFiveDF <- genFigDF(c("swb"))
figSixDF <- genFigDF(c("fbopinions" ,"postexp"))

## Plot main figures.
genMainFigure <- function(figDF) {
    mainFigure <- ggplot(figDF) +
        geom_errorbarh(aes(y = variableLabel, xmin = confIntLow,
                           xmax = confIntHigh),
                       color = "dodgerblue3") +
        geom_point(aes(x = treatmentEffect, y = variableLabel),
                   color = "dodgerblue3") +
        facet_wrap(
            . ~ index,
            strip.position = "left",
            scales = "free_y",
            ncol = 1
        ) +
        geom_vline(xintercept = 0, color = "firebrick3") + 
        labs(
            x = "Treatment effect (standard deviations)",
            y = ""
        )
    mainFigure
}

figureTwo <- genMainFigure(figTwoDF) + labs(title = "Figure 2. Substitutes for Facebook")
figureThree <- genMainFigure(figThreeDF) + labs(title = "Figure 3. Effects on News and Political Outcomes")
figureFive <- genMainFigure(figFiveDF) + labs(title = "Figure 5. Effects on Subjective Well-Being")
figureSix <- genMainFigure(figSixDF) + labs(title = "Figure 6. Effects on Post-Experiment Facebook Use and Opinions")

# Generate extra tables.
genExtraTable <- function(figDf, title) {
    table <- figDf %>%
        select(
            variableLabel,
            treatmentEffect,
            stdError,
            pValue
               ) %>%
        map_df(rev) %>%
        gt() %>%
        fmt_number(
            columns = vars(treatmentEffect, stdError, pValue),
            decimals = 2,
            drop_trailing_zeros = FALSE,
            sep_mark = ","
        ) %>%
        cols_align(
            align = "center",
            columns = vars(treatmentEffect, stdError, pValue)
        ) %>%
        cols_align(
            align = "left",
            columns = vars(variableLabel)
        ) %>%
        cols_label(
            variableLabel = html(""),
            treatmentEffect = html("<center>Treatment Effect</center>"),
            stdError = html("<center>Standard Error</center>"),
            pValue = html("<center><i>p</i>-value</center>")
        )%>%
        tab_style(
            style = cell_text(color = "red"),
            locations = cells_body(
                columns = vars(pValue),
                rows = pValue < 0.05)
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
                columns = vars(variableLabel),
                rows = grepl("index", variableLabel))
        ) %>%
        tab_source_note(
            source_note = html("<center><i>Notes</i>: The order in which the 
                               variables appear in the table may be different 
                               from the figure.</center>")
        ) %>%
        tab_header(title) %>%
        tab_options(
            table.width = pct(75)
        )  %>%
         as_raw_html(inline_css = TRUE)
    table
}

extraTableTwo <- genExtraTable(figTwoDF, "Substitutes for Facebook")
extraTableThree <- genExtraTable(figThreeDF, "Effects on News and Political Outcomes")
extraTableFive <- genExtraTable(figFiveDF, "Effects on Subjective Well-Being")
extraTableSix <- genExtraTable(figSixDF, "Effects on Post-Experiment Facebook Use and Opinions")
