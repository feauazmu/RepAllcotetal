## Define helper functions
importData <- function(df, sample, weights) {
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

## Main figures.
genMainFigure <- function(figDF) {
    mainFigure <- ggplot(figDF) +
        ggplot2::geom_errorbarh(aes(y = variableLabel, xmin = confIntLow,
                                    xmax = confIntHigh, height = 0.3),
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
                               from the order in which they appear in the
                               figure</center>")
        ) %>%
        tab_header(title) %>%
        tab_options(
            table.width = pct(75)
        )  %>%
        as_raw_html(inline_css = TRUE)
    table
}

## Mod figures.

genFigModDf <- function(yvars, mods){
    figDF <- data.frame()
    for (mod in mods) {
        for (yvar in yvars) {
            yvarMod <- paste(yvar, "_", mod, sep = "")
            figDF <-
                rbind(data.frame(
                    variableLabel = eval(parse(text = paste(
                        "name_", yvar,
                        sep = ""
                    ))),
                    treatmentEffect = eval(parse(
                        text = paste(yvarMod, "_IV_coeftest",
                                     sep = "")
                    ))["D", "Estimate"],
                    confIntLow = eval(parse(
                        text = paste(yvarMod, "_IV_coefci",
                                     sep = "")
                    ))["D", "2.5 %"],
                    confIntHigh = eval(parse(
                        text = paste(yvarMod, "_IV_coefci",
                                     sep = "")
                    ))["D", "97.5 %"],
                    modName = eval(parse(text = paste(
                        "name_", mod,
                        sep = ""
                    ))),
                    stringsAsFactors = TRUE
                ), figDF
                )
        }
    }
    
    figDF
}

genMainModFigure <- function(figDf) {
    mainModFigure <- ggplot(figDf) +
        ggplot2::geom_errorbarh(aes(y = variableLabel, xmin = confIntLow,
                                    xmax = confIntHigh, color = modName,
                                    height = 0.3), position = position_dodgev(height = 0.55), alpha = 0.5) +
        geom_point(aes(x = treatmentEffect, y = variableLabel, 
                       color = modName), position = position_dodgev(height = 0.55)) +
        geom_vline(xintercept = 0, color = "firebrick3") + 
        labs(
            x = "Treatment effect (standard deviations)",
            y = ""
        ) + 
        theme(legend.title = element_blank(), legend.position = "bottom")
    
}