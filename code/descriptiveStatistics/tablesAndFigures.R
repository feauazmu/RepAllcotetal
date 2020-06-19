library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(gt)
library(tibble)
library(tidyr)

# Sample demographics.  Table 2
genTableTwo <- function(data) {
  df <- data
  varb <- c(
    "hhld_inc_under50",
    "college",
    "male",
    "white",
    "ageunder30",
    "republican",
    "democrat",
    "fb_min_wins_b"
  )
  roNames <- c(
    "Income under $50,000",
    "College"
  )


  ## The sources for the following values can be found in the footnote for
  ## Table 2

  fbCol <- setNames(c(0.4067, 0.3312, 0.7272, 0.4431, 0.2552, NA, NA, 45),
    nm = varb
  )
  usCol <- setNames(c(
    0.4209, 0.2941, 0.7397, 0.4871, 0.2142, 0.2612, 0.1952,
    NA
  ),
  nm = varb
  )

  table <- df %>%
    filter(sample_main == 1) %>%
    summarise_at(.vars = varb, .funs = mean, na.rm = TRUE) %>%
    bind_rows(fbCol, usCol) %>%
    `colnames<-`(c(
      "Income under $50,000",
      "College",
      "Male",
      "White",
      "Age under 30",
      "Republican",
      "Democrat",
      "Facebook minutes"
    )) %>%
    mutate(rowname = c(
      "sample",
      "fb",
      "us"
    )) %>%
    pivot_longer(-rowname, "variable", "value") %>%
    pivot_wider(variable, rowname) %>%
    gt(
      rowname_col = "rowname",
    ) %>%
    fmt_number(
      columns = vars(sample, fb, us),
      decimals = 2
    ) %>%
    fmt_missing(
      columns = vars(fb, us),
      missing_text = ""
    ) %>%
    cols_align(
      align = "center",
      columns = vars(sample, fb, us)
    ) %>%
    cols_label(
      variable = "",
      sample = html("<center>Impact evaluation sample <br> (1)</center>"),
      fb = html("<center>Facebook users <br> (2)</center>"),
      us = html("<center>US population <br> (3)</center>")
    ) %>%
    tab_header("Table 2—Sample Demographics") %>%
    tab_source_note(
      source_note = html("<center><i>Notes</i>: Column 1 presents average 
            demographics for the impact evaluation sample: participants who were 
            willing to accept less than $102 to deactivate Facebook for the four 
            weeks after midline and were offered <i>p</i> = $102 or <i>p</i> = 
            $0 to do so. Column 2 presents our estimate of average demographics 
            of American adults with a Facebook account. The top five numbers in 
            column 2 are inferred from a Pew Research Center (2018f) survey of 
            social media use by demographic group. The bottom number in column 2 
            (the average of 45 minutes of Facebook use per day) is approximated 
            on those basis of sources such as Facebook (2016) and Molla and 
            Wagner (2018). Column 3 presents average demographics of American 
            adults. The top five numbers are from the 2017 American Community 
            Survey (US Census Bureau 2017), and the Republican and Democrat 
            shares are from the 2016 American National Election Study (American 
            National Election Studies 2016)</center>")
    ) %>%
    tab_options(
      table.width = pct(75)
    ) %>%
    as_raw_html(inline_css = TRUE)

  table
}

## Table 3 - Survey Response and Treatment Compliance Rates
genTableThree <- function(panel, configFile) {
  startExperiment <- as.Date(configFile$metadata$dates$start_experiment,
    format = "mdy(%m, %d, %Y)"
  )

  df <- panel %>%
    filter(sample_main == 1) %>%
    mutate(
      midline_day = difftime(enddate_midline, startExperiment,
        units = "days"
      ),
      comp_endline = structure(1 - no_endline,
        label = "Completed endline survey"
      ),
      comp_postendline = structure(1 - no_postendline,
        label = "Completed post-endline survey"
      )
    )

  ### Generating extra variables:
  answer_sms <- numeric(length = nrow(df))
  days_deact <- numeric(length = nrow(df))
  num_days_checked <- numeric(length = nrow(df))

  for (row in 1:nrow(df)) {
    for (x in 7:44) {
      if (x %in% 7:42) {
        if (
          !is.na(df[row, paste("happy_sms", x, sep = "")]) |
            !is.na(df[row, paste("lonely_sms", x, sep = "")]) |
            !is.na(df[row, paste("mood_sms", x, sep = "")])
        ) {
          answer_sms[row] <- answer_sms[row] + 1
        }
      }
      if (x %in% 12:44) {
        if (
          !is.na(df[row, paste("numchecks_day", x, sep = "")]) &
            df[row, paste("numchecks_day", x, sep = "")] > 0 &
            df[row, "midline_day"] + 1 < x &
            df[row, "endline_day"] < x &
            !is.na(df[row, paste("D_day", x, sep = "")])

        ) {
          days_deact[row] <- days_deact[row] +
            df[[row, paste("D_day", x, sep = "")]][1]
          num_days_checked[row] <- num_days_checked[row] + 1
        }
      }
    }
  }

  num_days_checked <- replace(num_days_checked, num_days_checked == 0, NA)
  ## Generate the table with the new variables

  table <- df %>%
    select(T, comp_endline, comp_postendline) %>%
    add_column(answer_sms, .before = "comp_postendline") %>%
    mutate(
      answer_sms = structure(answer_sms / 36,
        label = "Share of text messages completed"
      ),
      share_deact = structure(days_deact / num_days_checked,
        label = "Share days deactivated"
      )
    )

  ## Filling the p values from the t-tests
  pValues <- rep(NA, 4)
  for (col in 2:ncol(table)) {
    pValues[col - 1] <- t.test(
      table[table$T == 0, col],
      table[table$T == 1, col]
    )$p.value
  }
  pValues <- c(
    pValues[1], NA, pValues[2], NA, pValues[3], NA, pValues[4],
    rep(NA, 2)
  )

  tableThree <- table %>%
    group_by(T) %>%
    mutate(n = n()) %>%
    summarise_all(.funs = list(
      mean = ~ mean(x = ., na.rm = TRUE),
      sd = ~ sd(x = ., na.rm = TRUE)
    )) %>%
    select(
      comp_endline_mean,
      comp_endline_sd,
      answer_sms_mean,
      answer_sms_sd,
      comp_postendline_mean,
      comp_postendline_sd,
      share_deact_mean,
      share_deact_sd,
      n_mean
    ) %>%
    arrange(c(2, 1)) %>%
    rbind(pValues) %>%
    rename(n = n_mean) %>%
    mutate(rowname = c("treatment", "control", "pvalue")) %>%
    pivot_longer(-rowname, "Variable", "value") %>%
    pivot_wider(Variable, rowname) %>%
    mutate(Variable = c(
      "Completed endline survey",
      NA,
      "Share of text messages completed",
      NA,
      "Completed post-endline survey",
      NA,
      "Share days deactivated",
      NA,
      "Observations"
    )) %>%
    gt() %>%
    fmt_number(
      columns = vars(treatment, control),
      decimals = 2,
      drop_trailing_zeros = TRUE,
      sep_mark = ","
    ) %>%
    fmt_number(
      columns = vars(treatment, control),
      rows = c(2, 4, 6, 8),
      decimals = 2,
      drop_trailing_zeros = FALSE,
      pattern = "({x})",
      sep_mark = ","
    ) %>%
    fmt_number(
      columns = vars(pvalue),
      decimals = 2,
      drop_trailing_zeros = FALSE,
      sep_mark = ","
    ) %>%
    fmt_missing(
      columns = everything(),
      missing_text = ""
    ) %>%
    cols_align(
      align = "center",
      columns = vars(treatment, control, pvalue)
    ) %>%
    cols_label(
      treatment = html("<center>Treatment <br> mean/SD <br> (1)</center>"),
      control = html("<center>Control <br> mean/SD <br> (1)</center>"),
      pvalue = html("<center><br><i>t</i>-test <i>p</i>-value<br> (3)</center>")
    ) %>%
    tab_header("Table 3—Survey Response and Treatment Compliance Rates") %>%
    tab_source_note(
      source_note = html("<center><i>Notes</i>: Columns 1 and 2 present 
            survey response and treatment compliance rates for the Treatment 
            and Control groups in the impact evaluation sample: participants who 
            were willing to accept less than $102 to deactivate Facebook for the 
            four weeks after midline and were offered <i>p</i> = $102 or 
            <i>p</i> = $0 to do so. Column 3 presents <i>p</i>-values of tests 
            of differences in response rates between the two groups.</center>")
    ) %>%
    tab_options(
      table.width = pct(75)
    ) %>%
    as_raw_html(inline_css = TRUE)

  tableThree
}

## Figure 4 - Issue Opinions by Party at Endline

genFigFour <- function(panel) {
  df <- panel %>%
    filter(
      sample_main == 1 & (democrat_polar == 1 | republican_polar == 1)
    ) %>%
    mutate(
      issue_opinion = if_else(democrat_polar == 1, -1 * issue_polar, issue_polar)
    )

  ## Get the standard deviation from the control group in issue_opinion variable
  controlSdIssOp <- sd(unlist(df[df$T == 0, "issue_opinion"]), na.rm = TRUE)

  df <- df %>% mutate(issue_opinion = issue_opinion / controlSdIssOp)

  p <- ggplot() +
    geom_density(
      data = subset(df, T == 0 & democrat_polar == 0),
      kernel = "epanechnikov",
      aes(
        x = issue_opinion,
        color = "Control Republican",
        lty = "Control Republican"
      ), 
      na.rm=TRUE
    ) +
    geom_density(
      data = subset(df, T == 1 & democrat_polar == 0),
      kernel = "epanechnikov", aes(
        x = issue_opinion,
        color = "Treatment Republican",
        lty = "Treatment Republican"
      ), 
      na.rm=TRUE
    ) +
    geom_density(
      data = subset(df, T == 0 & democrat_polar == 1),
      kernel = "epanechnikov", aes(
        x = issue_opinion,
        color = "Control Democrat",
        lty = "Control Democrat"
      ), 
      na.rm=TRUE
    ) +
    geom_density(
      data = subset(df, T == 1 & democrat_polar == 1),
      kernel = "epanechnikov", aes(
        x = issue_opinion,
        color = "Treatment Democrat",
        lty = "Treatment Democrat"
      ), 
      na.rm=TRUE
    ) +
    labs(
      x = "Issue opinions (in units of control group standard deviations)",
      y = "Density"
    ) +
    scale_color_manual("", values = c(
      "Treatment Republican" = "red",
      "Treatment Democrat" = "blue",
      "Control Republican" = "red",
      "Control Democrat" = "blue"
    )) +
    scale_linetype_manual("", values = c(
      "Treatment Republican" = "solid",
      "Treatment Democrat" = "solid",
      "Control Republican" = "longdash",
      "Control Democrat" = "longdash"
    )) +
    theme(legend.position = "bottom") +
    labs(title = "Figure 4. Issue Opinions by Party at Endline")
  
  p
}
