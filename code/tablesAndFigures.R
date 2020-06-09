library(dplyr, warn.conflicts = FALSE)
library(gt)
library(tidyr)

# Sample demographics.  Table 2
genTableTwo <- function(data) {
    df <- data
    varb <- c("hhld_inc_under50", 
              "college", 
              "male", 
              "white", 
              "ageunder30",
              "republican",
              "democrat",
              "fb_min_wins_b")
    roNames <- c("Income under $50,000",
                 "College")

    
    ## The sources for the following values can be found in the footnote for 
    ## Table 2
    
    fbCol <- setNames(c(0.4067, 0.3312, 0.7272, 0.4431, 0.2552, NA, NA, 45), 
                      nm = varb)
    usCol <- setNames(c(0.4209, 0.2941, 0.7397, 0.4871, 0.2142, 0.2612, 0.1952, 
                       NA), 
                      nm = varb)
    
    table <- df %>% 
        filter(sample_main == 1) %>%
        summarise_at(.vars = varb, .funs = mean, na.rm=TRUE) %>%
        bind_rows(fbCol, usCol) %>% 
        `colnames<-`(c("Income under $50,000",
                       "College",
                       "Male",
                       "White",
                       "Age under 30",
                       "Republican",
                       "Democrat",
                       "Facebook minutes")) %>%
        mutate(rowname = c("sample", 
                           "fb",
                           "us")) %>%
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