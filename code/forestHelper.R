
genFullMatrix <- function(df, X, outcome, varAcross) {
    ## Drop rows with NAs
    fullDf <- cbind(X,
                    df[, paste0("index_", outcome, "_b")],
                    df[, paste0("index_", outcome)],
                    D = df$D,
                    T = df$T)
    fullDf <- fullDf[complete.cases(fullDf), ]
    
    ## Build X Matrix
    meanDf <- fullDf %>%
        mutate(across(
            .cols = !c(
                matches(paste0("^index_", outcome, "$")), 
                matches(varAcross),
                D,
                T
                ),
        ~ mean(.x, na.rm = TRUE)))
    
    fullMatrix <- data.matrix(meanDf)
    fullMatrix
}

genFigForest <-
    function(X,
             varAcross,
             ivForest,
             tau.hat,
             x.label,
             hide.x.axis = TRUE,
             title = NULL,
             y.scale = NULL,
             avg.late = NULL
            ) {
        figForest <-
            ggplot(data = data.frame(x = X[, varAcross],
                                     y = tau.hat$predictions),
                   aes(x = x, y = y)) +
            geom_line(color = "#238b45",
                      size = 1.3) +
            geom_hline(yintercept = average_late(ivForest)[1],
                       linetype = "dashed") +
            geom_line(
                aes(
                    y = tau.hat$predictions - qnorm(0.975) * sqrt(tau.hat$variance.estimates)
                ),
                linetype = "dashed",
                color = "#66c2a4"
            ) +
            geom_line(
                aes(
                    y = tau.hat$predictions + qnorm(0.975) * sqrt(tau.hat$variance.estimates)
                ),
                linetype = "dashed",
                color = "#66c2a4"
            ) +
            labs(x = x.label,
                 y = "CLATE")
        if (hide.x.axis) {
            figForest <- figForest +
                theme(
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                )
        }
        
        if(!is.null(avg.late)){
            figForest <- figForest +
                geom_hline(yintercept = avg.late,
                           linetype = "dotted")
        }
        
        if(!is.null(y.scale)){
            figForest <- figForest +
                scale_y_continuous(name="CLATE", limits=y.scale)
        }
        
        if(!is.null(title)){
            figForest <- figForest +
                labs(title = title)
        }
        
        figForest
        
    }
