suppressMessages(library(cowplot, warn.conflicts = FALSE))
library(dplyr, warn.conflicts = FALSE)
library(grf)
library(haven)
library(purrr, warn.conflicts = FALSE)
library(ggplot2)

## Set options.
sampleFraction <- 0.2
B <- 50000


## Import data
df <- read_stata(bzfile("./data/final_data.dta.bz2"))
df <- df %>% filter(sample_main == 1)

## Build the covariates matrix.
X <- df %>%
    select(
        educyears,
        male,
        white,
        age,
        repdem,
    )

## Build the outcome matrix
W <- data.matrix(df$D)

## Build the treatment assignment
Z <- data.matrix(df$T)

## Functions.
genXMatrix <- function(df, X, outcome, var) {
    XOut <- cbind(X, df[paste0("index_", outcome, "_b")])
    XOut <- XOut %>%
        mutate(
            across(
                .cols = !matches(var),
                ~ mean(.x, na.rm = TRUE)
            )
        )
    XOut <- data.matrix(XOut)
}



## Political polarization index
## Age


## Drop rows with NAs
fullDfPolarize <- cbind(
    X,
    index_polarize_b = df$index_polarize_b,
    index_polarize = df$index_polarize,
    D = df$D,
    T = df$T
)
fullDfPolarize <- fullDfPolarize[complete.cases(fullDfPolarize), ]
Y_polarize <- fullDfPolarize$index_polarize
D_polarize <- fullDfPolarize$D
T_polarize <- fullDfPolarize$T

## Build X Matrix
X_age_polarize <- fullDfPolarize %>%
    select(
        educyears,
        male,
        white,
        age,
        repdem,
        index_polarize_b
    ) %>%
    mutate(
        across(
            .cols = !age,
            ~ mean(.x, na.rm = TRUE)
        )
    )

X_age_polarize <- data.matrix(X_age_polarize)  

## Run forest! (Run)
ivForest <- instrumental_forest(
    X = X_age_polarize,
    Y = Y_polarize,
    W = D_polarize,
    Z = T_polarize,
    num.trees = B,
    sample.fraction = sampleFraction,
    seed = 202007
)

tau.hat.age <- predict(
    ivForest, 
    X_age_polarize,
    estimate.variance = TRUE)

avgLATE <- average_late(ivForest)[1]

figForestOne <- 
    ggplot(
        data = data.frame(
            x = X_age_polarize[, "age"], 
            y = tau.hat.age$predictions
            ), 
        aes(x = x, y = y)
        ) + 
    geom_line(
        color = "#238b45",
        size = 1.3
            ) +
    geom_hline(yintercept = average_late(ivForest)[1],
               linetype = "dashed") +
    geom_line(aes(
        y = tau.hat.age$predictions - qnorm(0.975) * sqrt(tau.hat.age$variance.estimates)
    ),
    linetype = "dashed",
    color = "#66c2a4") +
    geom_line(aes(
        y = tau.hat.age$predictions + qnorm(0.975) * sqrt(tau.hat.age$variance.estimates)
    ),
    linetype = "dashed",
    color = "#66c2a4") + 
    labs(
        x = "Age",
        y = "CLATE"
    ) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

densityAge <-
    ggplot(data = data.frame(X_age_polarize), aes(x = age)) + 
    geom_density(color = "#238b45", fill = "#b2e2e2", alpha = 0.4) + 
    labs(
        x = "Age",
        y = "Density"
    )

figForestAge <- plot_grid(figForestOne, densityAge, nrow = 2)
    
## Age against other variables
for(mod in c("male", "white")){
    for(i in 0:1){
        if(mod == "male"){
            scaleLimit <- c(-0.75, 0.2)
            if(i == 0){
                title = "Female"
            } else {
                title = "Male"
            }
        } else {
            scaleLimit <- c(-0.6, 0.3)
            if(i == 0){
                title = "Non-White"
            } else {
                title = "White"
            }
        }
        
        Y_pol <- fullDfPolarize[fullDfPolarize[mod] == i, "index_polarize"]
        W_pol <- fullDfPolarize[fullDfPolarize[mod] == i, "D"]
        Z_pol <- fullDfPolarize[fullDfPolarize[mod] == i, "T"]
        
        ## Build X Matrix
        X_pol <- fullDfPolarize %>%
            filter(
                eval(parse(text = mod)) == !!i
            ) %>%
            select(
                !c(matches(mod), index_polarize )
            ) %>%
            mutate(
                across(
                    .cols = !age,
                    ~ mean(.x, na.rm = TRUE)
                )
            )
        ## Run forest! (Run)
        ivForest <- instrumental_forest(
            X = X_pol,
            Y = Y_pol,
            W = W_pol,
            Z = Z_pol,
            num.trees = B,
            sample.fraction = sampleFraction,
            seed = 202007
        )
        tau.hat <- predict(
            ivForest, 
            X_pol,
            estimate.variance = TRUE)
        
        ## Plots
        assign(
            paste0("figForest", toString(i)), 
                ggplot(
                    data = data.frame(
                        x = X_pol[, "age"], 
                        y = tau.hat$predictions,
                        cofhigh = tau.hat$predictions + qnorm(0.975) * sqrt(tau.hat$variance.estimates),
                        coflow = tau.hat$predictions - qnorm(0.975) * sqrt(tau.hat$variance.estimates)
                    ), 
                    aes(x = x, y = y)
                ) + 
                geom_line(
                    color = "#238b45",
                    size = 1.3
                ) +
                geom_hline(yintercept = average_late(ivForest)[1],
                           linetype = "dashed") +
                geom_hline(yintercept = avgLATE,
                           linetype = "dotted") +
                geom_line(aes(
                    y = cofhigh
                ),
                linetype = "dashed",
                color = "#66c2a4") +
                geom_line(aes(
                    y = coflow
                ),
                linetype = "dashed",
                color = "#66c2a4") + 
                labs(
                    x = "Age",
                    y = "CLATE",
                    title = title
                ) + 
                scale_y_continuous(name="CLATE", limits=scaleLimit)
                
        )
        eval(parse(text = paste0("figForest", toString(i))))
    }
    assign(
        paste0("figForest", mod),
        plot_grid(figForest0, figForest1, labels = "AUTO")
    )
}

## Subjective well-being.
## Drop rows with NAs
fullDfswb <- cbind(
    X,
    index_swb_b = df$index_swb_b,
    index_swb = df$index_swb,
    index_social_b= df$index_social_b,
    D = df$D,
    T = df$T
)
fullDfswb <- fullDfswb[complete.cases(fullDfswb), ]
Y_swb <- fullDfswb$index_swb
D_swb <- fullDfswb$D
T_swb <- fullDfswb$T

## Build X Matrix
X_swb <- fullDfswb %>%
    select(
        educyears,
        male,
        white,
        age,
        repdem,
        index_swb_b,
        index_social_b,
    ) %>%
    mutate(
        across(
            .cols = !index_swb_b,
            ~ mean(.x, na.rm = TRUE)
        )
    )

X_swb <- data.matrix(X_swb)  

## Run forest! (Run)
ivForest <- instrumental_forest(
    X = X_swb,
    Y = Y_swb,
    W = D_swb,
    Z = T_swb,
    num.trees = B,
    sample.fraction = sampleFraction,
    seed = 202007
)

tau.hat.swb <- predict(
    ivForest, 
    X_swb,
    estimate.variance = TRUE)

figForestSwb <- 
    ggplot(
        data = data.frame(
            x = X_swb[, "index_swb_b"], 
            y = tau.hat.swb$predictions
        ), 
        aes(x = x, y = y)
    ) + 
    geom_line(
        color = "#238b45",
        size = 1.3
    ) +
    geom_hline(yintercept = average_late(ivForest)[1],
               linetype = "dashed") +
    geom_line(aes(
        y = tau.hat.swb$predictions - qnorm(0.975) * sqrt(tau.hat.swb$variance.estimates)
    ),
    linetype = "dashed",
    color = "#66c2a4") +
    geom_line(aes(
        y = tau.hat.swb$predictions + qnorm(0.975) * sqrt(tau.hat.swb$variance.estimates)
    ),
    linetype = "dashed",
    color = "#66c2a4") + 
    labs(
        x = "Subjective well-being index (baseline)",
        y = "CLATE"
    ) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

densitySwb <-
    ggplot(data = data.frame(X_swb), aes(x = index_swb_b)) + 
    geom_density(color = "#238b45", fill = "#b2e2e2") + 
    labs(
        x = "Subjective well-being index (baseline)",
        y = "Density"
    )

figForestSwbFull <- plot_grid(figForestSwb, densitySwb, nrow = 2)
