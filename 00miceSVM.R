mice.impute.svm <- function (y, ry, x, wy = NULL, C = 1, scaled = TRUE,...) 
{
    #install.on.demand("kernlab", "dplyr", "devEMF", ...)
    if (is.null(wy)) 
        wy <- !ry
    xobs <- x[ry, , drop = FALSE]
    yobs <- y[ry]
    n1 <- sum(ry)
    s <- sample(n1, n1, replace = TRUE)
    doty <- y
    doty[ry] <- yobs[s]
    dotx <- x
    dotx[ry, ] <- xobs[s, , drop = FALSE]
    x <- dotx
    y <- doty
    svm.model <- ksvm(y[ry] ~ x[ry, ], type = "C-svc",
		kernel = "vanilladot", cross = 0, C = C, scaled = scaled, 
		prob.model = TRUE, kpar = list())
    p <- predict(svm.model, x[wy, ], type = "probabilities")[, 2]
    vec <- (runif(length(p)) <= p)
    vec[vec] <- 1
    if (is.factor(y)) {
        vec <- factor(vec, c(0, 1), levels(y))
    }
    vec
}
