test_that("simple errors for bad input", {
        dt <- mtcars
        Y <- dt$vs
        X <- cbind(rep(1, 32), dt$mpg, dt$am)
        beta <- rep(0,3)

        # Create non-numeric X matrix
        X2 <- X
        X2[1,2] <- "a"
        # Create non-compatible beta vector
        beta2 = rep(0,4)

        expect_error(optim.IRLS())
        expect_error(optim.IRLS(X2, Y, beta), "not of numeric")
        expect_error(optim.IRLS(X, Y, beta2), "not compatible")
})

test_that("warning sign shows up", {
        dt <- mtcars
        Y <- dt$vs
        X <- cbind(rep(1, 32), dt$mpg, dt$am)
        beta <- rep(0,3)

        # Create Y vector with one element being 3
        Y[1] <- 3

        expect_warning(optim.IRLS(X, Y, beta), "not consist of 0 and 1")
})

test_that("simple errors for bad input", {
        dt <- mtcars
        Y <- dt$vs
        X <- cbind(rep(1, 32), dt$mpg, dt$am)
        beta <- rep(0,3)

        # Create non-numeric X matrix
        X2 <- X
        X2[1,2] <- "a"
        # Create non-compatible beta vector
        beta2 = rep(0,4)

        expect_error(optim.BFGS())
        expect_error(optim.BFGS(X2, Y, beta), "not of numeric")
        expect_error(optim.BFGS(X, Y, beta2), "not compatible")
})

test_that("warning sign shows up", {
        dt <- mtcars
        Y <- dt$vs
        X <- cbind(rep(1, 32), dt$mpg, dt$am)
        beta <- rep(0,3)

        # Create Y vector with one element being 3
        Y[1] <- 3

        expect_warning(optim.BFGS(X, Y, beta), "not consist of 0 and 1")
})
