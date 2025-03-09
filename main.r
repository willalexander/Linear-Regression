solve_linear_regression <- function(X0, Y) {
    num_parameters = dim(X)[2] + 1
    num_observations = dim(X)[1]
    if(length(Y) != num_observations)
    {
        print("Error. Number of response observations does not match number of explanatory observations.")
        return
    }

    X = cbind(rep(1, num_observations), X0)

    cat("Model:\n\n")

    cat("Y = ")
    for(i in 0:num_parameters)
    {
        cat("B", i, sep="")
        if(i == 0)
            cat(" ")
        else
            cat("x", i, " ", sep="")
        if(i < num_parameters)
            cat("+ ")
    }
    cat("+ E\n")

    # Initialise the parameter vector
    B = rep(0, num_parameters)

    cost = compute_cost(X, Y, B)
    cat("Cost: ", cost)
}

compute_cost <- function(X, Y, B) {
    sum(((X %*% matrix(B)) - Y) ^ 2)
}