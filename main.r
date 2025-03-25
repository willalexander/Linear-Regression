solve_linear_regression <- function(X, Y) {
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
    cat("Cost: ", cost, "\n")

    # Derive the normal equations, represented as a matrix:
    normal_eqns = derive_normal_equations(X, Y, B)
    cat("\nNormal Equations:\n")
    print(normal_eqns)
}

compute_cost <- function(X, Y, B) {
    sum(((X %*% matrix(B)) - Y) ^ 2)
}

derive_normal_equations <- function(X, Y, B) {
    num_parms = dim(X)[2] 
    num_obs = dim(X)[1]

    mat = matrix(nrow=num_parms, ncol=num_obs)
    for(j in 1:num_parms)
    {
        for(i in 1:num_obs)
        {
            hat_sum = 0.0
            for(j_ in 1:num_obs) 
                hat_sum = hat_sum + B[j_]*X[i, j_]
            mat[j, i] = (hat_sum - Y[i]) * X[i, j]
        }
    }
    return(mat)
}

X = rbind(
    c(1, 3, -2),
    c(3, 5,  6),
    c(2, 4,  3)
)
Y = c(5, 7, 8)

sln = c(-15, 8, 2)

solve_linear_system <- function(X, Y) {
    augmented = cbind(X, Y)
    colnames(augmented) <- NULL
    print(augmented)

    for(j in 1:(dim(X)[2] - 1)) {
        for(i in (j + 1):dim(X)[1]) {
            factor = augmented[i, j] / augmented[j, j]
            augmented[i, ] = augmented[i, ] - factor * augmented[j,]
        }
        print(augmented)
    }

    cat("### Echelon Form ###\n")

    for(j in dim(X)[1]:2) {
        for(i in (j - 1):1) {
            factor = augmented[i, j] / augmented[j, j]
            augmented[i,] = augmented[i,] - factor * augmented[j,]
        }
        print(augmented)
    }

    cat("### Solution Form ###\n")
    solution = rep(0, dim(X)[1])
    for(i in 1:dim(X)[1])
        solution[i] = augmented[i, dim(X)[1] + 1] / augmented[i, i]
    print("Solution:")
    print(solution)

    cat("Product: ", c(X %*% solution), "\n")
    cat("Expected product: ", Y, "\n")
}

