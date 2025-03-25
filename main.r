generate_data <- function(N, intercept, slope, sd) {
    # Simulate 2D linear point data with added Normal error term
    X = matrix(runif(N))

    error = rnorm(N, mean=0, sd=sd)

    Y = intercept + slope * X + error

    result = cbind(X, Y)
    return(result)
}

display_regression_model <- function(X, Y) {
    # Interpret and print the mathematical regression model.
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

    # Derive the normal equations, represented as a matrix:
    normal_eqns = derive_normal_equations(X, Y, B)
    cat("\nNormal Equations:\n")
    print(normal_eqns)
}

derive_normal_equations <- function(X, Y) {
    # Compute least squares loss function and differentiate to 
    # yield the normal equations
    num_parms = dim(X)[2] 
    num_obs = dim(X)[1]
    
    mat = matrix(nrow=num_parms, ncol=num_parms)
    prods = rep(0, num_parms)
    for(j in 1:num_parms) {
        for(k in 1:num_parms) {
            coeff = 0
            for(i in 1:num_obs)
                coeff = coeff + X[i, k] * X[i, j]
            mat[j, k] = coeff
        }
        prod = 0
        for(i in 1:num_obs)
            prod = prod + Y[i] * X[i, j]
        prods[j] = prod
    }

    return(cbind(mat, prods))
}

solve_linear_system <- function(X, Y) {
    # Solve linear system of equations
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

    return(solution)
}

draw_fit <- function(B) {
    # Draw the fitted regression line
    fitted_line <- function(x) {
        B[1] + B[2] * x
    }
    curve(fitted_line, from=0, to=1, add=TRUE, lwd=2, col="blue")
}

fit_regression <- function(X, Y) {
    # Fit linear regression to explanatory & response variables
    tmp = derive_normal_equations(X, Y)
    system_coeffs = tmp[,1:dim(X)[2]]
    system_prods = tmp[, dim(X)[2] + 1]

    B = solve_linear_system(system_coeffs, system_prods)
    return(B)
}

num_features = 1
data = generate_data(100, 0.2, 0.6, 0.1)
X = cbind(rep(1, dim(data)[2]), data[,1:num_features])
Y = data[,num_features + 1]
plot(X[,2], Y, col="green", pch=19)

B = fit_regression(X, Y)
draw_fit(B)