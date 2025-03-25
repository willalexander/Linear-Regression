### From-scratch implementation of Linear Regression in R.

<img src="/images/plots.png" width=75%/>

Uses Least-squares regression to fit a linear function to provided explanatory & response variables.

### Mathematical model:


$$ y_i = B_0 + B x_i + ε $$

$$ ε \sim N(0, σ^2) $$

* Represent the problem via design matrix and response vector:

$$\left[\begin{array}{ccc}
x11 & x12 \\
x21 & x22
\end{array}\right]
\left[\begin{array}{ccc}
y1 \\
y2
\end{array}\right]
$$

* Fit the regression line by minimising the least-squares loss function:

$$ L = \sum(y_i - \hat{y})^2  = \sum(y_i - B_0 - BX_i)^2 $$

* Differentiate loss function with respect to B to form the normal equations

* Solve the normal equations using Gaussian Elimination.
