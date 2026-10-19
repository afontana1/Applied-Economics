# Mathematical reference: definitions, equations, proof obligations, and failure modes

This companion reference is organized around the results repeatedly used in the 24-week curriculum. Equations are **instructional summaries**, not assertions that every included textbook proves every theorem at its greatest generality. Always consult the corresponding reading for hypotheses and proofs.

> **GitHub math style:** `$...$` for inline mathematical notation, and standalone `$$` blocks for display formulas. Avoid unsupported custom function macros and malformed variable-size curly-brace delimiters.


**Navigation:** Follow the 24-week study schedule, computational labs, and assessment rubrics in [README.md](README.md). Consult [source-index.md](source-index.md) for the chapter in the supplied textbooks that develops each result. This reference is intentionally concise and does not replace a complete proof.

## 1. Derivatives, elasticities, and Taylor approximations

For differentiable $f$ near $x_0$,

$$
f(x_0+h)=f(x_0)+f'(x_0)h+o(h).
$$

If $f$ is twice continuously differentiable near $x_0$,

$$
f(x_0+h)=f(x_0)+f'(x_0)h+\frac{1}{2}f''(x_0)h^2+o(h^2).
$$

The point elasticity $\varepsilon_{f,x}$ of a positive differentiable function with respect to positive $x$ is

$$
\varepsilon_{f,x}=\frac{\partial f}{\partial x}\frac{x}{f(x)}.
$$

**Check:** local expansions are accurate for small changes near the expansion point; a global comparative static requires more than a local derivative.

## 2. Multivariable derivatives and compositions

For $f:\mathbb{R}^n\to\mathbb{R}$,

$$
\nabla f=\begin{pmatrix}f_{x_1}\\\vdots\\f_{x_n}\end{pmatrix},
\qquad
H_f=(f_{x_i x_j})_{i,j=1}^n.
$$

For a differentiable composition $F(G(\boldsymbol{x}))$, the Jacobian chain rule is

$$
D(F\circ G)(\boldsymbol{x})=DF(G(\boldsymbol{x}))\,DG(\boldsymbol{x}).
$$

**Check:** matrix multiplication order matters. Dimensions must match before applying the chain rule.

## 3. Implicit differentiation

Given $F(\boldsymbol{x},\boldsymbol{a})=\boldsymbol{0}$, with $D_{\boldsymbol{x}}F$ square and nonsingular, the local response is

$$
\frac{\mathrm d\boldsymbol{x}}{\mathrm d\boldsymbol{a}}
=-\bigl(D_{\boldsymbol{x}}F\bigr)^{-1}D_{\boldsymbol{a}}F.
$$

**Proof outline:** Differentiate $F(\boldsymbol{x}(\boldsymbol{a}),\boldsymbol{a})=\boldsymbol{0}$; apply the chain rule and solve the resulting linear system.

**Failure mode:** a zero determinant invalidates this particular local formula; it need not imply that no equilibria exist.

## 4. Linear systems, rank, and least squares

For a linear system $A\boldsymbol{x}=\boldsymbol{b}$, uniqueness requires that the null space contain only zero, assuming a solution exists. For square matrices, full rank guarantees a unique solution for every $\boldsymbol{b}$.

Least squares minimizes

$$
L(\boldsymbol{\beta})=(\boldsymbol{y}-X\boldsymbol{\beta})^{\mathsf T}
(\boldsymbol{y}-X\boldsymbol{\beta}),
$$

with first-order condition

$$
X^{\mathsf T}X\hat{\boldsymbol{\beta}}=X^{\mathsf T}\boldsymbol{y}.
$$

If $X$ has full column rank, the minimizer is unique. In numerical computation, QR or SVD-based solvers are often preferable to explicitly computing $(X^{\mathsf T}X)^{-1}$.

**Failure mode:** linear algebraic rank and econometric exogeneity are separate issues.

## 5. Quadratic forms, eigenvalues, and local curvature

The quadratic form is $Q(\boldsymbol{x})=\boldsymbol{x}^{\mathsf T}A\boldsymbol{x}$. Its value depends only on the symmetric part of $A$.

For symmetric $A$,

$$
A\succ0\quad\Longleftrightarrow\quad
\boldsymbol{x}^{\mathsf T}A\boldsymbol{x}>0
\ \text{for every nonzero }\boldsymbol{x}
\quad\Longleftrightarrow\quad
\lambda_i(A)>0\ \text{for all }i.
$$

A twice-differentiable function with negative-definite Hessian at an interior stationary point has a strict local maximum there. Concavity throughout a convex domain can support a global result.

**Failure mode:** a positive-semidefinite Hessian does not ensure a strictly unique minimum.

## 6. Equality-constrained optimization and multipliers

Consider

$$
\max_{\boldsymbol{x}} f(\boldsymbol{x})
\qquad \text{subject to}\qquad g(\boldsymbol{x})=b.
$$

With $\mathcal L=f+\lambda(b-g)$, the regular interior first-order conditions are

$$
\nabla f(\boldsymbol{x}^{\ast})=\lambda\nabla g(\boldsymbol{x}^{\ast}),
\qquad g(\boldsymbol{x}^{\ast})=b.
$$

Under suitable regularity,

$$
\frac{\mathrm d V(b)}{\mathrm db}=\lambda(b),
\qquad V(b)=\max_{g(\boldsymbol{x})=b}f(\boldsymbol{x}).
$$

**Failure mode:** the multiplier is a derivative of the optimized value under the stated conventions, not automatically a literal observed market price.

## 7. Inequality constraints (KKT)

For the maximization problem $h_i(\boldsymbol{x})\le0$ with multipliers $\lambda_i\ge0$, adopt

$$
\mathcal L(\boldsymbol{x},\boldsymbol{\lambda})=f(\boldsymbol{x})-\sum_i\lambda_i h_i(\boldsymbol{x}).
$$

At an appropriately regular local optimum, KKT conditions include

$$
\nabla f(\boldsymbol{x}^{\ast})-\sum_i\lambda_i\nabla h_i(\boldsymbol{x}^{\ast})=0,
$$

$$
h_i(\boldsymbol{x}^{\ast})\le0,
\qquad \lambda_i\ge0,
\qquad \lambda_i h_i(\boldsymbol{x}^{\ast})=0.
$$

**Failure modes:** without regularity, first-order conditions can fail; without appropriate concavity/convexity, satisfying KKT need not guarantee a global optimum.

## 8. Homogeneity and Euler's theorem

If $f(t\boldsymbol{x})=t^k f(\boldsymbol{x})$ for positive $t$, and $f$ is differentiable,

$$
\boldsymbol{x}^{\mathsf T}\nabla f(\boldsymbol{x})=k f(\boldsymbol{x}).
$$

**Proof outline:** differentiate both sides with respect to $t$ and substitute $t=1$. Distinguish homogeneity of a function from homotheticity of preferences.

## 9. Existence by compactness

If $K$ is a nonempty compact subset of $\mathbb{R}^n$ and $f:K\to\mathbb{R}$ is continuous, then $f$ attains a maximum and a minimum on $K$ (extreme value theorem). This is an *existence* theorem; it does not establish uniqueness, optimality conditions, or comparative statics.

**Proof exercise:** show that $f(x)=x$ has no maximum on the open interval $(0,1)$ even though it is continuous there.

## 10. Linear scalar ODE

For $\dot{x}=a-bx$ and $b>0$,

$$
x^{\ast}=\frac{a}{b},\qquad
x(t)=x^{\ast}+(x_0-x^{\ast})e^{-bt}.
$$

The process approaches $x^{\ast}$ as $t\to\infty$.

For explicit Euler at time step $h$,

$$
x_{k+1}=x_k+h(a-bx_k).
$$

The Euler iterates converge to $x^{\ast}$ if $0<hb<2$. Thus even a stable continuous model can be destabilized by an overly large numerical step.

## 11. Vector dynamical systems

In continuous time,

$$
\dot{\boldsymbol{x}}=A\boldsymbol{x}
\quad\Longrightarrow\quad
\boldsymbol{x}(t)=e^{At}\boldsymbol{x}_0.
$$

For asymptotic stability of a linear homogeneous continuous-time system, every eigenvalue of $A$ must have strictly negative real part. For the discrete system $\boldsymbol{x}_{t+1}=A\boldsymbol{x}_t$, asymptotic stability requires every eigenvalue to have modulus strictly less than one.

**Failure mode:** the stability criteria are **not** interchangeable between discrete and continuous time.

## 12. Probability and uncertainty

Bayes' rule, for events with $\mathbb P(B)>0$, is

$$
\mathbb{P}(A\mid B)=\frac{\mathbb{P}(B\mid A)\mathbb{P}(A)}{\mathbb{P}(B)}.
$$

The law of total variance, when second moments exist, is

$$
\mathrm{Var}(X)=\mathbb{E}[\mathrm{Var}(X\mid Y)]
+\mathrm{Var}(\mathbb{E}[X\mid Y]).
$$

For a weighted sum,

$$
\mathrm{Var}\left(\sum_i w_i X_i\right)
=\sum_i\sum_j w_i w_j\mathrm{Cov}(X_i,X_j).
$$

**Failure mode:** summing variances while ignoring covariance is correct only under suitable independence or zero-covariance assumptions.

## 13. Markov transition matrices

For a row-stochastic matrix $P$ and a row vector $\boldsymbol{\pi}_t$,

$$
\boldsymbol{\pi}_{t+1}=\boldsymbol{\pi}_tP.
$$

A stationary distribution satisfies $\boldsymbol{\pi}^{\ast}P=\boldsymbol{\pi}^{\ast}$. Existence, uniqueness, and convergence depend on the structure of the chain; do not confuse these claims.

## 14. Shannon entropy

For a finite distribution $\boldsymbol{p}$,

$$
H(\boldsymbol{p})=-\sum_{i=1}^n p_i\log p_i,
$$

interpreting $0\log0=0$. Entropy measures uncertainty under the selected probability model; it does not by itself measure an economic loss or physical volatility.

## 15. Fourier approximation

For a sufficiently regular periodic function $f$ on $[-\pi,\pi]$, the Fourier coefficient formulas are

$$
a_n=\frac{1}{\pi}\int_{-\pi}^{\pi}f(x)\cos(nx)\,\mathrm dx,
\qquad
b_n=\frac{1}{\pi}\int_{-\pi}^{\pi}f(x)\sin(nx)\,\mathrm dx.
$$

Use convergence conditions when claiming pointwise or uniform accuracy of truncated expansions.

## 16. Newton iteration and numerical reliability

For a scalar root problem $f(x)=0$,

$$
x_{k+1}=x_k-\frac{f(x_k)}{f'(x_k)},
$$

when $f'(x_k)\ne0$. Local quadratic convergence requires additional smoothness and a suitable starting point near a simple root.

**Failure modes:** flat derivatives, remote starts, discontinuities, noisy evaluations, and ill-conditioned root problems.

## Proof and model quality checklist

For each theorem or derived implication:

1. **State the domain and hypotheses**, including continuity/differentiability, matrix rank, positivity, independence, and constraint regularity.
2. **Separate necessity from sufficiency.** An FOC may identify candidate solutions only.
3. **State locality versus globality.** A local derivative, inverse, or stability calculation may not extend globally.
4. **Check dimensions and units.** Gradients/Jacobians, multipliers, rates, and time steps must be consistent.
5. **Test a counterexample.** If removing an assumption invalidates the conclusion, show how.
6. **Verify computation.** Report residuals, sensitivity to tolerances, and random-seed conventions.
7. **Interpret carefully.** A mathematical association, equilibrium, or forecast is not in itself a causal identification claim.
