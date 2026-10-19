# Mathematical & Algorithmic Reference — Computational Economics

Companion to [README.md](README.md) and [source-index.md](source-index.md). These results are a **working reference**, not a substitute for verifying assumptions in the cited readings. All displayed formulas use GitHub-compatible `$$ ... $$` LaTeX; inline math uses `$...$`.

## 1. What counts as a solution?

A computational economic argument has at least four layers:

1. **Formal model:** states, actions, preferences, transitions, feasible sets, information and equilibrium or decision criterion.
2. **Mathematical justification:** existence, uniqueness, convergence conditions, identification or incentive compatibility as appropriate.
3. **Numerical implementation:** an algorithm, finite precision, discretization, approximations and verification.
4. **Empirical or policy interpretation:** which primitives are measured, calibrated, assumed or experimentally identified; which outcomes count as welfare.

A solver may satisfy layer 3 without satisfying the others. A numerical experiment is informative only conditional on the rules, populations, and data-generating process used to run it.

## 2. Numerical computation, approximation and complexity

### 2.1 Linear systems and conditioning

For $Ax=b$, a computed solution $\hat x$ has **residual** $r=b-A\hat x$. A small residual is not necessarily a small solution error if $A$ is ill-conditioned. For a compatible matrix norm,

$$
\kappa(A)=\|A\|\,\|A^{-1}\|.
$$

Under standard small-perturbation conditions, relative input perturbations may be amplified on the order of $\kappa(A)$. Compare stable QR/SVD solutions with normal-equation methods in ill-conditioned regression and network systems. For least squares, solve $\min_\beta \|X\beta-y\|_2^2$ using a numerically reliable method rather than assuming $X^{\mathsf T}X$ can safely be inverted.

### 2.2 Root finding and Newton's method

To solve $F(x)=0$, scalar Newton is

$$
x_{k+1}=x_k-\frac{F(x_k)}{F'(x_k)}.
$$

With vector-valued $F$ and Jacobian $J_F(x_k)$, solve for a step $d_k$:

$$
J_F(x_k)d_k=-F(x_k),\qquad x_{k+1}=x_k+d_k.
$$

Use a linear solver for the step rather than computing the explicit inverse. Derivatives, non-singularity, basin of attraction and line-search or bounding safeguards matter. A converged Newton iterate shows **one candidate root**, not global uniqueness or economic stability.

### 2.3 Contractions and fixed-point iteration

If a mapping $T$ on a complete metric space maps the space into itself and satisfies

$$
d(Tx,Ty)\leq q\,d(x,y),\qquad 0\leq q<1,
$$

then $T$ has a unique fixed point $x^*$ and iterates $x_{k+1}=T(x_k)$ converge to it. A standard a posteriori bound is

$$
d(x_k,x^*)\leq \frac{q}{1-q}d(x_k,x_{k-1}).
$$

Confirm that the hypotheses actually hold. Do not infer contraction simply because a few iterations appear to converge.

### 2.4 Computational complexity

For input size $n$, $O(n)$ and $O(n^2)$ refer to asymptotic resource growth, not measured seconds; practical performance also depends on constants and data layout. Exhaustive search across binary allocations of $n$ items generally involves $2^n$ possibilities. A fast approximation can be socially useful when exact optimization is infeasible; report the approximation ratio, computational budget, and assumptions.

## 3. Optimization, shadow prices and allocation

For a differentiable maximization problem with inequalities $g_j(x)\leq 0$ and suitable constraint qualifications, a convenient Lagrangian is

$$
\mathcal L(x,\lambda)=f(x)-\sum_{j=1}^m\lambda_j g_j(x),\qquad \lambda_j\geq 0.
$$

The KKT conditions at a regular local maximum include

$$
\nabla f(x^*)-\sum_j\lambda_j^*\nabla g_j(x^*)=0,
$$

$$
g_j(x^*)\leq 0,\qquad \lambda_j^*\geq 0,\qquad \lambda_j^*g_j(x^*)=0.
$$

With a concave objective and convex feasible set, appropriate regularity makes KKT sufficient for a global optimum. Shadow prices reflect the marginal value of relaxing a resource constraint **within that model**, not a universal observed market price. For integer decisions, LP relaxations can provide bounds but not necessarily implementable allocations.

## 4. Markov chains, stochastic decisions and dynamic programming

### 4.1 Markov transition and stationary distribution

For a finite-state row-stochastic transition matrix $P$,

$$
P_{ij}\geq 0,\qquad \sum_j P_{ij}=1,\qquad \mu_{t+1}=\mu_t P.
$$

An invariant distribution satisfies

$$
\mu^*=\mu^*P.
$$

Uniqueness and convergence depend on structural conditions; finite irreducible, aperiodic chains have a unique stationary distribution approached from any initial distribution. Distinguish a realized path from the evolution of a probability distribution. A sample average along one simulated path requires suitable ergodicity to estimate stationary expectations.

### 4.2 Bellman recursion

For discount factor $\beta\in(0,1)$, state $s$, feasible action $a\in\Gamma(s)$, payoff $r(s,a)$, and transition distribution $P(s'\mid s,a)$,

$$
V(s)=\max_{a\in\Gamma(s)}[r(s,a)+\beta\sum_{s'}P(s'\mid s,a)V(s')].
$$

Under finite bounded rewards and the usual discounted finite-state assumptions, the Bellman optimality operator is a contraction in sup norm with modulus $\beta$. **Value iteration** applies that operator repeatedly; **policy iteration** evaluates and improves a fixed policy. Check stopping criteria using Bellman residuals or policy accuracy, not just visual similarity of value functions.

### 4.3 Euler conditions and endogenous grids

In a standard interior, differentiable consumption-saving problem with gross return $R_{t+1}$, the Euler condition is

$$
u'(c_t)=\beta\,\mathbb E_t\left[R_{t+1}u'(c_{t+1})\right].
$$

This equality may become an inequality at borrowing limits or other binding constraints. EGM uses an invertible marginal-utility relation to infer the current resources consistent with a chosen future asset grid; the technique's applicability depends on the problem's structure. Compare simulated moments and Euler residuals across algorithms.

### 4.4 Simulation-based estimation

For model moments $m(\theta)$ and data moments $\hat m$, method of simulated moments seeks

$$
\hat\theta=\arg\min_{\theta}\,[\hat m-m_S(\theta)]^{\mathsf T}W[\hat m-m_S(\theta)].
$$

Here $m_S(\theta)$ is estimated from simulations; $W$ is a positive semidefinite weighting matrix, ideally chosen with identification and statistical efficiency in mind. Results depend on simulated sample size, random number handling, observable definitions, weighting, and whether chosen moments identify $\theta$. Nested fixed-point estimation adds a dynamic solver inside an outer estimation routine and therefore accumulates both numerical and statistical error.

## 5. Generative models and agent-based computation

An agent-based model can be written abstractly as

$$
S_{t+1}=F(S_t,\varepsilon_t;\theta,\mathcal G_t),
$$

where $S_t$ is the collection of agent states, $\mathcal G_t$ is a network or interaction structure, $\varepsilon_t$ represents randomness, and $\theta$ specifies behaviors and constraints. Aggregate statistic $Y_t=H(S_t)$ is calculated from the population. Emergence means that macro-level regularities arise from **specified interactions and local rules**, without requiring a centralized controller.

**A minimum ABM specification should state:**

- Population and attributes (including heterogeneity).
- Behavioral rules and information sets.
- Pairing, neighborhood or network structure.
- Update order: synchronous versus asynchronous.
- Randomness, initialization and burn-in/stop conditions.
- Aggregate observables, validation targets and interventions.

In a segregation model, a relocation rule based on neighbor composition can produce high aggregate segregation even if individual tolerance is not extreme. This is a *generative possibility*, not automatically an estimate of the real-world causal effect of preferences.

For Monte Carlo estimates $\hat\mu_R=R^{-1}\sum_{r=1}^R Y_r$ from independent replicates with finite variance,

$$
\mathrm{SE}(\hat\mu_R)\approx \frac{s_Y}{\sqrt{R}}.
$$

If replicates share random components, account for dependence. Compare variation between model parameterizations with variation within one parameterization.

## 6. Graphs, networks and diffusion

### 6.1 Graph and adjacency matrix

For a graph with adjacency matrix $A$, entry $A_{ij}$ denotes an edge or its weight from $i$ to $j$. In an undirected unweighted graph, node degree is $d_i=\sum_j A_{ij}$ and $A$ is symmetric. Directed networks distinguish in-degree and out-degree. Choice of direction, weights, nodes and observation window fundamentally changes economic interpretation.

### 6.2 Eigenvector and Katz–Bonacich centrality

Eigenvector centrality seeks a nonnegative vector $v$ satisfying

$$
Av=\lambda v.
$$

Under conditions permitting Perron–Frobenius reasoning, a leading eigenvector gives a centrality ranking. For a weighted adjacency matrix $A$ and $0<\alpha<1/\rho(A)$, Katz-style centrality is

$$
c=(I-\alpha A)^{-1}\mathbf 1.
$$

Here $\rho(A)$ is the spectral radius. Direction and normalization are convention-dependent. Network centrality is not necessarily causal influence; its economic interpretation depends on the specified diffusion or payoff process.

### 6.3 Random graphs and the giant component

In an Erdős–Rényi network $G(n,p)$ the expected degree is $(n-1)p$. In the sparse large-network regime $p=c/n$, the emergence of a giant connected component occurs at a threshold around $c=1$. This is an asymptotic statement, not a universal hard threshold in finite data. Homophily, clustering, preferential attachment and strategic selection can generate graphs very different from $G(n,p)$.

### 6.4 Linear consensus (DeGroot updating)

Let $W$ be a row-stochastic matrix and $x_t$ the opinion vector:

$$
x_{t+1}=Wx_t,\qquad x_t=W^t x_0.
$$

If $W$ is primitive (for example irreducible and aperiodic in the finite setting),

$$
W^t\longrightarrow \mathbf 1\pi^{\mathsf T},\qquad
\pi^{\mathsf T}W=\pi^{\mathsf T},
$$

so opinions converge to the consensus $\pi^{\mathsf T}x_0$. The influence vector $\pi$ describes asymptotic weights. This is not necessarily Bayesian learning; consensus may preserve systematic bias or misinformation.

### 6.5 Product adoption and contagion

A basic Bass diffusion specification for cumulative adoption fraction $F(t)$ is

$$
\frac{dF(t)}{dt}=(p+qF(t))(1-F(t)).
$$

Parameters $p$ and $q$ summarize external and imitation influences; a fitted S-curve alone does not prove a specific peer mechanism. For a stylized continuous-time SIS network approximation with infection rate $\tau$, recovery rate $\delta$, and adjacency matrix $A$, a linearized epidemic threshold is associated with

$$
\frac{\tau\rho(A)}{\delta}>1.
$$

This is a threshold criterion in a specified approximation, not a universal exact statement for finite stochastic epidemics or financial contagion. Financial default networks also require balance sheets, exposure magnitudes, recovery rules and feedback mechanisms.

## 7. Network formation and strategic interaction

**Pairwise stability** of an undirected link network (in a canonical transferable-or-nontransferable specification) means: (i) neither endpoint wants to unilaterally sever an existing link, and (ii) for an absent link, if one endpoint gains by forming it, the other must not strictly gain under the bilateral consent rule. Details depend on transfers, externalities, and payoff definitions.

A tractable linear-quadratic network game has payoff

$$
u_i(a)=b_i a_i-\frac{1}{2}a_i^2+\eta a_i\sum_j A_{ij}a_j.
$$

For unconstrained interior actions, the best-response system is

$$
a=b+\eta Aa.
$$

If $I-\eta A$ is invertible, the candidate equilibrium is

$$
a^*=(I-\eta A)^{-1}b.
$$

For $\eta\geq 0$ and a nonnegative $A$, a convenient sufficient condition for a unique nonnegative solution with $b\geq 0$ is $\eta\rho(A)<1$. If action constraints bind, complementarity conditions replace this simple expression. Strategic complements can magnify shocks through central network positions; strategic substitutes alter propagation patterns.

## 8. Algorithmic game theory and equilibrium inefficiency

### 8.1 Price of anarchy

For cost-minimizing agents and social cost $C(a)$, a typical pure-equilibrium price of anarchy is

$$
\mathrm{PoA}=\frac{\max_{a\in\mathrm{NE}} C(a)}{\min_{a\in\mathcal A} C(a)}.
$$

For welfare-maximization games the ratio convention is usually adjusted. Clarify whether the equilibrium concept is pure, mixed, correlated, coarse-correlated, approximate or dynamic.

**Pigou congestion example:** a unit mass chooses one of two routes: a route with latency $x$ when its load is $x$, and a route with constant latency $1$. At the nonatomic Wardrop equilibrium all traffic may use the variable-cost route, social cost $1$. The social optimum allocates $x=1/2$ to the variable route, yielding

$$
C^*=\left(\frac12\right)^2+\left(1-\frac12\right)=\frac34,
\qquad \mathrm{PoA}=\frac{4}{3}.
$$

One must specify the game and equilibrium rule; finite-player variants require separate analysis.

### 8.2 Repeated-game learning and no-regret

For repeated play with player $i$ choosing $a_{it}$, one version of **external regret** is

$$
R_i(T)=\max_{a_i}\sum_{t=1}^T u_i(a_i,a_{-i,t})-\sum_{t=1}^T u_i(a_{it},a_{-i,t}).
$$

If every player achieves average external regret $R_i(T)/T\to 0$ in a finite game, the empirical distribution of joint play approaches the **set of coarse correlated equilibria** (in the appropriate sense). This does *not* generally establish convergence of actions to a Nash equilibrium.

### 8.3 Computation of equilibria

Best-response dynamics may cycle; algorithms can converge to local/stable states rather than the desired global equilibrium. Exact Nash computation is more difficult in general than executing best responses in one iteration. Report the chosen equilibrium concept, error tolerance, runtime scaling, and how initial conditions and payoff representation affect results.

## 9. Mechanism design, auctions and matching

### 9.1 Truthful mechanisms

Under quasilinear preferences, agent $i$ with valuation $v_i(a;\theta_i)$ and payment $p_i$ obtains

$$
u_i=v_i(a;\theta_i)-p_i.
$$

Dominant-strategy incentive compatibility means truthful reporting maximizes utility for agent $i$ irrespective of others' reports, within the specified type and mechanism domains:

$$
u_i(\theta_i,\theta_i,\hat\theta_{-i})\geq
u_i(\theta_i,\hat\theta_i,\hat\theta_{-i}).
$$

**Individual rationality** compares participation utility to the opt-out alternative. **Budget balance** compares transfers collected and distributed. These requirements can conflict with allocative efficiency.

### 9.2 Vickrey and VCG

In a single-object private-values auction, the second-price sealed-bid rule allocates to the highest reported valuation and charges the winning bidder the second-highest bid. Under standard assumptions, truthful bidding is weakly dominant.

The broader Vickrey–Clarke–Groves construction selects an allocation maximizing total reported valuations:

$$
a^*(\hat\theta)\in\arg\max_{a\in\mathcal A}\sum_i v_i(a;\hat\theta_i).
$$

A canonical VCG payment for $i$ is

$$
p_i=h_i(\hat\theta_{-i})-
\sum_{j\ne i}v_j(a^*(\hat\theta);\hat\theta_j),
$$

where $h_i$ does not depend on $i$'s own reported type. For the Clarke pivot choice, $h_i$ is the maximum welfare attainable by the other agents without $i$. Exact allocation maximization may be combinatorially expensive; approximate allocation can destroy the incentive guarantees unless the mechanism is designed carefully.

### 9.3 Matching algorithms

**Deferred acceptance** repeatedly lets applicants propose to their favorite not-yet-rejecting option, while recipients tentatively retain preferred feasible proposals; the process stops after no further proposals are possible. Under standard strict preference and capacity assumptions, the appropriate deferred-acceptance variant yields a stable matching (no blocking pair), and the proposing side receives its best stable matching. Stability does not imply maximum total cardinal utility and can trade off against distributional objectives.

Matching mechanisms should be evaluated for capacity feasibility, stability, incentive properties, efficiency and fairness separately.

## 10. Computational social science: prediction versus causal evidence

Digital platform data do not automatically sample a population at random. Network links, recommended content, incentives to post, missing accounts and changing platform rules jointly determine observations. Prefer **data provenance diagrams** and explicit sampling models to unexamined assumptions that a large dataset is representative.

For intervention/policy experiments, distinguish:

- **An in-simulation treatment effect:** difference in outcomes when *you* change a parameter or rule in the executable model.
- **A causal effect in an observed population:** a counterfactual estimand identified under a real-world assignment/design argument and defensible measurement assumptions.
- **Forecasting skill:** predictive performance on a defined target distribution.

These are related but different. Agent-based models may help specify plausible mechanisms or counterfactual scenarios; econometric and experimental methods are needed to support empirical identification claims.

## 11. Verification checklist for every numerical or generative project

| Question | Minimum credible test |
|---|---|
| Did the algorithm implement the intended problem? | Hand-check toy cases; unit tests; conservation and feasibility checks |
| Does a computed solution satisfy its conditions? | FOC/KKT, Bellman, equilibrium, probability-sum or matching stability residuals |
| Are results numerical artifacts? | Grid, tolerance, step-size and solver comparison; perturbations near ill conditioning |
| Does stochastic noise dominate effects? | Multiple independent seeds; uncertainty intervals; Monte Carlo diagnostics |
| Could another mechanism generate similar observations? | Model comparison, alternative agent/network specifications, placebo statistics |
| Do reported welfare gains rely on hidden assumptions? | Explain whose utility, transfers, costs and opportunity costs are counted |
| Is the conclusion causally supported? | Explicit data design/identification argument, or qualify as conditional simulation |
| Can someone reproduce the result? | Versioned code, environment, parameters, provenance, minimal commands |

### Core mistakes to avoid

- Equating algorithmic convergence with existence or uniqueness of economic equilibrium.
- Treating eigenvector centrality as a measured causal treatment effect.
- Claiming stationarity for reducible or periodic chains without examining conditions.
- Calling agent rule changes empirical experiments without real-world identification.
- Comparing selfish and optimal allocations with inconsistent welfare accounting.
- Calling a VCG-like payment formula truthful while using a non-maximizing allocation algorithm.
- Reporting one agent-based run or one fitted network as if it captured model uncertainty.

**Recommended use:** derive results here by hand, reproduce them numerically from [README.md](README.md) exercises, then use [source-index.md](source-index.md) to locate the primary course material and its original assumptions.
