# ECON 233 — Finance I

## Asset pricing, portfolio construction, and financial markets

**Purpose.** Learn how to measure financial returns, characterize risk, construct efficient portfolios, understand asset pricing models, assess market efficiency and make defensible investment decisions. This guide is grounded in the supplied UCSC ECON 233 description: risky/intertemporal choice, asset pricing, efficient markets, institutions, derivatives; and the supplied coverage list: CAPM, Markowitz, Sharpe ratio, EAR, return measurement, leverage, diversification and portfolio optimization. The topic sequence, examples and project exercises are **designed for independent study**, not asserted to be actual university assignments. No specific Finance I textbook, lecture notes or problem sets were provided.

[← Return to Graduate Finance](../README.md) · [Corporate Finance](../ECON-235-Corporate-Finance/README.md) · [Financial Engineering](../ECON-236-Financial-Engineering/README.md) · [Risk-management playbook](../risk-management-playbook.md)

**Quarter format:** 12 weeks, approximately 10–14 hours/week, with weekly mathematical exercises, computational labs, applied portfolio research and a risk checkpoint. The 12-week teaching plan and advanced allocation methods appear after the original core modules.

**Navigation:** [Jump to the 12-week course schedule](#twelve-week-teaching-and-assessment-plan) · [Shared risk-management playbook](../risk-management-playbook.md) · [Program knowledge map](../README.md#knowledge-map-concept-dependencies)

## Outcomes: what mastery looks like

By the end, you should be able to:

- Translate quoted interest rates into comparable time-scaled returns and use cash-flow timing correctly.
- Distinguish expected vs realized, arithmetic vs geometric, nominal vs real, price vs total, and simple vs continuously compounded returns.
- Explain risk aversion, marginal utility, diversification, covariance, beta and why correlation matters more than standalone variance to a diversified investor.
- Derive and solve the two-asset and general mean–variance portfolio problems, with and without constraints.
- Derive the intuition and central relationships behind the capital allocation line, market portfolio, CAPM and security market line; articulate their assumptions and limits.
- Assess portfolio performance using risk-adjusted metrics and benchmark attribution, avoiding misleading comparisons.
- Interpret weak, semi-strong and strong efficiency as information-set claims, not guarantees of rational or frictionless markets.
- Prepare a concise investment policy and evidence-based portfolio recommendation, including estimation uncertainty and implementation frictions.

## Prerequisites and mathematical toolset

**Required:** basic microeconomics, risk and intertemporal choice, algebra, derivatives, unconstrained/constrained optimization, probability, expectations/variance, elementary statistics and matrix notation. **Useful:** linear algebra (positive-semidefinite matrices, matrix inverses), regressions and a programming language for portfolio calculations. A newcomer from math or OR may need more time on financial instrument mechanics; an economics reader may need more time on matrix optimization.

### Compact notation and units

| Symbol | Meaning | Beware |
|---|---|---|
| $P_t$ | Ex-dividend asset price at time $t$ | Specify valuation timestamp and currency |
| $D_t$ | Cash distribution between $t-1$ and $t$ | Include dividends when computing total return |
| $R_t$ | Simple holding-period total return | 1-month and 1-year returns are not directly comparable |
| $r_f$ | Risk-free return matched to horizon/currency | A proxy security can still carry reinvestment or price risk |
| $\mu$, $\Sigma$ | Expected return vector and covariance matrix | Estimated from noisy samples |
| $\mathbf w$ | Portfolio weights | State whether short sales and leverage are permitted |
| $\beta_i$ | Market beta $Cov(R_i,R_M)/Var(R_M)$ | Beta is not total risk |

## Learning sequence and dependency graph

```mermaid
flowchart LR
 A[Cash flows, compounding] --> B[Returns & risk statistics]
 B --> C[Preferences & expected utility]
 B --> D[Covariance & diversification]
 C --> E[Mean-variance allocation]
 D --> E
 E --> F[CAPM and asset pricing]
 F --> G[Performance & benchmarking]
 F --> H[Market efficiency & institutions]
 G --> I[Portfolio implementation]
 H --> I
 I --> J[Investment analysis capstone]
```

### Module 1 — Financial instruments, markets, and intertemporal choice

**Questions.** What claims do debt and equity represent? What is a cash flow worth today? Why can identical cash-flow totals have different values?

**Concepts:** equity vs debt, securities and financial intermediaries, spot vs forward markets, primary vs secondary markets, prices versus cash flows, arbitrage in simple settings, nominal vs real values, risky and intertemporal optimization, consumption smoothing, liquidity and market institutions. Clarify the distinct role of cash-flow timing and risk preferences.

**Core derivations:** For discrete-period cash flows, the present value is

$$
PV=\sum_t \frac{CF_t}{(1+i)^t}.
$$

For intertemporal choice, illustrate the Euler equation in a simplified setting with suitable assumptions:

$$
u'(c_t)=\beta(1+r)E_t\!\left[u'(c_{t+1})\right].
$$

Compare equivalent payment streams and explain why timing and the discount rate matter.

**Application:** compare two loans offering different payment timings; identify the market institution that enables borrowing and lending across periods.

**Mastery check:** Explain why two investors with different consumption needs might choose different assets even when they agree on probabilities and prices.

### Module 2 — Interest-rate and return mathematics

**Concepts:** simple versus compound interest, quoted APR, effective annual rate (EAR), continuous compounding, discount factors, holding-period returns, time-weighted and money-weighted returns, internal rate of return, reinvestment assumptions, nominal/real return approximation (Fisher relation), dividend/total return adjustments.

**Relationships:** For an APR convertible $m$ times per year, the effective annual rate is

$$
EAR=\left(1+\frac{APR}{m}\right)^m-1.
$$

The accumulation factor for continuous rate $r_c$ over $T$ years is

$$
A(T)=e^{r_cT}.
$$

For an asset with ex-dividend prices $P_t$ and a period dividend $D_t$, holding-period and log returns are

$$
\begin{aligned}
R_t &= \frac{P_t-P_{t-1}+D_t}{P_{t-1}},\\
r_{\log,t} &= \ln(1+R_t).
\end{aligned}
$$

Multi-period gross returns compound multiplicatively:

$$
1+R_{total}=\prod_{t=1}^{T}(1+R_t).
$$

The arithmetic mean of periodic returns is not the compounded growth rate.

**Application:** create a spreadsheet comparing investments with the same quoted rate but differing compounding frequencies, deposits, fees and dividends. Calculate both time-weighted and money-weighted performance when capital flows occur midperiod.

**Mastery check:** Explain why averaging annual returns arithmetically generally misstates compound wealth growth.

### Module 3 — Measuring uncertainty and risk preferences

**Concepts:** discrete states and probabilities, expected return, variance, standard deviation, covariance, correlation, downside tail risk, skewness, kurtosis, risk aversion, utility curvature, certainty equivalent and risk premium. Distinguish **risk** under modeled probabilities from unmodeled uncertainty and estimation error.

**Relationships:** Expected return, variance, and Arrow–Pratt absolute risk aversion are

$$
\begin{aligned}
E[R] &= \sum_s p_s R_s,\\
\operatorname{Var}(R) &= E\!\left[(R-E[R])^2\right],\\
A(W) &= -\frac{u''(W)}{u'(W)}.
\end{aligned}
$$

Under small-risk approximations, the risk premium is related to variance and utility curvature; do not present a local approximation as a general equality.

**Application:** compare a sure payoff with a lottery under linear, log and CRRA utility; calculate certainty equivalents where possible. Explain why a utility-maximizing decision is not determined by expected cash value alone.

**Mastery check:** Why is a high-volatility asset not necessarily unattractive if its returns are negatively correlated with the investor's other holdings?

### Module 4 — Diversification and covariance structure

**Concepts:** systematic and idiosyncratic risk, diversification, covariance matrices, imperfect correlation, portfolio return distributions, correlation versus causality, concentration risk, sector/factor exposures.

**Relationships:** For portfolio weights $\mathbf w$, expected returns $\boldsymbol\mu$, and covariance matrix $\Sigma$,

$$
\begin{aligned}
E[R_p] &= \mathbf w^\top\boldsymbol\mu,\\
\sigma_p^2 &= \mathbf w^\top\Sigma\mathbf w,\\
\mathbf 1^\top\mathbf w &= 1.
\end{aligned}
$$

For two assets, with weight $w$ in asset 1 and $1-w$ in asset 2,

$$
\sigma_p^2=w^2\sigma_1^2+(1-w)^2\sigma_2^2+2w(1-w)\sigma_1\sigma_2\rho_{12}.
$$

**Application:** build a two-asset chart varying correlation from $-1$ to $+1$. Then move to 5–10 assets, construct an estimated correlation heat map and compare equal-weight vs concentration-heavy allocations.

**Mastery check:** Derive the minimum-variance weight for two risky assets without constraints, then explain why unrestricted optimization may suggest a short position.

### Module 5 — Markowitz portfolio optimization

**Concepts:** feasible set, efficient frontier, global minimum-variance portfolio, expected-return targeting, constrained versus unconstrained solutions, long-only and leverage constraints, regularization and estimation risk **[Extension]**.

**Optimization templates:**

$$
\min_{\mathbf w}\; \mathbf w^\top\Sigma\mathbf w\quad\text{s.t.}\quad\mathbf1^\top\mathbf w=1,\;\boldsymbol\mu^\top\mathbf w=\mu_*,
$$

An alternative mean–variance objective, subject to realistic constraints, is

$$
\max_{\mathbf w}\;\boldsymbol\mu^\top\mathbf w-\frac{\lambda}{2}\mathbf w^\top\Sigma\mathbf w.
$$

For the unrestricted global-minimum-variance portfolio, when $\Sigma$ is invertible,

$$
\mathbf w_{GMV}=\frac{\Sigma^{-1}\mathbf 1}{\mathbf 1^\top\Sigma^{-1}\mathbf 1}.
$$

**Application:** solve with a spreadsheet optimizer and with SciPy/cvxpy. Compare the theoretical frontier to one that includes minimum/maximum positions, transaction costs and no-short-selling constraints. Report sensitivity to small changes in estimated means.

**Mastery check:** Explain why a mathematically optimal portfolio can be unstable and practically inferior to a simple diversified benchmark.

### Module 6 — Risk-free lending, borrowing, and the Sharpe ratio

**Concepts:** capital allocation line, tangency portfolio, separation intuition, borrowing at a risky lending spread, leverage, margin constraints, risk-free cash benchmark, Sharpe ratio.

**Relationships:** The Sharpe ratio, using matched time horizons, is

$$
\operatorname{Sharpe}=\frac{E[R_p]-r_f}{\sigma_p}.
$$

The capital allocation line, for positive exposure to a risky portfolio, is

$$
E[R_c]=r_f+\frac{\sigma_c}{\sigma_p}\left(E[R_p]-r_f\right).
$$

Interpret these measures cautiously for non-normal, asymmetric or illiquid return distributions.

**Application:** compare two funds with different nominal returns but different volatility; assess whether leverage raises expected returns while also raising losses and financing costs.

**Mastery check:** A portfolio reports an impressive Sharpe ratio. What additional information (sample window, leverage, drawdowns, tails, illiquidity) is needed before concluding it is a superior investment?

### Module 7 — CAPM and equilibrium asset pricing

**Concepts:** market portfolio, systematic risk, beta, security market line, equity risk premium, Jensen alpha, assumptions underlying equilibrium, comparison with APT and multi-factor models **[Extension]**.

**Relationships:** Market beta and CAPM expected return are

$$
\begin{aligned}
\beta_i &= \frac{\operatorname{Cov}(R_i,R_M)}{\operatorname{Var}(R_M)},\\
E[R_i] &= r_f+\beta_i\left(E[R_M]-r_f\right).
\end{aligned}
$$

An estimated alpha (with sample mean returns and an estimated beta) is

$$
\alpha_i=\bar R_i-\left[r_f+\hat\beta_i(\bar R_M-r_f)\right].
$$

Interpret alpha subject to estimation error and the benchmark's assumptions.

**Application:** regress an asset's excess return on a broad market excess return; report intercept, beta, confidence intervals, residual variation and results across windows. Relate estimated cost of equity to ECON 235, but do not assume a historical beta is automatically the correct project discount rate.

**Mastery check:** Can an asset have high total volatility and low CAPM beta? Why does that matter for pricing versus portfolio-level risk management?

### Module 8 — Asset-pricing tests, market efficiency, and institutions

**Concepts:** weak/semi-strong/strong information efficiency, random-walk intuition versus martingale claims, market anomalies, joint-hypothesis problem, index funds, mutual funds, ETFs, brokerage, short-selling frictions, market liquidity, bid–ask spreads and price discovery.

**Application:** choose an apparently exploitable trading signal. List the information available at decision time, transaction costs, potential look-ahead bias, market impact, survivorship bias and a proper out-of-sample test.

**Mastery check:** Does evidence of abnormal returns necessarily refute market efficiency, or could the expected-return model be misspecified?

### Module 9 — Performance measurement and investment management

**Concepts:** total vs active return, benchmark selection, tracking error, information ratio, Treynor ratio, Jensen alpha, portfolio turnover, drawdown and downside statistics, factor attribution, rebalancing, taxation and fees.

**Key relationships:** Information and Treynor ratios are

$$
\begin{aligned}
IR &= \frac{E[R_p-R_b]}{\operatorname{SD}(R_p-R_b)},\\
\operatorname{Treynor} &= \frac{E[R_p]-r_f}{\beta_p}.
\end{aligned}
$$

Treynor's ratio requires economically meaningful beta; information ratios require consistent periods and robust estimates. Both are unstable when their denominators are near zero.

**Application:** build an investment manager comparison using identical dates, currencies, dividends and fee treatments. Show how rankings change when different benchmarks or volatility estimates are used.

**Mastery check:** Distinguish an investment with good standalone Sharpe ratio from one with strong *incremental* value to an existing portfolio.

### Module 10 — Portfolio policy, leverage, and introductory derivatives

**Concepts:** portfolio mandates, liability-aware investing, investment horizon, liquidity budgets, rebalancing bands, diversification across asset classes, leverage and borrowing constraints, introduction to forward/futures/option payoffs, derivative exposure versus ownership.

**Application:** produce an investment policy statement containing objective, risk budget, eligible assets, concentration/borrowing restrictions, benchmark, rebalancing and monitoring plan. Give an example of using a future for exposure management and distinguish that from an option's nonlinear insurance-like payoff.

**Mastery check:** A portfolio's expected return and Sharpe ratio improve with leverage in a frictionless model. Identify at least five real-world constraints that could make the strategy inappropriate.

## Expanded quantitative methods and CFA-style portfolio practice

The original ten modules give the theoretical sequence. The following material turns them into a **full twelve-week course** rather than a condensed survey. Topics explicitly named in the ECON 233 description remain the conceptual core; multi-factor testing, modern allocation methods, investment policy design, alternative assets, and enhanced risk analytics are **deliberate additions for a broader CFA-style self-study path**, not claims about a historical UCSC syllabus. This is **not a CFA exam-preparation guarantee**.

### Portfolio optimization methods: choose the objective before the solver

Start with an investable universe, aligned total-return histories, a covariance estimate, and explicit eligibility, liquidity, gross leverage, turnover, and position limits. A realistic optimization must distinguish **estimation of inputs**, **choice of objective**, and **execution of the resulting trades**. Compare each method against both a 1/N portfolio and a policy benchmark:

| Method | Optimization or construction rule | When it helps | Main failure mode to investigate |
|---|---|---|---|
| Equal weight | $w_i=1/n$ | Transparent, estimation-light baseline | Concentration in correlated risk factors; high rebalance turnover |
| Minimum variance | Minimize $\mathbf w^\top\Sigma\mathbf w$ | Defensive allocation, uncertain return forecasts | Covariance error, factor concentration, unstable short positions |
| Mean–variance / maximum utility | Maximize $\boldsymbol\mu^\top\mathbf w-\lambda\mathbf w^\top\Sigma\mathbf w/2$ | Explicit return–risk preference | High sensitivity to estimated expected returns |
| Maximum Sharpe | Maximize expected excess return divided by volatility, subject to admissible weights | Tangency/allocation analysis | Unstable if forecasts and lending/borrowing rates are unreliable |
| Inverse volatility | $w_i\propto1/\sigma_i$ | Simple volatility-based heuristic | Ignores correlation and return expectations |
| Equal risk contribution / risk parity | Balance $w_i(\Sigma\mathbf w)_i$ contributions | Diversify modeled variance contributions | Leverage, regime shifts, and unstable correlation estimates |
| Maximum diversification | Maximize $\sum_i w_i\sigma_i/\sqrt{\mathbf w^\top\Sigma\mathbf w}$ | Spread risk across less correlated exposures | Estimated correlation risk and hidden factor exposures |
| Black–Litterman | Combine prior equilibrium returns with uncertain views | Temper aggressive mean forecasts | Prior, confidence, and market-cap assumptions drive output |
| Shrinkage / robust optimization | Regularize $\Sigma$, shrink $\boldsymbol\mu$, penalize turnover or worst-case loss | Reduce sampling instability | Choice of penalty/radius is itself a modeling judgment |
| Minimum CVaR/ES | Minimize average tail loss at a specified confidence level | Focus on large downside outcomes | Sparse tail data; scenario dependence; may miss omitted risks |
| Liability-relative / goals based | Control shortfall against dated liabilities, funding targets and liquidity needs | Pension, endowment, household or insurance portfolio | Objective depends on credible liability and cash-flow models |

For mean–variance optimization with transaction costs, illustrate an investment policy using a practical constraint set:

$$
\begin{aligned}
\max_{\mathbf w}\quad & \boldsymbol\mu^\top\mathbf w-\frac{\lambda}{2}\mathbf w^\top\Sigma\mathbf w-\eta\lVert\mathbf w-\mathbf w_0\rVert_1\\
\text{subject to}\quad & \mathbf 1^\top\mathbf w=1,\\
& 0\leq w_i\leq u_i,\\
& \lVert\mathbf w-\mathbf w_0\rVert_1\leq \tau.
\end{aligned}
$$

Here $\mathbf w_0$ are pre-trade weights, $\eta$ is a linear-turnover penalty, $u_i$ are position limits, and $\tau$ is a turnover budget. An $\ell_1$ penalty is a *proxy* for variable trading costs; it does not model bid–ask impact, taxes or market capacity automatically. For an equal-risk-contribution long-only portfolio with positive risk contributions, the target condition is

$$
RC_i=\frac{w_i(\Sigma\mathbf w)_i}{\sqrt{\mathbf w^\top\Sigma\mathbf w}}\approx\frac{\sigma_p}{n}.
$$

**Study experiment:** estimate $\Sigma$ on a rolling window; compute five allocations; compare realized out-of-sample annualized volatility, excess return, maximum drawdown, turnover, and distance from target weights. Repeat during a correlation spike. If the optimized portfolio's gains disappear after costs or parameter perturbations, report that finding rather than overfit the example.

### Factor models, securities, and professional investment analysis

Add a bridge from the one-factor CAPM toward the quantitative and fundamental material commonly associated with professional investment education:

- **Multi-factor risk models:** market, size, value, momentum, profitability and investment factors as illustrative modeling choices; understand that factor labels and empirical evidence depend on the sample and construction method. Estimate factor loadings and separate residual risk from systematic exposures.
- **Equity research:** firm business model, quality of earnings, margins, payout, balance-sheet leverage, earnings versus cash generation, fundamental valuation (DDM and introductory FCFE/FCFF) and how analyst expectations affect market prices. Full enterprise modeling belongs in ECON 235.
- **Fixed income:** price–yield sensitivity, coupon reinvestment, duration, convexity, credit spreads, default versus interest-rate exposure, and how bonds change total portfolio risk. Deep fixed-income valuation appears in ECON 235/236.
- **Alternative investments:** real assets, commodities, real estate, private markets, hedge-fund strategies, appraisal smoothing and the difficulty of comparing illiquid reported returns to liquid benchmarks.
- **Professional practices:** investment policy statement (IPS), strategic versus tactical allocation, benchmarks, best execution, client suitability, conflicts of interest, disclosure, governance, ethical use of performance statistics and avoidance of survivorship/look-ahead biases.

An empirical factor model is a *description to be tested*, not an arbitrage identity:

$$
R_{i,t}-R_{f,t}=\alpha_i+\boldsymbol\beta_i^\top\mathbf F_t+\varepsilon_{i,t}.
$$

Estimate alpha with appropriate standard errors, test stability over subperiods, and state clearly whether $\mathbf F_t$ are traded excess-return factors. Compare gross and net alpha after plausible fees and implementation costs. For fundamentals, explain how a valuation thesis differs from evidence of mispricing.

### Risk management runs through every portfolio decision

Differentiate **market risk** (rates, equities, spreads, FX), **factor and concentration risk**, **credit/counterparty risk**, **liquidity and funding risk**, **leverage/margin risk**, **model and parameter risk**, and **operational/behavioral risk**. Establish a risk budget *before* constructing the portfolio. Monitor what can go wrong, not only what has historically fluctuated.

Useful quantitative measures include ex ante volatility, beta, tracking error, factor exposure, peak-to-trough drawdown, downside deviation, scenario loss, VaR and expected shortfall. Define loss $L=-R_p$ over a fixed horizon and confidence $\alpha$. A conventional positive-loss value-at-risk and upper-tail expected shortfall for a continuous loss distribution are

$$
VaR_\alpha(L)=\inf\{\ell:\Pr(L\leq\ell)\geq\alpha\},
$$

$$
ES_\alpha(L)=E[L\mid L\geq VaR_\alpha(L)]\quad\text{(continuous distributions)}.
$$

For distributions with atoms, use the quantile-integral definition instead of the conditional-tail shortcut. Historical VaR depends on the available sample, and even accurate VaR does not bound worst-case losses. Expected shortfall measures the average severity of the selected tail, not an absolute maximum loss.

**Required stress suite:** an equity crash, a sharp parallel rate increase, an inflation shock with stock/bond correlations turning positive, a liquidity freeze, a widening credit spread, and forced deleveraging. Specify shock magnitudes as *assumptions*, not purported forecasts. Report expected and stressed returns, cash needs, mandate breaches, and what changes the investment decision.

### Twelve-week teaching and assessment plan

**Target workload:** approximately 10–14 hours/week: two theory/reading sessions, one derivation/problem session, one numerical or market-data lab, and a short written interpretation. Weeks 1–11 each produce a checkable artifact; Week 12 is a full project defense. A suggested grading structure is problem sets 25%, labs 30%, midterm in Week 6 15%, final portfolio research project 30%. Independent learners can use the same proportions to allocate time without formal grading.

| Week | Theory and learning objectives | Quantitative/problem work | Applied risk checkpoint and deliverable |
|---|---|---|---|
| **1** | Financial claims, market institutions, consumption choice and arbitrage intuition. Trace ownership and payoff rights. | Build cash-flow timelines, calculate present values and compare assets on a common currency/date basis. | Identify liquidity, issuer and legal/settlement risks. **Deliverable:** instrument and cash-flow map. |
| **2** | APR/EAR, compounding, inflation, geometric versus arithmetic return, TWR versus IRR. | Audit return calculations across deposits, equity with dividends and a bond. Build an investment index accounting for contributions. | Currency/real-return/fee reconciliation. **Deliverable:** return-conventions workbook. |
| **3** | Risk preferences, distributions, covariance, diversification and downside statistics. | Calculate certainty equivalents, marginal risk contributions, covariance estimates and two-asset minimum variance. | Volatility versus tail/shortfall risk. **Deliverable:** probability-and-risk notebook. |
| **4** | Markowitz theory, efficient frontier, capital allocation and risk-free borrowing/lending. | Derive GMV, frontier, tangency candidate and long-only optimization; verify KKT or solver conditions. | Leverage/concentration constraints. **Deliverable:** constrained efficient-frontier report. |
| **5** | Alternative optimization: risk parity, inverse vol, max diversification, robust/shrinkage, Black–Litterman and CVaR. | Solve at least four methods on a common dataset and test parameter perturbations. | Regime-sensitive correlations and estimation risk. **Deliverable:** optimizer comparison including transaction costs. |
| **6** | CAPM, beta, equity risk premium and multifactor alternatives; **midterm**. | Estimate CAPM and factor models, produce beta/alpha intervals and compute cost of equity. | Factor crowding and benchmark choice. **Deliverable:** midterm derivations plus asset-pricing note. |
| **7** | Market efficiency, anomalies, market microstructure, data quality and disciplined backtesting. | Research one signal, construct train/test split and account for spreads, turnover and selection bias. | Look-ahead/survivorship/model-mining controls. **Deliverable:** replicable signal-test plan. |
| **8** | Equity, fixed income, credit, alternatives and cross-asset allocation. | Analyze representative securities by cash-flow, yield, duration, credit quality and portfolio contribution. | Rates/spread/illiquidity risks. **Deliverable:** cross-asset security analysis. |
| **9** | Portfolio performance: Sharpe, Treynor, Jensen alpha, information ratio, attribution and benchmark selection. | Calculate gross/net returns, rolling performance, factor and allocation attribution. | Performance smoothing and stale marks. **Deliverable:** manager due-diligence dashboard. |
| **10** | IPS, asset allocation, goals/liabilities, suitability, execution, rebalancing, taxes and ethics. | Construct a target allocation with policy bands, turnover budget and scenario-based rebalancing rule. | Funding and liquidity needs. **Deliverable:** institutional/client IPS. |
| **11** | Portfolio enterprise risk: VaR, ES, stress tests, tracking-error risk budget and governance. | Backtest risk forecasts, decompose exposures, stress correlations and implement a drawdown/margin response. | Tail, model, liquidity and governance risks. **Deliverable:** portfolio risk committee pack. |
| **12** | Integrated investment process and written/oral defense. | Rebuild your final portfolio from raw data; audit all units, assumptions, optimizers and risk statistics. | Compare recommended policy with 1/N, market benchmark and a stress-tested defensive portfolio. **Deliverable:** full portfolio capstone. |

**Week-6 checkpoint:** no progression to active portfolio selection without (i) consistent returns, (ii) positive-semidefinite covariance estimates or a documented correction, (iii) derivation of the main optimizer and CAPM, and (iv) an out-of-sample protocol. **Week-12 checkpoint:** explain why your recommendation is sensible even if the estimated expected returns are wrong.

### Expanded final project requirements

Keep the existing final project, but raise the standard: use **8–15 investable proxies**, benchmark two market regimes, compare **at least five allocation approaches**, state estimation and rebalance dates, show a long-only implementation (and a separate clearly labeled leveraged experiment if desired), and create a seven-column audit table: expected return, realized out-of-sample return, volatility, expected shortfall, drawdown, turnover, and stress loss. Include a one-page IPS and a one-page risk committee brief. Provide code or spreadsheet cells, not only charts.

**Method references for further study (external professional benchmark, not course claims):** CFA Institute's 2026 material on [portfolio planning and construction](https://www.cfainstitute.org/insights/professional-learning/refresher-readings/2026/basics-of-portfolio-planning-and-construction), [asset allocation methods](https://www.cfainstitute.org/insights/professional-learning/refresher-readings/2026/principles-asset-allocation), and [risk management](https://www.cfainstitute.org/insights/professional-learning/refresher-readings/2026/introduction-risk-management). Use their learning outcomes for breadth, not as evidence that this is an official CFA or UCSC syllabus.

## Concepts you must not conflate

- **Return vs yield:** realized holding-period return includes price change; a quoted yield is a rate defined under a particular cash-flow assumption.
- **Standard deviation vs beta:** total risk differs from exposure to a market benchmark.
- **Price efficiency vs investor rationality:** informational efficiency is a hypothesis about prices relative to an information set, not a statement that everyone behaves optimally.
- **CAPM vs no-arbitrage derivative pricing:** CAPM concerns required returns in a market equilibrium; derivatives may be priced by replication with risk-neutral expectations under appropriate assumptions.
- **Expected versus estimated inputs:** optimizing noisy forecasts can produce implausible portfolio weights; always investigate sensitivity.
- **Sharpe vs alpha:** one measures excess return per unit total risk; the other is a model-dependent intercept or abnormal return measure.

## Recommended learning outputs

| Checkpoint | Artifact | Acceptance standard |
|---|---|---|
| After Modules 1–3 | Time-value and risk notebook | Cash-flow timing, compounding conventions and probability assumptions are explicit |
| After Modules 4–5 | Markowitz optimizer | Constrained/unconstrained cases; weights sum correctly; covariance valid; chart and limiting checks |
| After Modules 6–7 | CAPM research note | Beta/alpha, regression period, matching risk-free rate, interpretation and uncertainty |
| After Modules 8–10 | Investment policy memo | Benchmark, turnover/costs, leverage controls, monitoring plan and critique of strategy |

## Final applied project — from raw data to an investment recommendation

Construct a multi-asset portfolio with at least six instruments or asset-class proxies. Use an explicitly dated and aligned return sample; calculate simple/log and annualized return measures and the covariance matrix; plot a constrained efficient frontier; identify tangency and minimum-variance portfolios; estimate market betas; report Sharpe, drawdown and benchmark-relative performance. Conduct an out-of-sample evaluation or clearly state why one is unavailable. Stress the portfolio under at least three coherent return/correlation shocks and compare results to a simple equal-weight baseline. Prepare a 2–3 page investment memo separating measured historical facts from estimated expectations and assumptions.

**Rubric (self-assess):** financial conventions 20%; correct portfolio math 25%; reproducible calculations 20%; implementation realism 15%; interpretation and model criticism 20%.

## Readings, source status, and next additions

**Verified from user's description:** risky and intertemporal choice, asset pricing, efficient-market hypotheses, financial institutions, derivative securities; Markowitz, CAPM, Sharpe ratio, leverage, diversification, return measures, EAR and optimization. **Suggested preparatory/advanced content:** regression diagnostics, estimation shrinkage, APT/factors, portfolio implementation research and more extensive performance attribution. These support the stated goals but are not claimed to have been covered by ECON 233.

**To complete source mapping when course files arrive:** insert exact lecture names/page references; instructor topic order; assessed derivations; problem-set links; textbook edition and chapters. Until then, work the mastery problems above as authored exercises, not purported university exam questions.

**Next:** [Use required returns for corporate capital budgeting](../ECON-235-Corporate-Finance/README.md) or [transition from financial assets to derivatives](../ECON-236-Financial-Engineering/README.md).
