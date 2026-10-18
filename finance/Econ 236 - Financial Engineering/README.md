# ECON 236 — Financial Engineering

## Interest-rate mathematics, derivative securities, stochastic models, and financial risk management

**Purpose.** Develop the mathematics and economic reasoning needed to describe contingent claims, price them by no-arbitrage or consistent models, and use them to manage financial risks at corporations, banks and other institutions.

**Reading anchor:** John C. Hull, *Options, Futures, and Other Derivatives*, is the named core textbook. The user also studied **financial mathematics** and **interest-rate mathematics** texts (titles and editions not provided). This guide deliberately puts those subjects **at the beginning** and revisits them throughout the pricing and risk-management modules. Its coverage follows the supplied UCSC ECON 236 course description—interest rate, FX, commodity and equity risk; futures, forwards, options, swaps, structured notes and asset-backed securities; pricing/payoffs and corporate cases—plus explicitly named topics including Black–Scholes, Itô's lemma and stochastic processes. **No finance lecture notes, assignments, or exact book chapters were provided.** Therefore this is an advanced *proposed self-study sequence*, not a verified reconstruction of an individual course syllabus. Material marked **[Extension]** goes beyond what the brief course description confirms.

[← Graduate Finance](../README.md) · [Finance I](../ECON-233-Finance-I/README.md) · [Corporate Finance](../ECON-235-Corporate-Finance/README.md) · [Risk-management playbook](../risk-management-playbook.md)

**Quarter format:** 12 weeks, approximately 12–16 hours/week. Hull remains the core derivatives reference, with financial mathematics and interest-rate mathematics used throughout. Greek-based hedging, volatility surfaces, model validation, rates exposure and institutional risk are substantial assessed topics.

**Navigation:** [Jump to the 12-week course schedule](#full-twelve-week-financial-engineering-plan) · [Shared risk-management playbook](../risk-management-playbook.md) · [Program knowledge map](../README.md#knowledge-map-concept-dependencies)

## The intellectual spine

Financial engineering is not a list of derivative formulas. The central progression is:

1. **Date and value cash flows:** understand rates, discount factors, compounding, no-arbitrage and contracts.
2. **Describe uncertainty:** specify payoff functions and stochastic models for underlying assets and rates.
3. **Find a consistent price:** use replication, hedging, risk-neutral valuation and numerical approximations subject to assumptions.
4. **Measure sensitivities:** quantify changes in value with spot, volatility, time, rates, curves and credit parameters.
5. **Construct hedges:** offset exposures under cost, margin, liquidity, basis and counterparty constraints.
6. **Challenge the model:** test hedging effectiveness, scenario losses, path dependence, tail risks and model breakdowns.

### Dependency map

```mermaid
flowchart TD
 A[Compounding, discount factors, bond math] --> B[Term structures & forward rates]
 A --> C[No-arbitrage & replication]
 B --> D[Forwards, futures, FRAs & swaps]
 C --> D
 E[Probability, conditional expectation, stochastic processes] --> F[Brownian motion & Ito's lemma]
 F --> G[Geometric Brownian motion]
 C --> H[Binomial & risk-neutral pricing]
 G --> I[Black-Scholes-Merton PDE]
 H --> I
 I --> J[Option prices & Greeks]
 B --> K[Fixed-income duration / curve risk]
 D --> K
 J --> L[Numerical pricing & volatility modeling]
 D --> M[Multi-asset risk & hedging]
 K --> M
 L --> M
 M --> N[Corporate/financial institution cases]
 O[Model validation, stress, credit and liquidity] --> N
```

## Entry requirements: mathematics and finance

| Area | Essential ability | Preparation if rusty |
|---|---|---|
| Discounting/interest math | Move between nominal, effective, periodic and continuously compounded rates; discount irregular cash flows | Module 1; external financial mathematics text |
| Multivariate calculus | Partial derivatives, Taylor approximations, change of variables | Refresh calculus before Itô and Greeks |
| Probability | Conditional expectation, normal distributions, stochastic independence, covariance, conditioning | Refresh measure/probability foundations as necessary |
| Differential equations | Solve basic ODEs; recognize PDE terminal/boundary-value problems | Review before Black–Scholes PDE |
| Linear algebra/numerics | Solve systems, interpolate, simulate, check convergence | Review for curves and numerical pricing |
| Finance fundamentals | Bond cash flows, forwards, investment risk and corporate exposures | ECON 233 and ECON 235 fundamentals |
| Coding | Reproducible Python, MATLAB, R or spreadsheet functions | Build basic discounting and option-payoff scripts |

**A mathematically mature reader need not arrive already fluent in stochastic calculus.** Its foundations are taught within this guide. Conversely, knowing Brownian motion is not a substitute for contract settlement conventions and economic interpretation.

## Reference conventions and assumptions

Use $t$ for valuation time, $T$ for maturity, $S_t$ for the underlying spot price, $K$ for strike/delivery price, $r$ for a continuously compounded *risk-free* rate in simplified examples, $q$ for a continuous yield/foreign-rate analogue where applicable, $\sigma$ for diffusion volatility, $W_t$ for Brownian motion, and $E_t^{Q}$ for expectation under a model's risk-neutral measure. Specify currency, calendar, payment time, day count, settlement, collateral/discount curve and compounding before performing numerical valuation.

> **Model discipline:** Black–Scholes–Merton is an idealized benchmark, not a universal empirical law. Drift assumptions, diffusion volatility, trading ability, market completeness, collateral, transaction costs, jumps, exercise features and default risk can all change a valuation or hedge strategy.

---

# Part I — Financial mathematics and contractual cash flows

### Module 1 — Compounding, annuities, discount factors, and financial equivalence

**Concepts:** simple interest, nominal rates and compounding frequency, EAR, continuously compounded rates, accumulation functions, present value, discount factors, annuities, perpetuities, amortizing loans, loan balances, internal-rate calculations, day-count and calendar conventions.

**Key formulas:** For nominal annual rate $j$ convertible $m$ times,

$$
EAR=\left(1+\frac jm\right)^m-1.
$$

For constant continuously compounded rate $r$ over year fraction $T$,

$$
D(0,T)=e^{-rT}.
$$

Discount dated deterministic cash flows using consistent discount factors:

$$
PV=\sum_i CF_{t_i}D(0,t_i).
$$

For $n$ end-of-period level payments $A$ at effective per-period rate $i\ne0$,

$$
PV=A\frac{1-(1+i)^{-n}}{i}.
$$

**Financial mathematics practice:** create a cash-flow engine that supports payment schedules, different effective/nominal quote conventions, present/future values, amortization and sensitivity to payment timing. Check a zero-rate limiting case and a cash-flow replication case.

**Why this matters:** a one-basis-point convention error across a large notional can be economically material; consistent dates and conventions are prerequisites for any pricing model.

**Mastery check:** two bonds advertise the same yield but compound on different bases. Requote them consistently and explain why price comparisons can otherwise be misleading.

### Module 2 — Fixed-income mathematics: spot, forward and yield curves

**Concepts:** zero-coupon bonds, bootstrapping a discount curve, zero/spot rates, forward rates, par coupon rates, yield to maturity, yield curve interpolation, forward rate agreements (FRAs), basis, parallel and nonparallel curve shifts, term structure interpretation, interest accrual.

**Relationships:** For continuously compounded rates, the forward rate between $T_1$ and $T_2$ is

$$
f(T_1,T_2)=-\frac{\ln[D(0,T_2)/D(0,T_1)]}{T_2-T_1}.
$$

Equivalently, in the same convention,

$$
D(0,T_2)=D(0,T_1)e^{-f(T_1,T_2)(T_2-T_1)}.
$$

**Application:** bootstrap discount factors from a hypothetical set of deposit, zero and par swap quotes under clearly stated conventions, then price a nonstandard coupon bond. Compare using a flat yield with cash-flow-specific discount factors.

**Mastery check:** explain why a high yield-to-maturity is not identical to an expected realized return; describe curve and reinvestment risks.

### Module 3 — No-arbitrage, payoff diagrams, and market mechanics

**Concepts:** arbitrage portfolios, law of one price, long/short positions, borrowing and lending, state-contingent payoffs, static versus dynamic replication, arbitrage bounds, bid–ask spreads, margin, collateral, clearinghouses and exchange versus over-the-counter trading.

**Application:** build a payoff table for cash, stock, bond, forward, call and put across multiple underlying-price scenarios. Detect when two traded combinations have identical payoffs but different initial prices; explicitly include the assumptions needed to enforce the price equality.

**Mastery check:** why does the existence of a spread, limited borrowing capacity or different funding rates change the exploitability of a theoretical arbitrage?

# Part II — Linear derivatives and interest-rate risk

### Module 4 — Forwards and futures: pricing, payoff, basis, and hedging

**Concepts:** long and short positions, delivery versus settlement, forward contract value after initiation, futures marking-to-market, convergence, basis risk, hedging effectiveness, cost-of-carry, dividends/income, foreign-currency forwards and commodity storage/convenience yields.

**Idealized benchmarks:** For a stock with known continuous yield $q$ and constant financing rate $r$,

$$
F_0=S_0e^{(r-q)T}.
$$

For currency with analogous domestic and foreign continuously compounded rates, if spot $S_0$ is quoted as **domestic currency per unit of foreign currency**,

$$
F_0=S_0e^{(r_d-r_f)T}.
$$

Commodity forward pricing additionally requires storage, income and convenience-yield assumptions and may not reduce to this simple formula.

**Application:** choose a forward hedge for a foreign-currency invoice, a futures hedge for inventory input cost and a commodity contract with meaningful basis risk. Compute profit-and-loss at expiry and mark-to-market scenarios before expiry.

**Mastery check:** distinguish spot exposure from basis exposure, and distinguish futures daily cash flows from a forward's terminal settlement.

### Module 5 — Interest-rate swaps, FRAs, and corporate rate hedging

**Concepts:** fixed-for-floating swaps, net settlement, notional principal, swap legs, par swap rate, forward rate agreements, interest-rate caps/floors, payer and receiver swaps, swap value as differences of discounted cash-flow legs, counterparty/collateral conventions.

**Valuation principle:** at initiation, an idealized par swap has approximately zero value when its fixed rate equals the appropriate rate implied by consistent discounting and forward projection. Later valuation equals the difference between present values of remaining legs. In modern collateralized settings, discounting and projection curves can differ **[Extension]**.

**Application:** a firm has floating-rate debt and wants predictable interest payments. Create a fixed-pay/float-receive swap cash-flow table, examine a +100 bp / −100 bp rate move, and describe how hedge effectiveness depends on reference indices, reset dates, credit, and cash-flow matching.

**Mastery check:** what residual risks remain after a borrower swaps floating payments for fixed ones?

### Module 6 — Bond duration, convexity, and curve exposure

**Concepts:** Macaulay/modified duration, DV01/PV01, key-rate duration **[Extension]**, convexity, immunization, asset–liability management, duration mismatch, yield sensitivity versus credit spread sensitivity, callable bond optionality.

**Benchmark approximations:** With duration $D_{mod}$ and convexity $C$,

$$
\frac{\Delta P}{P}\approx-D_{mod}\Delta y+\frac12 C(\Delta y)^2.
$$

For a one-basis-point yield move and consistent price units,

$$
DV01\approx P\,D_{mod}\times10^{-4}.
$$

For multi-curve exposures, key-rate/repricing calculations are more informative than assuming a uniform parallel yield shift.

**Application:** calculate duration and convexity from bond cash flows; check with finite differences. Build a duration-matched hedge with futures or a swap and analyze nonparallel shifts, convexity differences and basis effects.

**Mastery check:** why can a duration-neutral portfolio still lose money when interest-rate volatility or curve shape changes?

# Part III — Optionality and discrete-time valuation

### Module 7 — Options and structured payoffs

**Concepts:** European versus American exercise, calls and puts, intrinsic/time value, moneyness, payoff versus profit, put–call parity, static option strategies (covered call, protective put, bull spread, straddle), payoff decomposition of structured notes, early exercise and embedded options.

**Core payoffs:** At maturity, a European call and a European put pay, respectively,

$$
\begin{aligned}
\text{Call payoff}&=(S_T-K)^+,\\
\text{Put payoff}&=(K-S_T)^+,\\
x^+&=\max(x,0).
\end{aligned}
$$

Under a known continuous yield $q$, European put–call parity in the idealized model is

$$
C-P=S_0e^{-qT}-Ke^{-rT}.
$$

**Application:** graph payoffs and net profits for long stock, protective put, collar and short call. Include the option premium and financing explicitly; avoid mistaking a low premium for a free hedge.

**Mastery check:** why can American options be worth at least their European counterparts, and why does the optimal exercise decision depend on carry and distributions?

### Module 8 — Binomial replication and risk-neutral probabilities

**Concepts:** one-period and multiperiod trees, replicating delta and bond holdings, unique state prices in a complete binomial world, risk-neutral pricing, martingales under a change of measure, backward induction, American exercise decisions.

**One-step benchmark:** In a one-step tree, the underlying moves from $S$ to $Su$ or $Sd$ over $\Delta t$. When $d<e^{r\Delta t}<u$, the risk-neutral pricing weight is

$$
p^{\ast}=\frac{e^{r\Delta t}-d}{u-d}.
$$

The corresponding contingent payoff has price, under the usual complete frictionless model assumptions,

$$
V_0=e^{-r\Delta t}\left[p^{\ast}V_u+(1-p^{\ast})V_d\right]
$$

**The risk-neutral weight $p^{\ast}$ is not necessarily the physical probability.**

**Application:** implement 1-, 2-, 50-, 100- and 500-step trees; compare prices with the Black–Scholes benchmark as the step count increases, and include early-exercise comparison for an American put.

**Mastery check:** explain how replication prices an option without assuming that all investors agree on the expected stock return.

# Part IV — Continuous-time finance and Black–Scholes

### Module 9 — Stochastic processes, Brownian motion, and Itô's lemma

**Concepts:** filtration and available information, random walks and Brownian motion, independent increments, conditional expectation, diffusion processes, martingales (introductory level), stochastic differential equations, quadratic variation, Itô integral and Itô's lemma.

**Canonical process:** Under geometric Brownian motion,

$$
dS_t=\mu S_t\,dt+\sigma S_t\,dW_t.
$$

For constant $\mu,\sigma$ and $W_0=0$, this implies

$$
S_T=S_0\exp\!\left[\left(\mu-\frac12\sigma^2\right)T+\sigma W_T\right].
$$

**Itô's lemma (one factor):** for smooth $f(t,S_t)$,

$$
df=\left(f_t+\mu S f_S+\tfrac12\sigma^2 S^2 f_{SS}\right)dt+\sigma S f_S dW_t.
$$

The second-derivative term arises from Brownian quadratic variation, so ordinary chain rules are insufficient.

**Application:** simulate Brownian paths and GBM paths with fixed random seeds; compare approximate sample mean/variance of log returns with theoretical values. Derive $d\ln S$ using Itô and show where the $-\sigma^2/2$ correction arises.

**Mastery check:** derive the expected price under GBM and explain the difference between drift of $S_t$ and drift of $\ln S_t$.

### Module 10 — Black–Scholes–Merton: hedging, PDE, and risk-neutral valuation

**Concepts:** delta hedging, self-financing replication intuition, no-arbitrage PDE, terminal/boundary conditions, lognormal distribution under the model, risk-neutral drift, closed-form European option prices, dividends, put–call parity and model assumptions.

**Core derivation:** Apply Itô's lemma to option value $V(t,S)$. Under continuous trading, frictionless financing, a stock with continuous yield $q$, constant $r,\sigma$, and dynamic hedging assumptions, eliminating the instantaneous Brownian shock gives

$$
V_t+\tfrac12\sigma^2S^2V_{SS}+(r-q)SV_S-rV=0.
$$

For a European call,

$$
C=S_0e^{-qT}N(d_1)-Ke^{-rT}N(d_2),\qquad
 d_1=\frac{\ln(S_0/K)+(r-q+\sigma^2/2)T}{\sigma\sqrt T},\quad
 d_2=d_1-\sigma\sqrt T.
$$

**Application:** code BSM call and put prices; test put–call parity, zero-volatility intuition (with care around limiting cases), bounds, sensitivity signs and convergence against a binomial tree. Provide both a mathematical derivation and an economic explanation of why the expected stock return $\mu$ disappears from the idealized replication price.

**Mastery check:** distinguish $Q$-measure pricing from real-world expected growth; explain why the absence of $\mu$ does **not** imply that risk is irrelevant to investment decisions.

### Module 11 — Greeks, dynamic hedging, and volatility

**Concepts:** delta, gamma, vega, theta, rho, cross-gamma/other sensitivities **[Extension]**, hedge ratios, implied volatility, realized volatility, volatility smiles/skews, discrete rebalancing, gamma P&L, transaction costs and model risk.

**Illustration:** For a Black–Scholes–Merton European call with continuous yield,

$$
\Delta=e^{-qT}N(d_1).
$$

A first-order sensitivity approximation is

$$
dV\approx\Delta\,dS+\mathrm{Vega}\,d\sigma+\rho\,dr+\Theta\,dt.
$$

For larger changes, second-order terms, including

$$
\frac12\Gamma(dS)^2,
$$

matter. Interpret $dt$ and each Greek consistently with the pricing clock and quote conventions.

**Application:** construct delta-neutral option portfolios; shock spot and volatility independently and jointly; run discrete hedging simulations to examine tracking error. Compare hedge P&L in low- vs high-volatility scenarios.

**Mastery check:** can a position be delta-neutral and still have substantial losses? Explain via gamma, vega, jumps, liquidity and rebalancing.

### Module 12 — Numerical pricing methods and model validation

**Concepts:** recombining trees, Monte Carlo under pricing measure, simulation of correlated processes, variance reduction **[Extension]**, finite-difference PDE methods **[Extension]**, least-squares American option pricing **[Extension]**, convergence tests, calibration and reproducibility.

**Application:** price the same European payoff with BSM closed-form, a convergent tree and Monte Carlo using independently implemented methods. Plot absolute error versus tree steps and Monte Carlo standard error versus simulation count. For path-dependent claims such as Asian options, explain why a vanilla closed-form result may not directly apply.

**Mastery check:** why is a numerically precise answer not sufficient evidence that the pricing model is appropriate for the underlying product?

# Part V — Risk management and additional instruments

### Module 13 — Multi-market hedging: FX, commodities, equities, rates

**Concepts:** exposure mapping by risk factor, cash-flow vs accounting exposures, hedge instrument selection, hedge ratios, minimum-variance hedge intuition, liquidity and rollover, cross-hedging, basis risk, calendar mismatch, forward versus options choice, tail-risk constraints.

**Application case A (FX):** a U.S. buyer owes a foreign-currency payment at a fixed future date. Compare no hedge, a forward, and a purchased call on the foreign currency in domestic-currency terms. Calculate total domestic-currency cost for a range of exchange rates, including premium, maturity and settlement terms.

**Application case B (commodities):** an airline-like buyer faces jet fuel price risk but hedges using a related liquid benchmark. Estimate a regression/covariance-based hedge ratio and quantify basis-risk residuals.

**Application case C (rates):** hedge floating-rate financing via a pay-fixed swap or cap, then stress credit and funding spreads separately from reference rates.

**Mastery check:** identify why a contract can be an effective market hedge but an ineffective hedge for the firm's *economic* exposure.

### Module 14 — Structured notes, credit, and asset-backed securities

**Core course-description topics:** structured notes and asset-backed securities; contract waterfall, coupons, embedded option features, tranching, credit enhancement, collateral cash flows, subordination, prepayment and default scenarios, counterparty exposure.

**Suggested analytical sequence:** decompose a structured note into bond-like promised cash flows and embedded derivatives; model how asset-backed cash flows are distributed through a simple waterfall; stress prepayments, defaults, recovery rates and collateral correlation. Do not assume a quoted yield adequately describes option, liquidity or credit risks.

**[Extension]**: credit default swaps, structural/reduced-form credit models, XVA, wrong-way risk, detailed mortgage cash-flow models and comprehensive regulatory capital are useful but not confirmed in the supplied course description.

**Mastery check:** why does tranching redistribute risk without eliminating the underlying collateral's aggregate losses?

### Module 15 — Financial institution risk, model limitations, and stress testing

**Concepts:** market risk, counterparty risk, credit risk, funding liquidity, collateral/margin risk, concentration, basis risk, model risk, scenario and sensitivity analysis, Value at Risk (VaR), Expected Shortfall (ES), historical versus hypothetical stress, backtesting **[Extension]**.

**Definitions:** for loss variable $L$, $VaR_\alpha$ is an $\alpha$-quantile of $L$. For a continuous loss distribution, $ES_\alpha$ can be characterized as expected loss in the upper $1-\alpha$ tail. Estimation requires explicit horizon, percentile, distribution assumptions and valuation model.

**Application:** aggregate simulated P&L from several contracts, show VaR/ES under a historical model and a fat-tailed stress alternative, and explain why normal-distribution approximations can miss nonlinear tail risk. Add a margin-call and liquidity stress case.

**Mastery check:** explain why a hedge that reduces daily marked-to-market volatility can increase short-term funding needs.

### Module 16 — Advanced interest-rate models and optional research track **[Extension]**

This module develops the supplementary interest-rate mathematics into an advanced financial engineering research path. It is **not claimed as confirmed ECON 236 lecture coverage**.

**Topics:** stochastic short-rate models (Vasicek and CIR), no-arbitrage term-structure consistency, risk-neutral bond pricing, calibration to the initial curve, forward-rate models (HJM), market models, cap/floor and swaption pricing, stochastic volatility/local volatility and jumps.

**Example process:** The Vasicek short rate satisfies

$$
dr_t=a(b-r_t)\,dt+\sigma_r\,dW_t.
$$

It illustrates mean reversion but permits negative rates. CIR variants have state-dependent volatility and different boundary behavior. Calibrate and assess any chosen process for the instrument and market being modeled.

**Application:** compare deterministic and stochastic-rate valuations for a long-lived rate-sensitive claim; map which assumptions explain the differences.

**Mastery check:** Why is reproducing today's yield curve not sufficient to establish that a term-structure model prices interest-rate options well?

---

## Advanced options laboratory: the Greeks as a risk-management system

The underlying course already teaches options, Itô calculus and Black–Scholes. To support a **twelve-week financial-engineering quarter**, this extended laboratory makes the Greeks, option portfolio hedging and stress P&L a central assessed skill rather than a single short topic. The formulas in this section use the European Black–Scholes–Merton benchmark with constant $r$, $q$, $\sigma$, spot $S$, strike $K$, and time **remaining** $\tau=T-t>0$; $N$ denotes the standard normal CDF and $\phi$ its PDF. Other products/models require other sensitivities.

### The complete first-order and major second-order sensitivities

For a European call and put with a continuous yield $q$,

$$
\begin{aligned}
d_1&=\frac{\ln(S/K)+(r-q+\sigma^2/2)\tau}{\sigma\sqrt\tau},\\
d_2&=d_1-\sigma\sqrt\tau,\\
C&=Se^{-q\tau}N(d_1)-Ke^{-r\tau}N(d_2),\\
P&=Ke^{-r\tau}N(-d_2)-Se^{-q\tau}N(-d_1).
\end{aligned}
$$

**Delta ($\Delta$)** measures a local option-price change per unit change in the underlying price, with other model inputs fixed:

$$
\Delta_C=e^{-q\tau}N(d_1),\qquad \Delta_P=e^{-q\tau}\bigl[N(d_1)-1\bigr].
$$

**Gamma ($\Gamma$)** measures the rate of change of delta with respect to spot, and is the same for the benchmark call and put with identical $S,K,\tau$:

$$
\Gamma=\frac{e^{-q\tau}\phi(d_1)}{S\sigma\sqrt\tau}.
$$

**Vega ($\nu$)** is the price derivative with respect to volatility (volatility measured as a decimal); it is the same for benchmark call and put:

$$
\nu=Se^{-q\tau}\phi(d_1)\sqrt\tau.
$$

**Theta ($\Theta$)** here means the derivative with respect to calendar time $t$ *holding maturity $T$ fixed*, not the derivative with respect to time-to-maturity $\tau$. For the benchmark call,

$$
\Theta_C=-\frac{Se^{-q\tau}\phi(d_1)\sigma}{2\sqrt\tau}-rKe^{-r\tau}N(d_2)+qSe^{-q\tau}N(d_1).
$$

**Rho ($\rho$)** is the derivative with respect to the continuously compounded risk-free rate $r$, holding $q$ fixed:

$$
\rho_C=K\tau e^{-r\tau}N(d_2),\qquad\rho_P=-K\tau e^{-r\tau}N(-d_2).
$$

**Put theta:** derive from put-call parity rather than memorizing blindly; explicitly identify whether the model has dividends. Distinguish **vanna** ($\partial^2V/\partial S\partial\sigma$), **volga/vomma** ($\partial^2V/\partial\sigma^2$), **charm** (delta's time sensitivity), **speed** (gamma's spot sensitivity), and **color** (gamma's time sensitivity) as *higher-order extension concepts*. Use finite differences and automated differentiation as checks where appropriate.

**Interpretation and numerical conventions:** A quoted vega of 20 in $\partial V/\partial\sigma$ units corresponds to approximately 0.20 currency units for a one percentage-point change in implied volatility, not 20. Rho is similarly often re-quoted for a one-basis-point rate change. Theta quoted per year must be converted to a calendar or trading day under a consistent convention. Gamma increases near maturity for near-the-money positions, where small underlying movements can rapidly alter the required delta hedge. Delta neutrality is local; it neither eliminates gamma nor makes a position insensitive to volatility or gaps.

### Risk explain, hedge design and P&L attribution

For a sufficiently small joint shock, with no omitted cross terms at the approximation order chosen,

$$
\Delta V\approx \Delta\,\Delta S+\frac12\Gamma(\Delta S)^2+\nu\,\Delta\sigma+\Theta\,\Delta t+\rho\,\Delta r.
$$

Record the units and sign of every shock. In the risk report, compute this approximation **and** fully reprice all contracts under each shock. The discrepancy identifies higher-order, nonlinear, basis, and model effects. A delta-only hedge offsets the linear spot term; it does not eliminate tail losses or nonlinear risk.

For hedging $m$ portfolio sensitivities with $n$ hedge instruments, build the hedge matrix of per-unit sensitivities $G\in\mathbb R^{m\times n}$ and target neutralization

$$
G\mathbf h=-\mathbf g_{\text{portfolio}}.
$$

If the system is overdetermined, cannot be solved under position bounds, or contains nearly collinear hedges, use a **constrained weighted least-squares risk minimization** and document residual exposures, hedge cost, bid–ask spread, position limits, and maturity mismatch. A textbook delta–gamma hedge may require a second option because buying underlying stock adjusts delta but not gamma; a delta–gamma–vega hedge may require multiple options with genuinely independent sensitivity profiles.

**Risk lab A — Greeks engine:** implement analytic $\Delta$, $\Gamma$, $\nu$, $\Theta$ and $\rho$ for a call and put; compare with central finite differences, taking care that theta uses calendar time, not remaining time. Test deep ITM, ATM, deep OTM, short maturity, dividend yield, small volatility, and one-day moves. Report absolute and relative errors, not only passing assertions.

**Risk lab B — dynamic hedge P&L:** sell a European call in a simulated market, initialize a delta hedge, rebalance daily versus weekly, add per-trade costs, and measure the distribution of final hedge errors. Repeat under a physical volatility that differs from the implied volatility used to price and hedge. Discuss gamma/theta P&L, discrete gaps, margin and the risks of assuming the diffusion model is true.

**Risk lab C — volatility surface:** calculate implied volatilities for a synthetic or openly sourced option surface; validate no-arbitrage price bounds, compare skew/smile at short and long maturities, interpolate cautiously, and calculate a vega-bucket report across strikes and tenors. Explain **sticky strike versus sticky delta** as alternative stress conventions rather than a universal market law.

### Interest-rate risk is curve risk, not just one duration number

Build on financial mathematics, interest-rate conventions and Hull's fixed-income chapters: zero curves, par and forward rates, bootstrapping, bond and swap cash-flow valuation, key-rate sensitivity, DV01, convexity, basis risk, reset schedules, discounting under collateral conventions, and swap/option exposures.

With a bond price $P(\mathbf z)$ expressed as a function of curve-node zero rates $z_k$, define a signed finite-difference key-rate exposure using a one-basis-point bump $h=10^{-4}$:

$$
KRDV01_k\approx-\frac{P(\mathbf z+h\mathbf e_k)-P(\mathbf z-h\mathbf e_k)}{2}.
$$

Here this expression measures the approximate **positive price loss** for a one-basis-point upward move when the instrument has ordinary positive duration; be explicit about the bump and interpolation scheme. Alternative industry sign conventions must be reconciled. Sum of key-rate exposures approximates parallel DV01 for small moves if curve construction is compatible; convexity and cross terms matter for large changes.

**Rates lab:** bootstrap one discount curve, construct a portfolio of loans, bonds and swaps, value cash flows, compute parallel DV01 and a vector of key-rate DV01s, and hedge a *steepening* rather than just a parallel shock. Report fixed-versus-floating reset risk, basis between tenors and the additional collateral required by a derivative mark-to-market change. Optional extension: Vasicek/CIR simulation, caplets, swaps, swaptions and calibration of a curve-consistent short-rate model.

### Hedge ratios and residual risk in commodity and FX contracts

Distinguish a perfect maturity-aligned forward hedge from a cross-hedge using correlated futures prices. If the spot-exposure price change is $\Delta S$ and the futures price change is $\Delta F$ over a matched hedging horizon, then the minimum-variance **price-change** hedge ratio for similarly scaled units is

$$
h^{\ast}=\frac{\mathrm{Cov}(\Delta S,\Delta F)}{\mathrm{Var}(\Delta F)}=\rho_{SF}\frac{\sigma_{\Delta S}}{\sigma_{\Delta F}}.
$$

Translate the ratio into a number of contracts **after** matching contract size, exposure size, direction and units. Check that spot and futures price movements are defined in the same units, and do not confuse the hedge's statistical minimum variance with lowest expected procurement cost or best worst-case result. Hedge horizon, index basis and volume uncertainty can dominate the residual risk.

For an FX purchase, compare a forward, option and zero-cost collar while explicitly including premium financing, exercise and physical/cash settlement. For an interest-rate borrower, compare a pay-fixed swap and purchased cap. Document cases where forwards lock a cost but give up favorable price moves, and options preserve upside but require premium and can have basis/exercise effects.

### Risk methodology, model governance and institutional controls

Develop an instrument-to-risk-factor exposure cube across spot, curve nodes, implied-volatility nodes, credit spreads, issuer/counterparty, horizon and currency. For each hedge distinguish **market-value protection**, **cash-flow protection**, **earnings protection**, and **funding liquidity**. Risk models are useful only if the decision-maker understands their blind spots.

Run a full stress suite: equity gap, simultaneous spot-volatility shock, interest-rate curve twist, commodity squeeze, FX devaluation, credit-spread widening, counterparty deterioration, funding withdrawal and increase in margin requirements. Compute **full revaluation** stressed P&L, expected shortfall (with documented data/model), hedge turnover, daily/peak collateral, and residual basis risk. Add an independent reconciliation of front-office position marks with risk and accounting systems as a model governance check.

For credit and securitization, survey probability of default, exposure at default, loss given default, netting/collateral, wrong-way risk, ABS cash-flow waterfalls and embedded prepayment options. A simple expected credit loss approximation under a clearly defined horizon is

$$
EL\approx PD\times LGD\times EAD.
$$

It is *not* an option-pricing formula and does not replace state-dependent discounting, default timing or dependence modeling. For a structured note, decompose contractual bond-like and optional payoffs, disclose credit and liquidity exposure, and explain why a headline coupon understates downside possibilities. Advanced XVA, intensity models and stochastic-rate derivatives remain optional depth beyond the confirmed course brief.

## Full twelve-week Financial Engineering plan

**Target workload:** 12–16 hours/week: Hull study, financial-math derivations, a coding/problem lab, and a risk memo. Suggested assessments: mathematical problem sets 25%; implementation labs 35%; Week-7 derivations/midterm 15%; Week-12 pricing-and-hedging defense 25%. All examples use documented conventions and explicit quote dates; do not represent toy simulation outputs as tradable prices.

| Week | Core concepts (existing module crosswalk) | Mathematical / programming requirement | Risk and application submission |
|---|---|---|---|
| **1** | Financial mathematics, cash-flow equivalence, contract mechanics (Modules 1, 3) | Write discount-factor and payment-schedule engine; unit-test APR/EAR, accrual, day count and bond-price identities. | Contract specification and convention risk audit. |
| **2** | Zero/forward curves, bootstrapping, YTM, term-structure instruments (Modules 2, 6) | Bootstrap curve, price coupon bond, compute DV01, convexity and key-rate exposures. | Curve-shock and curve-model risk report. |
| **3** | Forwards, futures, cost of carry, FX and commodities, basis (Module 4) | Price equity/FX forwards; model futures margin; estimate optimal cross-hedge ratio. | Three-way comparison: unhedged, forward and futures hedge. |
| **4** | FRAs, swaps, fixed/floating funding, asset–liability mismatch (Modules 5–6) | Construct swap cash flows and a par swap rate; value after curve shift; design hedge for nonparallel shocks. | Rates desk DV01/curve exposure sheet. |
| **5** | Option contract mechanics, arbitrage bounds, put–call parity, vertical spreads/straddles (Module 7) | Code expiry-payoff and dated profit charts; price one option portfolio by parity and bounds. | Protect/collar a corporate FX or equity exposure. |
| **6** | Binomial trees, replication, risk-neutral probabilities, American exercise (Module 8) | Derive a one-step hedge; implement a multi-step European/American tree and test convergence. | Early-exercise and discrete-model risk memo. |
| **7** | Brownian motion, conditional expectations, Itô's lemma, GBM; **midterm** (Module 9) | Derive $d\log S$, compute a diffusion solution and simulate physical versus risk-neutral paths. | Drift-versus-pricing-model conceptual examination. |
| **8** | BSM replication/PDE, closed-form prices and **full Greeks** (Modules 10–11) | Derive the PDE; implement call/put, analytic Delta/Gamma/Vega/Theta/Rho with finite-difference tests. | Option book local sensitivity risk explain. |
| **9** | Dynamic delta–gamma–vega hedging; volatility smile/skew, calibration (Module 11) | Solve a constrained multi-Greek hedge and simulate rebalancing P&L under model mismatch. | Hedge residuals, transaction cost and implied-volatility risk. |
| **10** | Trees, Monte Carlo, finite differences, American/path-dependent options, model validation (Module 12) | Cross-check a European call by three methods; implement Asian payoff MC and test convergence/SE. | Pricing-model validation and confidence/error report. |
| **11** | Multi-market hedging, credit, structured notes, ABS, VaR/ES, liquidity and counterparty risk (Modules 13–15; 16 optional) | Full-revaluation stress portfolio; structured-note payoff decomposition; hedge ratio and collateral schedule. | Bank/corporate risk committee packet with worst-case scenario. |
| **12** | Integrated pricing, hedging and governance (all modules) | Submit audited pricing library, fixed-income curve, Greeks, hedging simulation, stress engine and benchmarks. | Defend hedge recommendation against model, funding and liquidity challenges. |

**Week-7 threshold:** derive replication/tree logic, construct a correct forward, and explain the difference between physical and risk-neutral probabilities. **Week-12 threshold:** a price, hedge or VaR number is not accepted without instrument conventions, model assumptions, independent numerical checks and an explicit residual-risk statement.

### Expanded final toolkit acceptance tests

Extend the original toolkit project so that the repository includes: (1) term structure cash flows and bootstrapped curve; (2) equity, FX and commodity forwards; (3) interest-rate swap; (4) European BSM call/put with **five major Greeks** and finite-difference tests; (5) American put via binomial tree; (6) Monte Carlo of at least one path-dependent claim; (7) delta/gamma/vega hedge ratios under trade constraints; (8) parallel and nonparallel rate risk; (9) stress full-revaluation versus Greek approximation; (10) daily hedging P&L with transaction costs; (11) liquidity/collateral and counterparty register; and (12) written model limitations.

**Evaluation:** mathematical derivation 20%, correct pricing and convergence tests 25%, sensitivity/hedging mechanics 25%, stress and institutional risk 20%, documentation and clarity 10%. Hull remains the primary derivatives reference; the unnamed financial mathematics and interest-rate mathematics texts supply the foundational rate conventions and curve methods. Use subject headings rather than unverified chapter numbers.

## Hull-centered reading strategy (edition independent)

**Use Hull as the central spine, not as a disconnected encyclopedia.** Because no edition was supplied, the mapping below is by subject rather than invented chapter or page number. Locate the corresponding subject headings in your copy.

| Sequence | Hull topics to locate | Financial-math overlay | Produced output |
|---|---|---|---|
| Modules 1–3 | Mechanics of markets, contracts, arbitrage | Interest-rate conventions, discount factors, day counts | Cash-flow and payoff engine |
| 4–6 | Futures/forwards, interest-rate contracts, swaps | Term structure, forwards, duration/convexity | Hedge pricing and curve notebook |
| 7–8 | Option markets, strategies, binomial models | State prices, discrete compounding | Payoff plotter and tree engine |
| 9–10 | Wiener processes, Itô, Black–Scholes–Merton | Stochastic calculus, PDE tools | Independent BSM derivation and code |
| 11–12 | Greeks, volatility, numerical procedures | Approximation methods and error analysis | Hedging simulation and price-validation report |
| 13–15 | Hedging, securitization, credit/structured instruments where covered by your edition | Exposure aggregation and tail-risk math | Enterprise risk report |
| 16 | Interest-rate models and further quantitative topics if covered | Stochastic rates and yield-curve calibration | Optional research project |

**How to work a chapter:** first identify assumptions and settlement conventions; then reproduce a payoff diagram; derive the price; implement it; compare against an alternative method or limiting case; and finally describe the economic exposure that the instrument transfers.

## Core formula atlas and model interpretation

These are **model-dependent identities**, not general market laws. For assumptions, derivations and conventions, consult the respective modules.

**Discount factor** — flat continuously compounded rate:

$$
D(0,T)=e^{-rT}.
$$

**Continuously compounded forward rate** between $T_1$ and $T_2$:

$$
f(T_1,T_2)=-\frac{\ln[D(0,T_2)/D(0,T_1)]}{T_2-T_1}.
$$

**Equity forward** — known continuous dividend yield and idealized financing:

$$
F_0=S_0e^{(r-q)T}.
$$

**Currency forward** — spot quoted in domestic currency per unit foreign:

$$
F_0=S_0e^{(r_d-r_f)T}.
$$

**One-step binomial risk-neutral pricing weight** — under the stated model conditions:

$$
p^{\ast}=\frac{e^{r\Delta t}-d}{u-d}.
$$

**Geometric Brownian motion** — positive diffusion-model prices:

$$
dS_t=\mu S_t\,dt+\sigma S_t\,dW_t.
$$

**Itô's lemma** for a smooth function of GBM:

$$
df=\left(f_t+\mu S f_S+\frac12\sigma^2S^2f_{SS}\right)dt+\sigma S f_S\,dW.
$$

**Black–Scholes–Merton PDE** — frictionless continuous dynamic hedging:

$$
V_t+\frac12\sigma^2S^2V_{SS}+(r-q)SV_S-rV=0.
$$

**European put–call parity** — matched contracts and idealized assumptions:

$$
C-P=S_0e^{-qT}-Ke^{-rT}.
$$

**Bond local price sensitivity** — duration/convexity approximation:

$$
\frac{\Delta P}{P}\approx-D_{mod}\Delta y+\frac12 C(\Delta y)^2.
$$

**VaR and expected shortfall** are quantile and tail-loss measures, respectively, not equations independent of horizon, confidence level, distribution or estimation convention.

## Derivation and computation checkpoints

| Checkpoint | Minimum derivation | Independent computational check |
|---|---|---|
| Contract valuation | Replicate simple payoff using cash and underlying | Compare cash-flow tables across states |
| Term structure | Derive forward from discount factors | Reprice input bonds with bootstrapped curve |
| Option parity | Reproduce identical terminal payoffs | Verify call/put parity numerically |
| Binomial | Derive delta and $p^{\ast}$ | Convergence of multi-step tree |
| Itô | Derive $d\ln S$ for GBM | Simulated log-price moments |
| BSM | Derive delta-hedged PDE and its assumptions | BSM versus tree/Monte Carlo |
| Greeks | Differentiate analytic price as appropriate | Central finite-difference bump checks |
| Curve hedge | Calculate duration/DV01 exposure | Reprice under small and large curve shifts |
| Enterprise hedge | Map risk factor to instrument and ratio | Scenario P&L including basis and costs |

## Major distinctions to preserve

1. **Real-world vs risk-neutral probabilities.** Physical probabilities describe modeled outcomes; risk-neutral weights facilitate no-arbitrage pricing under suitable market assumptions. Their equality is not generally expected.
2. **Pricing vs forecasting.** BSM may assign a consistent arbitrage-free price without predicting a stock's expected realized return.
3. **Valuation vs P&L.** The present value of a contract differs from cash it pays on settlement, and collateral cash flows can differ from both.
4. **Hedging vs eliminating risk.** Imperfect maturity matching, volatility, basis, jumps, liquidity and credit risk produce residual exposure.
5. **Local vs global risk measures.** Delta and DV01 are local sensitivities; large or nonlinear changes require repricing and scenarios.
6. **Market price vs model value.** A calibrated price is conditional on data, conventions and assumptions; model agreement on a few instruments is not proof of correctness.
7. **Interest-rate risk vs credit risk.** Movements in a government yield curve and a firm's borrowing spread may be driven by different factors.
8. **Derivative notional vs economic loss.** Notional is a contractual reference amount, not automatically the amount at risk or contract value.

## Example case portfolio

### Case 1 — Foreign exchange hedge decision

A corporation must pay €10 million in six months. Define a domestic-currency-per-euro spot/forward convention; compare a forward hedge, a euro call, and no hedge; calculate premium-adjusted final USD costs across spot outcomes and explain opportunity costs and liquidity needs.

### Case 2 — Fixed versus floating financing

A manufacturer has floating-rate debt and wishes to limit cash-flow volatility. Analyze a pay-fixed swap, a purchased cap and an unhedged position. Use a rate curve, reset schedule and duration exposure; stress rates and funding spread independently.

### Case 3 — Options market-maker risk

Price a small European call/put book with BSM, report delta/gamma/vega, construct a delta hedge and simulate imperfect discrete rebalancing. Explain hedging slippage from transaction costs, volatility shocks and gaps.

### Case 4 — Structured-credit cash flow

Create a simplified pool of loans with scheduled repayments and losses. Allocate cash flows to senior and junior tranches under a clear waterfall and compare slow prepayment versus high-default scenarios. Discuss sensitivity to correlation and assumptions. The model is pedagogical, not a market-ready ABS valuation.

## Capstone — build a derivative pricing and risk-management toolkit

Design and document a toolkit covering a bond/discount curve, forward, fixed–floating swap, binomial option pricer, BSM European pricer, numerical Greek estimator and portfolio scenario evaluator. Apply it to a company exposed to FX, commodity and funding-rate shocks.

**Deliverables:**

1. A `conventions.md` file defining rates, dates, currency quotes, units and settlement assumptions.
2. A reproducible spreadsheet or Python/R/MATLAB notebook with independent pricing checks and unit tests.
3. Plots of option payoffs and hedge P&L, plus at least one BSM-versus-binomial convergence plot.
4. A risk register with factor exposures, hedge instruments, residual risks, margin/collateral assumptions and model limitations.
5. A 3–5 page case recommendation explaining cost-versus-protection tradeoffs and the risks that remain after hedging.

**Assessment rubric:** interest/cash-flow mathematics 20%; economic no-arbitrage reasoning 20%; stochastic/option derivations 20%; validation and numerics 15%; real-world hedge analysis 15%; clear reporting 10%.

## Reading boundaries and future course-file integration

**Grounded in user-supplied course description:** corporate/bank market risks from FX, interest, commodity and equity markets; options, forwards, futures, swaps, structured notes and asset-backed securities; risk-management cases. **Grounded in additional user context:** Hull textbook, financial/interest-rate mathematics resources, Black–Scholes, Itô and stochastic processes. **Explicit optional extensions:** complex curve frameworks, HJM, stochastic volatility, XVA, credit derivative technicalities, detailed finite differences and regulatory risk measurement. None are labeled as official course requirements without lecture evidence.

Once lecture slides, problems and named financial math books are supplied, replace the topic-only reading crosswalk with verified book chapters, lecture pages and assignments. Preserve instructor-specific model conventions when they differ from these generic benchmarks and document those differences rather than silently revising them.
