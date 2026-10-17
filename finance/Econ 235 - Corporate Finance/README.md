# ECON 235 — Corporate Finance

## Capital budgeting, corporate valuation, financing, and governance

**Purpose.** Apply financial theory to a firm's investment, financing, payout and governance decisions. The source basis is the supplied UCSC ECON 235 course description (capital budgeting, investment decisions, capital structure, dividends and governance) and the user's inventory (free cash flow, dividend growth, duration/maturity, NPV, operating cash flow, EBITDA, break-even analysis and the yield curve). This is a **source-informed self-study design**; no course slides, assigned chapters, company cases or assessments have been uploaded for ECON 235. Content marked **[Extension]** goes beyond the confirmed topic inventory.

[← Graduate Finance](../README.md) · [Finance I](../ECON-233-Finance-I/README.md) · [Financial Engineering](../ECON-236-Financial-Engineering/README.md)

## Learning outcomes

Upon completion, you should be able to:

- Translate accounting statements into operating cash flows, working-capital needs, capital expenditures, and free cash flow to the firm/equity.
- Value a bond, dividend stream, project, business or enterprise with rates and cash flows defined consistently.
- Evaluate independent and mutually exclusive projects using NPV, and interpret IRR, payback and break-even without misusing them.
- Estimate cost of equity, cost of debt and a defensible, capital-structure-consistent WACC.
- Analyze operating versus financial leverage and distinguish enterprise value from equity value.
- Explain how capital structure, financial distress, tax shields and agency incentives affect financing choices.
- Evaluate dividends and share repurchases without confusing accounting earnings with available cash.
- Interpret yield-curve changes, maturity, bond duration, convexity and exposure to changing interest rates.
- Produce an investment committee memorandum that reports assumptions, scenarios, stakeholder impacts and decision rules.

## Prerequisite bridges from Finance I

Review time value of money, effective vs nominal rates, risk premia, CAPM, beta, diversification, opportunity cost, market prices and financial instrument basics in [ECON 233](../ECON-233-Finance-I/README.md). Add basic financial statement literacy: balance sheets, income statements, cash flow statements, accruals vs cash, depreciation, tax effects, debt schedules and working capital. An engineering or OR reader should not skip the accounting bridge: a precisely optimized model built on inconsistent accounting definitions is unreliable.

### Notation, cash flow and valuation consistency

- $EBIT$: earnings before interest and taxes; operating earnings after depreciation/amortization when those expenses are included.
- $EBITDA$: earnings before interest, taxes, depreciation and amortization. **EBITDA is not cash flow**: it excludes CapEx, working capital, taxes and other cash needs.
- **Net operating profit after tax** under a simplified marginal tax rate $T$:

  $$
  NOPAT=EBIT(1-T).
  $$
- **Free cash flow to the firm** in the simplified unlevered operating framework (adjust non-operating items as required):

  $$
  FCFF=EBIT(1-T)+D\&A-CapEx-\Delta NWC.
  $$
- **Free cash flow to equity** under consistent definitions (not interchangeable with FCFF):

  $$
  FCFE=NetIncome+D\&A-CapEx-\Delta NWC+NetBorrowing.
  $$
- **Enterprise value (EV)** reflects value of operations to capital providers; **equity value** is the residual after debt and other claims plus applicable non-operating assets and adjustments. Do not conflate EV and market capitalization.
- Match **nominal cash flows with nominal discount rates**, **real with real**, and cash-flow currency with discount-rate currency.

## Learning dependency map

```mermaid
flowchart LR
 A[Accounting & cash-flow definitions] --> B[Discounting & term structure]
 A --> C[Operating forecast / FCFF]
 B --> D[Security & project valuation]
 C --> D
 E[CAPM / risk premia from ECON 233] --> F[Cost of capital & WACC]
 F --> D
 D --> G[NPV / capital budgeting]
 G --> H[Capital structure / payout]
 H --> I[Governance & liquidity]
 B --> J[Duration & rate risk]
 I --> K[Integrated corporate recommendation]
 J --> K
```

## Structured modules

### Module 1 — Corporate objective, financial statements, and sources of value

**Questions:** What exactly does a company maximize? Who claims its cash flows? What distinguishes profit from cash?

**Concepts:** enterprise vs shareholder value; stakeholders and contractual claims; accounting profit, EBIT, EBITDA, net income, depreciation, operating cash flow, free cash flow; book vs market values; invested capital; cash conversion; governance, agency and corporate objectives.

**Core exercise:** take a simplified income statement, cash flow statement and balance sheet; reconcile net income to cash from operations and then to FCFF. Show explicitly why EBITDA is not the amount available to all capital providers.

**Application:** compare a fast-growing company with positive EBITDA but negative FCFF to a mature company with positive FCFF. Explain how growth and working capital can consume cash.

**Mastery:** explain the distinction between an accounting improvement, a working-capital release, and a genuine increase in long-run value.

### Module 2 — Interest-rate mathematics, debt instruments and yield curves

**Concepts:** discounting, compounding, spot and forward rates, coupon bonds, yield to maturity, price–yield inverse relation, maturity, term structure, reinvestment versus price risk, borrowing conventions, floating vs fixed-rate debt.

**Relationships:** For fixed coupon payments $C_t$ and face value $F$, with a yield $y$ matched to the payment frequency,

$$
P=\sum_{t=1}^{T}\frac{C_t}{(1+y)^t}+\frac{F}{(1+y)^T}.
$$

With a term structure of cash-flow-specific zero-coupon discount factors $D(0,t)$,

$$
P=\sum_t CF_t D(0,t).
$$

Specify the appropriate day-count and cash-flow timing conventions.

**Application:** price a bond using a flat yield and a non-flat discount curve. Change rates and see why a long-maturity, low-coupon security generally has greater rate sensitivity.

**Mastery:** distinguish coupon rate, current yield, yield to maturity, spot rate, forward rate and an effective borrowing cost after issuance fees.

### Module 3 — Operating forecasts, free cash flow, and terminal value

**Concepts:** sales drivers, pricing, costs, taxes, operating margins, maintenance versus growth CapEx, depreciation schedules, net working capital, reinvestment, cash-flow timing, forecast horizon, terminal value, enterprise-to-equity bridge.

**Relationships:** In a steady-state perpetuity with $WACC>g$ and consistent assumptions, terminal value is

$$
TV_T=\frac{FCFF_{T+1}}{WACC-g}.
$$

With an explicit $T$-period forecast in the usual end-of-period discrete-time setup,

$$
EV_0=\sum_{t=1}^{T}\frac{FCFF_t}{(1+WACC)^t}+\frac{TV_T}{(1+WACC)^T}.
$$

**Application:** create a five-year operating model, with a working-capital schedule and explicit maintenance/growth CapEx. Reconcile implied long-run growth to reinvestment, capacity and the economy's constraints; compare terminal growth and exit multiple methods **[Extension]**.

**Mastery:** identify why applying a high growth rate forever or mixing unlevered FCFF with cost of equity overstates value.

### Module 4 — Capital budgeting and investment decision rules

**Concepts:** sunk vs incremental cost, opportunity costs, cannibalization, taxes, depreciation shields, net working capital recovery, NPV, IRR and multiple IRRs, modified IRR **[Extension]**, profitability index, simple/discounted payback, mutually exclusive projects, equivalent annual cost/annuity, economic lives, capital rationing.

**Key rule:** For cash flows $CF_t$ and an appropriate per-period discount rate $k$,

$$
NPV=\sum_{t=0}^{T}\frac{CF_t}{(1+k)^t}.
$$

The initial cash flow $CF_0$ is often negative. Under the model's assumptions, accept independent positive-NPV projects; for mutually exclusive projects, compare NPVs while accounting for differing scale and timing.

**Application:** compare two pieces of equipment with different upfront costs and lifetimes. Calculate incremental NPV and equivalent annual cost where repeatable replacement assumptions hold. Explain when a higher IRR can accompany a lower NPV.

**Mastery:** construct a project cash-flow table without including interest expense and then discounting again at WACC (which would double count financing).

### Module 5 — Operating leverage, financial leverage, and break-even analysis

**Concepts:** fixed and variable costs, accounting and cash break-even, contribution margin, degree of operating leverage, debt-related financial leverage, margin of safety, sensitivity of profit/FCF to sales volume.

**Relationships:** For price $p$, variable unit cost $v$, and fixed operating cost $F$, with $p-v>0$,

$$
Q_{BE}=\frac{F}{p-v}.
$$

In a simple cost–volume–profit setting, the degree of operating leverage (away from break-even) is

$$
DOL=\frac{Q(p-v)}{Q(p-v)-F}.
$$

Operating leverage is a business-cost property; financial leverage relates to debt and the claims structure.

**Application:** model a company with higher fixed costs but lower variable costs versus a flexible producer. Stress demand, wages and financing rates separately. Distinguish EBIT break-even from zero free-cash-flow break-even.

**Mastery:** explain how a firm can have low debt but still face high economic risk due to fixed operating commitments.

### Module 6 — Cost of capital, CAPM, and WACC

**Concepts:** opportunity cost of capital, business versus financial risk, project beta, equity/debt market weights, tax shields, marginal cost of debt, risk-free rate and equity risk premium estimation, WACC versus APV **[Extension]**.

**Relationships:** CAPM provides an estimate of the cost of equity within its assumptions:

$$
r_E=r_f+\beta_E\left(E[R_M]-r_f\right).
$$

For a basic debt/equity capital structure with market-value weights, a usable interest tax shield and tax rate $T$,

$$
WACC=\frac{E}{D+E}r_E+\frac{D}{D+E}r_D(1-T).
$$

Financing policy, project risk and tax treatment may require adjustments.

**Application:** estimate WACC for a firm with traded debt and equity. Revalue the same operating cash flows under different risk assumptions while avoiding treating a firm's average WACC as universal for all projects.

**Mastery:** why is a project riskier than the company potentially assigned a higher required rate, even when financed from existing cash?

### Module 7 — Capital structure and financing policy

**Concepts:** Modigliani–Miller benchmark under restrictive assumptions, leverage and required equity return, interest tax shields, distress costs, debt overhang, trade-off and pecking-order theories **[Extension]**, asymmetric information, financing flexibility, covenant capacity and maturity choice.

**Derivation focus:** show why redistributing a fixed operating payoff across debt and equity does not necessarily create operating value in a frictionless no-tax benchmark, and how taxes, default costs and agency effects can change the conclusion. Distinguish equity-value effects from enterprise-value effects.

**Application:** compare an all-equity, moderately levered and highly levered firm using interest coverage, liquidity and scenario losses. Estimate the costs and benefits of added leverage rather than assuming debt always reduces WACC.

**Mastery:** identify when the tax benefits of borrowing are not fully usable or are outweighed by expected distress costs.

### Module 8 — Dividend growth, equity valuation, and payout policy

**Concepts:** dividend discount models, constant-growth Gordon model, multi-stage dividend valuation, payout ratio, retained earnings and reinvestment, dividends versus buybacks, signaling/information, payout irrelevance benchmark and practical frictions.

**Relationship:** For a sustainable constant-growth dividend stream with $r_E>g$,

$$
P_0=\frac{D_1}{r_E-g}.
$$

Growth does not create value automatically: financing, reinvestment and returns on incremental capital matter.

**Application:** value a stable dividend-paying firm and a growth company that reinvests cash. Explain when an FCFF model is more meaningful than a dividend growth model; model payout scenarios under limited cash.

**Mastery:** explain why retaining all earnings does not automatically increase firm value, and why dividends should not be modeled independently of investment and financing constraints.

### Module 9 — Governance, agency, and investment incentives

**Concepts:** separation of ownership and control, managerial incentives, agency costs, principal–agent problems, conflicts between shareholders and creditors, financing contracts, managerial option compensation, board oversight and incentive compatibility. These connect directly to the user's ECON 200 game theory and mechanism design foundations.

**Application:** analyze an executive proposal for an acquisition that increases the manager's control but has a negative expected NPV. Design a decision process using independent valuation, incentive limits and performance benchmarks.

**Mastery:** show how a corporate policy might optimize a manager's objective while reducing total enterprise value.

### Module 10 — Duration, convexity, yield curves, and asset–liability choices

**Concepts:** maturity, Macaulay and modified duration, effective duration when cash flows change with rates, DV01, convexity, refinancing risk, term-structure shapes, fixed/floating debt, interest-rate exposure and duration matching. Advanced interest-rate hedging is studied in ECON 236.

**Approximations:** For duration $D_{mod}$, convexity $C$ and an appropriately quoted yield change $\Delta y$,

$$
\frac{\Delta P}{P}\approx-D_{mod}\Delta y+\frac12 C(\Delta y)^2.
$$

For a one-basis-point parallel move, an approximate value change in price-currency units is

$$
DV01\approx P\,D_{mod}\times10^{-4}.
$$

Duration and convexity must be defined under the same yield convention; both are local approximations.

**Application:** simulate rate increases, decreases and nonparallel curve shifts for the firm's outstanding debt. Compare short fixed, long fixed and floating finance, including reset and refinancing risk. Compute duration by cash-flow weighting and verify with finite differences.

**Mastery:** why does matching duration at a point in time not fully hedge a liability when the yield curve twists or cash flows are option-dependent?

### Module 11 — Risk, liquidity, and uncertainty in corporate decisions

**Concepts:** scenario analysis vs simple one-variable sensitivity, business cycle exposure, liquidity buffers, covenant stress, hedging versus speculation, financing flexibility, risk management and value creation. Risk-neutral probability in derivatives pricing is not a company's subjective demand forecast.

**Application:** build base, upside and downside cases for operating cash flow, rates and currency. Identify which exposures are operational and which can be contractually hedged. Map hedge implementation to ECON 236 forwards, options, futures and swaps.

**Mastery:** explain why hedging a currency translation effect may not hedge long-run competitive exposure and why a hedge can create short-run collateral demands.

## Synthesis: how corporate finance joins the other courses

| Corporate question | Input from ECON 233 | Tool in ECON 235 | Complement in ECON 236 |
|---|---|---|---|
| What is the required return? | CAPM, systematic risk and risk premia | Cost of equity, WACC and project discount rate | Separate hedge pricing from required corporate returns |
| Should the firm build a plant? | Uncertainty and diversification | Incremental cash flows, NPV, real/nominal consistency | Hedges for FX, rates and commodity inputs |
| Should it borrow long or short? | Bond returns and risk | Term structure, duration, financing flexibility | Swaps, futures and option-based rate protection |
| What is the value of the shares? | Risk premia and market pricing | FCFF/FCFE/DDM and equity bridge | Embedded options, warrants and convertible claims **[Extension]** |
| How should management handle risks? | Risk budgets and preferences | Liquidity, covenants, governance | Quantitative hedge ratios and stress tests |

## Modeling and quality-control checklist

- [ ] Forecast cash flows are **incremental**, after tax, and dated correctly.
- [ ] CapEx and changes in working capital are included, not substituted by EBITDA.
- [ ] Free cash flow definition is matched to discount rate and valued claim.
- [ ] Terminal growth and reinvestment economics are feasible and consistent.
- [ ] Capital structure, tax treatment and WACC assumptions are explicit.
- [ ] Values are reconciled between enterprise, debt and equity claims.
- [ ] Mutually exclusive investments are compared on NPV, not IRR alone.
- [ ] The conclusions survive material price, volume, margin, discount-rate and financing shocks.
- [ ] Liquidity, covenant, and governance risks are considered.
- [ ] A decision-maker can identify the assumptions that would reverse the recommendation.

## Final project — corporate investment and financing memo

Evaluate a proposed expansion at a fictional or public company. Build five years of operating and working-capital forecasts plus a defendable terminal value. Derive FCFF, estimate a market-based cost of capital, calculate enterprise/project NPV and compare three funding structures. Add a dividend/buyback choice and assess whether the financing plan can survive a downside revenue case and a yield-curve shock. Submit: (1) auditable spreadsheet/notebook; (2) EV-to-equity bridge; (3) scenario/sensitivity table; and (4) 2–4 page investment committee memo with decision, uncertainties and governance risks.

**Self-assessment rubric:** cash-flow mechanics 25%; valuation and rates 25%; financing/capital structure 20%; scenario robustness 15%; clarity and institutional realism 15%.

## Suggested six-week sequence

| Week | Modules | Submission |
|---|---|---|
| 1 | 1–2 | Cash-flow reconciliation and bond valuation |
| 2 | 3–4 | FCFF forecast and incremental NPV |
| 3 | 5–6 | Break-even and WACC sensitivity |
| 4 | 7–8 | Capital structure and payout memo |
| 5 | 9–11 | Governance, duration and enterprise risk scenarios |
| 6 | Final project | Investment committee report |

## Source and reading status

**Specified:** ECON 235 description and the listed applied topics: free cash flow, dividend models, duration, maturity, NPV, operating cash flow, EBITDA, break-even analysis, the yield curve, capital structure, capital budgeting, payout and governance. **Proposed but not specifically documented by lecture files:** APV, modified IRR, advanced valuation multiples, expanded pecking-order material, real options and quantitative hedge case designs. No claimed lecture pagination or textbook assignments appear here because the necessary materials have not been supplied.

**Next reading:** [ECON 236 Financial Engineering](../ECON-236-Financial-Engineering/README.md) develops the contracts that can hedge the interest-rate, FX, commodity and equity risks identified here.
