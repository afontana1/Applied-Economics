# How Economists Measure the Value of Things That Don't Have a Price

**A comprehensive learning guide to revealed preferences, nonmarket valuation, environmental economics, the value of human life, and their philosophical limits**

*Prepared October 15, 2026. All dollar examples are illustrative unless explicitly identified as agency guidance. This guide preserves the complete substantive overview developed in the conversation and expands it with a curated learning bibliography for each section.*

## How to use this guide

Read Sections 1–3 for the theory and empirical methods, Section 4 for environmental applications, Section 5 for health and mortality-risk valuation, and Sections 6–8 for interpretation and policymaking. Each section ends with **Resources for further study**, organized roughly from accessible introductions to technical and primary materials. Some academic articles and books may require library access. Formulas use display-style LaTeX delimiters (`$$...$$`) for compatibility with Markdown renderers supporting mathematical typesetting.

## Contents

1. [The theoretical foundation: What is economic value?](#1-the-theoretical-foundation-what-is-economic-value)
2. [Revealed preference: Inferring value from actual behavior](#2-revealed-preference-inferring-value-from-actual-behavior)
3. [Stated preference: When relevant behavior does not exist](#3-stated-preference-when-relevant-behavior-does-not-exist)
4. [Matching methods to environmental values](#4-matching-methods-to-environmental-values)
5. [Valuing human life and health](#5-valuing-human-life-and-health)
6. [The deeper methodological problems](#6-the-deeper-methodological-problems)
7. [Putting the methods together: An Arctic oil-spill assessment](#7-putting-the-methods-together-an-arctic-oil-spill-assessment)
8. [Preference, welfare, and moral judgment](#8-preference-welfare-and-moral-judgment)
9. [Cross-section reading pathway and consolidated references](#9-cross-section-reading-pathway-and-consolidated-references)

---

## 1. The theoretical foundation: What is economic value?

The key idea connecting these methods is that **economists can infer how much people value something by observing what they are willing to sacrifice to obtain it, preserve it, or avoid losing it**. But there are important differences between observing an actual sacrifice, asking someone what they *would* sacrifice, and estimating the economic consequences of losing something.

This framework largely follows the U.S. Environmental Protection Agency's *2024 Guidelines for Preparing Economic Analyses*, supplemented by NOAA's environmental damage-assessment methods and the health economics literature.

### 1.1 Revealed preference versus stated preference

An important technical clarification: **contingent valuation and hypothetical discrete choice experiments are *stated preference* methods, not revealed preference methods.**

| Approach | What is observed | Illustrative examples |
|---|---|---|
| **Revealed preference** | What people actually choose or do | Buying homes, accepting dangerous jobs, traveling to national parks |
| **Stated preference** | Responses about what people would choose under a specified hypothetical situation | Willingness-to-pay surveys, hypothetical conservation choices |
| **Indirect and accounting-based approaches** | Economic consequences, expenditures, model-based replacements, or estimates from other studies | Avoided damage costs, restoration costs, benefit transfer |

The third category can be useful **without necessarily revealing people's full willingness to pay**.

A **discrete choice model** can fall into either of the first two categories. Observing which beach people actually visit gives revealed-preference data. Asking respondents to choose between hypothetical beaches in a survey gives stated-preference data. The mathematical choice model may be similar; the source of data differs.

### 1.2 Willingness to pay and willingness to accept

Two fundamental measures are:

- **Willingness to pay (WTP):** The maximum amount someone would give up to obtain an improvement or avoid a loss.
- **Willingness to accept (WTA):** The minimum compensation someone would require to accept a deterioration or surrender a benefit.

These measures are **not generally identical**.

Imagine someone values protecting a wildlife habitat. Asked how much they would pay to protect it, they say $200. Asked how much compensation they would require to agree to its destruction, they say $2,000. Both answers could reflect that individual's preferences.

Why might they differ? Income constraints, available substitutes, reference points, perceived entitlements, and the endowment effect can all matter.

In formal welfare economics, these questions connect to **compensating variation** and **equivalent variation**: monetary changes that make someone indifferent between specified welfare situations.

Importantly, WTP is constrained by **ability to pay**. Someone with little money might intensely prefer the protection of an ecosystem without being able to offer a large payment.

### 1.3 Consumer surplus

The market price is not necessarily the total value received from consuming something.

Suppose you're willing to pay $100 to visit a national park, but admission and other relevant costs total $35. Your net benefit, or consumer surplus, is $65 in this simple example.

At the market level, economists estimate consumer surplus from a demand curve:

$$
CS=\int_0^{Q^*}P(Q)\,dQ-P^*Q^*
$$

In words, **total willingness to pay minus actual expenditure**.

#### Consumer surplus, illustrated

<!-- Self-contained original figure; some Markdown readers may suppress inline SVG. -->
<svg viewBox="0 0 340 225" width="680" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="A downward-sloping demand curve above a horizontal market price. The triangular area between demand and price represents consumer surplus, and the rectangular area beneath price represents expenditure.">
  <line x1="47" y1="12" x2="47" y2="190" stroke="currentColor" stroke-width="1.6"/>
  <line x1="47" y1="190" x2="326" y2="190" stroke="currentColor" stroke-width="1.6"/>
  <path d="M47 28 L252 190 L47 190 Z" fill="#94A3B8" fill-opacity="0.12"/>
  <path d="M47 28 L171 126 L47 126 Z" fill="#16A34A" fill-opacity="0.24"/>
  <path d="M47 126 L171 126 L171 190 L47 190 Z" fill="#64748B" fill-opacity="0.19"/>
  <line x1="47" y1="126" x2="302" y2="126" stroke="#64748B" stroke-width="1.7" stroke-dasharray="5 4"/>
  <line x1="47" y1="28" x2="252" y2="190" stroke="#0284C7" stroke-width="2.8"/>
  <line x1="171" y1="126" x2="171" y2="190" stroke="currentColor" stroke-dasharray="4 4" stroke-opacity="0.5"/>
  <text x="83" y="93" fill="#15803D" font-size="12" font-weight="600">Consumer surplus</text>
  <text x="75" y="161" fill="currentColor" font-size="12">Expenditure</text>
  <text x="203" y="101" fill="#0284C7" font-size="12">Demand</text>
  <text x="9" y="18" fill="currentColor" font-size="11">Price</text>
  <text x="281" y="211" fill="currentColor" font-size="11">Quantity</text>
  <text x="12" y="130" fill="currentColor" font-size="11">P*</text>
  <text x="162" y="207" fill="currentColor" font-size="11">Q*</text>
</svg>

*Simplified linear demand: the triangle represents consumer surplus at the prevailing price and quantity; the rectangle represents expenditure.*

This matters for environmental valuation. Revenue from park entrance fees may substantially understate visitors' welfare benefits.

For sufficiently large changes, economists typically use more precise **Hicksian welfare measures** rather than relying exclusively on the ordinary demand-curve surplus approximation.

### Resources for further study — Section 1

1. **[U.S. EPA, *Guidelines for Preparing Economic Analyses*, 3rd ed. (2024)](https://www.epa.gov/environmental-economics/guidelines-preparing-economic-analyses-3rd-edition)** — **Start here.** Read **Appendix A: Economic Theory** and **Chapter 7: Analyzing Benefits** for WTP, WTA, consumer surplus, and welfare measurement; the EPA page links each chapter separately.
2. **[NOAA, *Economic Valuation of Natural Resources: A Handbook for Coastal Resource Policymakers*](https://coastalscience.noaa.gov/data_reports/economic-valuation-of-natural-resources-a-handbook-for-coastal-resource-policymakers/)** — Accessible treatment of why market prices and observed expenditures differ from economic value, with coastal case studies.
3. **[National Research Council, *Valuing Ecosystem Services: Toward Better Environmental Decision-Making* (2005)](https://www.nationalacademies.org/publications/11139)** — Read the conceptual chapters before the technical methods chapter; especially useful for linking ecological changes to economic welfare.
4. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*, 3rd ed. (2014)](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Graduate-level reference. Focus on Chapters 2–4 on welfare definitions and measurement. *Book; purchase or library access may be needed.*
5. **[OECD, *Handbook of Biodiversity Valuation* (2002)](https://www.oecd.org/content/dam/oecd/en/publications/reports/2002/03/handbook-of-biodiversity-valuation_g1gh28a2/9789264175792-en.pdf)** — Introduces total economic value and the distinction between use and nonuse values. Read its discussion of the total economic value framework.

**Check your understanding:** Why would entrance-fee revenue understate a national park's recreational value? How can two WTP and WTA measures for the same habitat differ without being logically contradictory?

---

## 2. Revealed preference: Inferring value from actual behavior

These are the major methods used in environmental and welfare economics.

### 2.1 Overview of methods

| Method | What economists observe | What it can reveal |
|---|---|---|
| Market demand estimation | Quantities purchased at different prices | Consumer surplus and WTP for market goods |
| Travel cost method | Expenditure and time spent visiting locations | Recreational value of natural sites |
| Random utility / site choice models | Which alternative people select | Value of environmental attributes and substitutes |
| Hedonic property pricing | Home prices and environmental amenities | Implicit prices of clean air, quiet, views, safety |
| Hedonic wage-risk models | Compensation and occupational hazards | WTP to accept or reduce mortality risk |
| Averting / defensive behavior | Spending to avoid pollution or risk | Value of reducing exposure or harm |
| Production function methods | Environmental inputs and market output | Value of ecological contributions to production |
| Residential sorting models | Where people choose to live | Preferences for local public goods and environmental quality |
| Incentivized experiments | Real purchases, bids, or paid choices | WTP under experimentally controlled conditions |

The EPA explicitly identifies production functions, travel costs, hedonic pricing, and averting behavior as core revealed-preference families. Several other methods are applications, extensions, or experimental implementations of related principles.

### 2.2 Travel cost method

This is one of the foundational methods of environmental economics.

Suppose the government wants to estimate the recreational value of Yellowstone National Park. Admission fees alone won't tell us how much people value visiting. But people reveal something about their preferences through their willingness to travel.

Imagine a visitor spends $150 on gas and transportation and incurs $200 in opportunity costs from travel time. The effective price of visiting Yellowstone is approximately $350, even if admission is inexpensive.

Economists collect data on where visitors live, how frequently they visit, the cost of getting there, their incomes, and available alternatives. They estimate something resembling a demand function:

$$
Q_i=f(TC_i,Y_i,S_i,X_i)
$$

Where trip quantity is a function of travel cost, income, substitutes, and other characteristics. The slope of the estimated demand relationship helps recover recreational consumer surplus.

**Best applications:** National parks, fishing, beaches, hiking, wildlife tourism, and other destinations people incur costs to visit.

**Limitation:** The method cannot directly reveal how much someone values Yellowstone's existence if they never plan to visit. It also requires careful treatment of multi-purpose trips, travel time, and the possibility that traveling itself is enjoyable.

### 2.3 Random utility models (RUM)

This is a more flexible version of recreation-demand modeling. Rather than merely counting trips, economists examine choices among alternatives.

Suppose a family chooses between three beaches:

| Beach | Travel cost | Water quality |
|---|---:|---|
| A | $15 | Poor |
| B | $40 | Good |
| C | $75 | Excellent |

If the family consistently chooses B over A, they're apparently willing to incur an additional $25 in travel costs for the bundle of characteristics associated with B. But that **does not prove** they value the water-quality difference at exactly $25: other beach characteristics could matter.

Economists therefore model utility as a function of observable attributes:

$$
U_{ij}=\beta_q q_j-\beta_c c_{ij}+\varepsilon_{ij}
$$

Here, environmental quality enters positively, monetary/travel cost negatively, and an unobserved component captures remaining determinants of choice. The estimated coefficients can be translated into **marginal willingness to pay** for quality changes.

The value of preserving one beach depends on the availability and quality of others. If five excellent substitutes exist nearby, losing one beach may impose a smaller recreational welfare loss than losing the only accessible beach.

Economists use **conditional logit**, **nested logit**, and **mixed logit** models. Mixed logit models allow preference heterogeneity and more flexible substitution patterns.

The method is frequently used for fishing-site selection and evaluating recreational losses after oil spills.

### 2.4 Hedonic pricing

Hedonic pricing begins from the observation that a differentiated product's price reflects a bundle of attributes.

A house is not simply purchased for its square footage. Its price also reflects location, school quality, crime, neighborhood characteristics, air quality, noise, flood exposure, and proximity to parks.

An economist might estimate:

$$
P_i=\alpha+\beta_1 SqFt_i+\beta_2 Air_i+\beta_3 School_i+\cdots+\varepsilon_i
$$

The air-quality coefficient can reveal how much additional housing expenditure buyers are willing to accept for cleaner air at the margin.

Imagine two otherwise comparable houses. One experiences heavy industrial pollution and the other does not. If the cleaner location reliably commands a $40,000 premium, the difference may reveal a **capitalized environmental amenity value**.

Two qualifications matter:

1. Establishing comparability is difficult. Pollution may correlate with income, crime, schools, employment opportunities, or unobserved neighborhood characteristics.
2. A house-price premium measures a particular capitalization of benefits, **not necessarily the entire social value** of clean air.

Researchers use repeat-sales models, difference-in-differences, regression discontinuity, instrumental variables, and other quasi-experimental designs to isolate environmental effects.

### 2.5 Averting behavior and defensive expenditures

Suppose households buy expensive water filtration systems because their tap water is contaminated. Their decision to spend money avoiding contamination reveals that cleaner water has value.

Economists study air filters, water treatment, protective equipment, and costs incurred when avoiding dangerous locations.

But someone buying a $400 water filter may value improved taste, convenience, and perceived health protection simultaneously. Economists need a model isolating pollution-related benefits from other attributes.

**Expenditure is not necessarily equal to total WTP.** Someone might spend $400 on protective equipment yet be willing to pay substantially more to eliminate the hazard entirely. Other people may be unable to afford protection despite valuing it highly.

Well-specified averting-behavior models can estimate welfare effects; merely summing defensive expenditures generally cannot.

### 2.6 Production functions and ecosystem services

This method is especially important for **indirect environmental use values**.

Suppose wetlands support fish populations through nursery habitat. Fishery production might depend on labor, capital, and an ecosystem input:

$$
Q=f(L,K,E)
$$

The marginal contribution of the ecosystem is:

$$
\frac{\partial Q}{\partial E}
$$

Under appropriate competitive-market and marginal-change assumptions, its monetary contribution can be approximated by:

$$
MV_E=P_Q\frac{\partial Q}{\partial E}
$$

Suppose wetland restoration increases commercially valuable fish production by 20,000 kilograms per year. At $8 per kilogram, that is $160,000 in additional **gross output**.

But it is **not automatically $160,000 in net welfare**. Catching, processing, and marketing fish cost money, and additional supply may affect prices. Proper valuation accounts for changes in producer and consumer surplus, costs, and ecological interactions.

The same principle applies to pollination, soil productivity, water quality, and ecosystem services contributing to market production.

### 2.7 Residential sorting and equilibrium models

These extend hedonic pricing by modeling households' decisions about **where** to live, not merely the observed relationship between house prices and amenities.

Imagine improved air quality in one neighborhood. Some households move toward it; housing prices and rents rise; residents and landlords respond; the population's spatial distribution changes.

A sorting model attempts to recover household preferences while accounting for these equilibrium responses.

It is particularly useful for **large environmental changes** that alter behavior and prices, and for examining valuations by income, household composition, and other characteristics.

### 2.8 Real economic experiments

Economists can construct markets in which subjects face **real financial consequences**.

Researchers may use incentive-compatible auctions or suitably designed purchase mechanisms to estimate WTP for a product characteristic. Participants may actually have to purchase the item at a price determined by the experiment.

A well-known approach is the **Becker–DeGroot–Marschak (BDM)** mechanism, under which truthful bidding is optimal given the mechanism's assumptions.

Field experiments can vary prices, information, or access and observe resulting purchases.

These approaches offer unusually strong evidence for experimentally tractable goods, but they are much harder to implement for large public goods such as preserving an entire Arctic ecosystem.

### 2.9 Classical revealed preference: Samuelson and Afriat

There is a foundational meaning of *revealed preference* in microeconomics.

Paul Samuelson's revealed-preference approach asks whether observed choices are **consistent with a stable preference ordering**.

If a consumer could afford A or B but chooses A, A is **directly revealed preferred** to B, assuming the choice reflects preferences.

With observations at different prices and incomes, economists test restrictions such as the **Generalized Axiom of Revealed Preference (GARP)**. **Afriat's theorem** establishes conditions under which a finite dataset of choices can be rationalized by a well-behaved utility function.

This is different from environmental nonmarket valuation, but it provides part of the intellectual foundation.

Even if observed behavior is consistent with a utility function, that does not identify a unique cardinal measure of human well-being.

### Resources for further study — Section 2

1. **[EPA, Guidelines, Chapter 7: Analyzing Benefits](https://www.epa.gov/system/files/documents/2024-12/chapter-7-guidelines-for-preparing-economic-analyses_final_508-compliant.pdf)** — Primary practical overview of production, travel costs, hedonic models, and averting behavior. Read the revealed-preference subsections and method-selection discussion.
2. **[NOAA, Natural Resource Valuation Methods](https://darrp.noaa.gov/economics/valuation)** — Clear, short applications of revealed-preference tools to environmental damage and lost recreation.
3. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Read Chapters 8–11 for production, recreation demand, property-value models, and hedonic wage models. *Technical book.*
4. **[Kenneth Train, *Discrete Choice Methods with Simulation*, 2nd ed. (2009)](https://eml.berkeley.edu/books/choice2.html)** — **Free online chapters**; start with Chapters 1–3 and progress to Chapters 4, 6, and 13 for nested/mixed logit and endogeneity.
5. **[Sherwin Rosen, “Hedonic Prices and Implicit Markets” (1974)](https://doi.org/10.1086/260169)** — Seminal theory of implicit prices for attributes, including housing and other differentiated goods. *Journal access may be required.*
6. **[National Research Council, *Valuing Ecosystem Services*, Chapter 4](https://www.nationalacademies.org/read/11139/chapter/6)** — Integrates multiple revealed- and stated-preference methods in ecosystem applications, including pooling actual and hypothetical choice data.
7. **[Becker, DeGroot, and Marschak, “Measuring Utility by a Single-Response Sequential Method” (1964)](https://doi.org/10.1002/bs.3830090304)** — Foundational incentivized elicitation mechanism. *Technical article.*
8. **[S. N. Afriat, “The Construction of Utility Functions from Expenditure Data” (1967)](https://doi.org/10.2307/2525382)** — Theoretical foundation for rationalizing finite choice datasets. *Advanced theory.*

**Check your understanding:** Why can't one family choosing Beach B instead of Beach A identify the water-quality coefficient by itself? Why is a $40,000 housing premium not automatically the total value of cleaner air? What does GARP test, and what does it not tell us about cardinal utility?

---
## 3. Stated preference: When relevant behavior does not exist

Revealed preference works particularly well when a good affects observed market behavior. But return to the Arctic oil spill. Imagine an ecosystem so remote nobody visits it, lives nearby, or commercially exploits it. People may care deeply about preserving it, but **there may be no related market transaction** from which to infer those preferences.

This is where stated preference becomes indispensable.

### 3.1 Contingent valuation (CV)

The economist constructs a hypothetical but carefully specified market, for example:

> A conservation initiative would reduce the probability of serious damage to a remote Arctic marine ecosystem over the next 20 years. It would be funded by a compulsory annual household payment. Would you vote in favor if your household's annual cost were $80?

Different respondents receive different proposed costs. Researchers observe the proportion willing to vote yes at each cost level and estimate a distribution of WTP.

Modern contingent valuation commonly uses **referendum-style binary choices**, rather than simply asking respondents to invent a monetary number.

Its strength is the ability to estimate **passive-use or existence values**, including preferences that do not show up in purchases.

Its obvious problem is **hypothetical bias**: someone might support an $80 payment in a survey but oppose the tax in reality.

Researchers seek to address this with credible scenarios, realistic payment mechanisms, scope tests, comprehension checks, and calibration methods.

### 3.2 Discrete choice experiments (DCE)

These decompose the good or policy into attributes.

Imagine three proposed environmental policies:

| Attribute | Policy A | Policy B | No new policy |
|---|---|---|---|
| Arctic habitat restored | 50% | 80% | 0% additional |
| Wildlife recovery time | 20 years | 10 years | No additional restoration |
| Annual household tax | $45 | $110 | $0 |

Respondents choose among the options over several choice tasks. An analyst can estimate how much respondents trade money for different environmental changes.

For a simplified linear utility model:

$$
U=\beta_q q+\beta_c C+\varepsilon
$$

Marginal WTP for environmental quality is:

$$
MWTP=-\frac{\beta_q}{\beta_c}
$$

Here the estimated cost coefficient is typically negative.

Compared with a single contingent valuation question, a DCE can identify marginal values for multiple policy attributes. For example: **How much are respondents willing to pay for faster wildlife recovery, holding restored acreage constant?**

Choice experiments can be particularly valuable in designing restoration packages.

### 3.3 Other stated-preference and elicitation methods

| Method | How it works | Typical role |
|---|---|---|
| Contingent ranking | Rank several hypothetical alternatives | Infer relative preferences |
| Best–worst scaling | Identify the most and least desirable attributes or options | Estimate attribute importance and preference tradeoffs |
| Payment cards | Select a WTP range from monetary amounts | Alternative CV elicitation format |
| Open-ended valuation | State maximum WTP directly | Simple but vulnerable to response difficulties |
| Contingent behavior | State how actual behavior would change under a hypothetical scenario | Predict future recreation or consumption |
| Time trade-off | Choose between health duration and health quality | Estimate health-state utility |
| Standard gamble | Choose between a certain health state and a health-risk lottery | Estimate utility under uncertainty |

Some methods directly measure monetary WTP. Others initially measure **preferences or utilities** and require additional assumptions to translate them into money.

### 3.4 Combining stated and revealed preference

One increasingly useful strategy is to combine both sources of information.

For instance, economists can study which beaches people actually visit and survey those same people about how their choices would change if water quality improved in a future scenario.

Observed choices anchor the model in actual behavior. Hypothetical scenarios allow investigation of conditions not yet observed.

The difficult part is modeling differences in errors and behavior between hypothetical and actual choices. **The datasets should not simply be pooled as though they were identical.**

### Resources for further study — Section 3

1. **[EPA, Guidelines, Chapter 7](https://www.epa.gov/system/files/documents/2024-12/chapter-7-guidelines-for-preparing-economic-analyses_final_508-compliant.pdf)** — Read the stated-preference section, particularly elicitation formats, incentive compatibility, and interpreting welfare estimates.
2. **[NOAA Panel on Contingent Valuation (Arrow, Solow, et al.), Report (1993)](https://repository.library.noaa.gov/view/noaa/60900/noaa_60900_DS1.pdf)** — **Historically essential.** Recommendations and cautions about designing credible contingent-valuation surveys in the context of oil-spill damage assessment.
3. **[Carson and Groves, “Incentive and Informational Properties of Preference Questions” (2007)](https://doi.org/10.1007/s10640-007-9124-5)** — Why survey format, consequences, strategic incentives, and information matter. *Technical but central.*
4. **[Kenneth Train, *Discrete Choice Methods with Simulation*](https://eml.berkeley.edu/books/choice2.html)** — Use Chapters 2–6 to understand how discrete responses identify preferences and substitution patterns; also applicable to hypothetical choice surveys.
5. **[National Research Council, *Valuing Ecosystem Services*, Chapter 4](https://www.nationalacademies.org/read/11139/chapter/6)** — Stated-versus-revealed valuation and combining datasets, with discussion of passive-use valuation.
6. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Chapter 12, “Stated Preference Methods for Valuation,” plus its bibliography. *Technical book.*
7. **[NOAA, Natural Resource Valuation Methods](https://darrp.noaa.gov/economics/valuation)** — Short applied explanation of why surveys are sometimes needed to value nonuse ecosystem services.

**Check your understanding:** What does a referendum-style survey reveal that a visitor count cannot? Why is an opt-out/no-policy choice relevant to DCE validity? How would you test whether reported WTP increases with the amount of habitat protected (a scope test)?

---

## 4. Matching methods to environmental values

The intuition that distinct components of environmental value require different methods is correct. But the key subtlety is this: **value categories describe *why* something matters, while methods describe *how* we gather evidence about it.**

One method may capture multiple kinds of value, and one environmental change may generate multiple benefits.

### 4.1 The complete environmental valuation matrix

| Type of value | Preferred methods | Important limitations |
|---|---|---|
| **Direct use** — fishing, tourism, recreation | Market demand, travel cost, site choice models, production functions | Misses benefits unrelated to direct use |
| **Indirect use** — flood protection, pollination, water purification | Production functions, damage functions, hedonic pricing, avoided-damage models, stated preference | Ecological relationships and costs must be modeled correctly |
| **Option value** — preserving possible future use | Dynamic choice models, stated preference, real-options analysis | Future preferences, uncertainty, and irreversibility matter |
| **Existence value** — valuing something simply because it exists | Contingent valuation, discrete choice experiments | Generally cannot be isolated from ordinary revealed-preference data |
| **Bequest value** — protecting something for future generations | Contingent valuation, discrete choice experiments, intergenerational welfare models | Preferences about future beneficiaries are difficult to separate from other motives |

This broad classification is consistent with the OECD's **total economic value** framework. An additional possible category is **altruistic value**, meaning concern for others in the present generation; it is distinguished from bequest value, which emphasizes future generations.

### 4.2 Option value and quasi-option value

Option value deserves special attention.

Suppose a wilderness area contains an ecosystem with potentially valuable future medical, recreational, or scientific uses, but we do not yet know which uses will materialize.

Standard valuation might consider expected future benefits. However, preserving **the option to use the ecosystem after learning more** may provide additional value.

This connects to **quasi-option value**, arising from learning before an irreversible decision.

Imagine a company can destroy an ecosystem today or wait five years while scientists study it. Because destruction cannot be undone, **waiting can be economically valuable** even when immediate development has apparently positive expected net benefits.

This is connected to real-options theory in finance.

Strictly speaking, option value, quasi-option value, and expected future-use value are distinct concepts. They should **not automatically be added** as separate benefits, because doing so can double-count value.

### 4.3 Applying this to an Arctic oil spill

Suppose an oil spill damages Arctic waters, wildlife, fishing habitat, and a culturally significant landscape. An economist might build a damage assessment from several complementary methods:

1. **Commercial and ecological production losses.** Estimate lost fishery output, diminished ecosystem services, and net economic impacts with ecological production and market models.
2. **Recreational losses.** Use trip counts, travel costs, and recreation-demand models to estimate lost consumer surplus from cancelled or degraded visits.
3. **Passive-use losses.** Use stated-preference surveys to estimate households' WTP to avoid ecological injuries, including effects on people who never visit the Arctic.
4. **Restoration requirements.** Determine ecological services lost and how much restoration would compensate, including losses before full recovery.

**Avoid overlap.** If a stated-preference estimate already measures the total value of preventing a spill—including recreation—one cannot simply add all estimated recreation damages without determining which benefits are already included.

### 4.4 Habitat Equivalency Analysis (HEA): An alternative to pricing everything

Instead of asking, “Exactly how many dollars is the damaged ecosystem worth?”, a regulator can ask:

> **How much additional ecological restoration is required to compensate for the ecological services that were lost?**

This is the logic of **Habitat Equivalency Analysis (HEA)**.

The analysis compares the **quantity and timing of ecological service losses** with the quantity and timing of benefits from a proposed restoration project.

Suppose 100 acres of habitat lose 50% of their services for ten years. A restoration project elsewhere may need to provide enough additional habitat services over time to offset the loss, accounting for recovery, ecological equivalence, and discounting.

The responsible party might then be required to finance that restoration.

This is not simply a market-price estimate of the damaged ecosystem; it is a method for **scaling compensatory restoration** under specified assumptions.

NOAA uses this approach in natural resource damage assessments for oil spills, hazardous waste sites, and vessel groundings. Its service-to-service scaling commonly uses **discounted-service-acre-years (DSAYs)**. Related **Resource Equivalency Analysis (REA)** can scale animal or plant restoration, such as compensating for lost “duck-years.”

**Environmental compensation does not always require assigning an explicit monetary price to nature's intrinsic worth.** Sometimes the relevant question is what restoration would make the public whole under applicable legal and scientific assumptions.

**In the actual Deepwater Horizon natural resource damage assessment**, NOAA reports that it used **both revealed- and stated-preference methods** to estimate recreational and total-value losses. Recreational losses are a subset of the broader total-value calculation; the agency collected extensive survey and visitation data to assess how the spill altered public recreation and preferences. See [NOAA, “The True Cost of an Oil Spill: Q&A with a NOAA Economist”](https://response.restoration.noaa.gov/true-cost-oil-spill-qa-noaa-economist-0).

### 4.5 Related methods that are not direct preference measures

| Method | What it estimates | Caution |
|---|---|---|
| Replacement cost | Expense of replacing a lost ecological service | Replacement cost is not necessarily WTP |
| Avoided damage cost | Losses prevented by a protective ecosystem | Requires a credible causal damage model |
| Restoration cost | Expenditure needed to restore a resource | Restoration spending is a cost, not automatically the resource's total value |
| Benefit transfer | Values adapted from previous studies | Context and population may differ |
| Meta-analysis | Synthesizes results from valuation studies | Depends on study quality and comparability |
| Shadow pricing | Imputed social value of a scarce nonmarket resource | Depends on model assumptions and policy constraints |

Benefit transfer is common in regulatory analysis because an original valuation study for every environmental effect is often too expensive or time-consuming.

But the [National Research Council](https://www.nationalacademies.org/read/11139/chapter/2) cautions that replacement or treatment cost **does not establish the welfare benefit** of ecosystem services just because the cost can be calculated.

### Resources for further study — Section 4

1. **[OECD, *Handbook of Biodiversity Valuation* (2002)](https://www.oecd.org/content/dam/oecd/en/publications/reports/2002/03/handbook-of-biodiversity-valuation_g1gh28a2/9789264175792-en.pdf)** — Primary introduction to total economic value, direct/indirect use, option, existence, altruistic, and bequest values.
2. **[National Research Council, *Valuing Ecosystem Services* (2005)](https://www.nationalacademies.org/publications/11139)** — **Highly recommended** for integrating environmental science, welfare theory, valuation methodology, and policy. Chapter 4 is especially relevant.
3. **[NOAA, Habitat Equivalency Analysis](https://darrp.noaa.gov/economics/habitat-equivalency-analysis)** — Official explanation of service-to-service scaling, HEA, DSAYs, and resource equivalency analysis.
4. **[NOAA, *Natural Resource Damage Assessment: Scaling Compensatory Restoration Actions*](https://repository.library.noaa.gov/view/noaa/71576)** — Technical guidance on choosing restoration alternatives and matching compensatory restoration to lost services. *Advanced primary guidance; large PDF.*
5. **[NOAA, Natural Resource Valuation Methods](https://darrp.noaa.gov/economics/valuation)** — Applied comparison of monetary valuation and restoration scaling.
6. **[EPA, Guidelines, Chapter 7](https://www.epa.gov/system/files/documents/2024-12/chapter-7-guidelines-for-preparing-economic-analyses_final_508-compliant.pdf)** — Benefit transfer, endpoint matching, and methodological limitations.
7. **[IPBES, *Methodological Assessment of the Diverse Values and Valuation of Nature* (2022)](https://www.ipbes.net/the-values-assessment)** — Goes beyond narrow monetary valuation to compare multiple concepts of nature's value and practical decision frameworks.
8. **[NOAA, “The True Cost of an Oil Spill: Q&A with a NOAA Economist”](https://response.restoration.noaa.gov/true-cost-oil-spill-qa-noaa-economist-0)** — An accessible interview explaining why the Deepwater Horizon assessment used **both** revealed and stated preferences, and how restoration scaling differs from monetizing every lost service.
9. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Chapters 8–9 and 12 for primary valuation tools; Chapters 2–4 for value concepts. *Technical book.*

**Check your understanding:** Why might an isolated Arctic ecosystem have substantial value without any visitors? How does HEA differ from stating a dollar valuation? Which two estimates might overlap if one adds recreation benefits to a stated-preference estimate of total spill prevention benefits?

---

## 5. Valuing human life and health

This is one of the most philosophically interesting applications of revealed preference.

The essential distinction is among:

- the value of **reducing small mortality risks**;
- the value of **extending or improving life**; and
- the **economic costs associated with someone's death**.

They are different objects of measurement.

### 5.1 Value of a Statistical Life (VSL)

The underlying approximation is:

$$
VSL\approx\frac{\Delta WTP}{\Delta p}
$$

Here the denominator represents a small reduction in the probability of death, and the numerator represents willingness to pay for it.

Suppose someone is willing to pay $900 to reduce their annual mortality probability by 1 in 10,000:

$$
VSL=\frac{900}{0.0001}=\$9{,}000{,}000
$$

This is an illustrative **value of marginal mortality-risk reduction**, scaled to one expected death avoided.

In an actual agency illustration, the EPA considers 100,000 people, each willing to pay $100 for a reduction in individual mortality risk of 1 in 100,000. Together they would pay $10 million for a reduction corresponding to **one expected death avoided**. That is **not** the price of preventing the certain death of any identified individual.

The **U.S. Department of Transportation** lists **$14.2 million for analyses using a 2025 base year** in guidance effective **March 20, 2026**. Agencies can use different estimates and conventions; analysts must attend to the base year, inflation and real-income updates, and applicable guidance.

How do economists estimate VSL?

#### A. Hedonic wage-risk models

One of the most important methods examines compensation for occupational fatality risk.

Suppose two jobs require comparable skills and responsibilities. One pays an additional $1,000 annually but raises fatality risk by 1 in 10,000 per year.

The implied VSL is:

$$
VSL=\frac{\$1{,}000}{1/10{,}000}=\$10{,}000{,}000
$$

Economists often estimate a wage equation:

$$
\ln w_i=\alpha+\beta Risk_i+\gamma X_i+\varepsilon_i
$$

Where the additional variables include worker, job, industry, geographic, and other relevant controls. The wage-risk coefficient can be transformed into an implicit price of risk, depending on whether wages and risks are defined annually, hourly, or logarithmically.

The major issue is **causal identification**. Dangerous and safe jobs vary in many ways beyond fatality risk; workers differ in skill, education, risk preferences, bargaining power, and available opportunities; and workers may misunderstand their risk.

Naively comparing a firefighter's wage with an office worker's wage would **not** yield a credible VSL.

EPA guidance draws on both hedonic wage-risk and stated-preference literatures.

#### B. Consumer safety expenditures

A second approach examines purchases of safety-enhancing goods.

Imagine otherwise comparable cars, one of which has a feature that reduces the buyer's probability of dying in a crash. If buyers systematically pay more for the safer car, economists may infer WTP for mortality-risk reduction.

Other settings include protective equipment, smoke alarms, and risk-reducing products.

The challenge is separating safety from comfort, quality, convenience, and other attributes. Results also depend on whether consumers accurately understand the risk reduction.

#### C. Stated-preference mortality-risk studies

Economists can ask people to evaluate hypothetical safety policies or treatments differing in costs, mortality risks, and side effects. By varying these attributes, researchers estimate WTP for risk reductions.

These studies matter especially where observable markets do not match the policy risk—such as reducing mortality from long-term environmental exposure.

Small probabilities are difficult to understand. Surveys therefore use visual risk grids, frequency formats, and comprehension checks.

#### D. Summary of mortality-risk methods

| Method | Observed or elicited tradeoff | Principal limitation |
|---|---|---|
| Hedonic wage-risk | Compensation versus occupational mortality risk | Job characteristics and worker selection |
| Safety-product purchases | Money versus product-related risk reduction | Safety must be isolated from other attributes |
| Averting behavior | Effort or spending to avoid risk | Perceptions and joint benefits |
| Stated-preference surveys | Hypothetical money-risk tradeoffs | Hypothetical bias and risk comprehension |
| Choice experiments | Policies/products with differing risks and costs | Experimental design and assumptions |
| Meta-analysis | Synthesized estimates across studies | Comparability across risk contexts |

### 5.2 Value of a Statistical Life Year (VSLY)

VSL values marginal mortality-risk changes in terms of **expected deaths avoided**. But what if one intervention extends life by 40 years while another extends it by only two?

This motivates the **Value of a Statistical Life Year**.

One approach takes a VSL estimate and spreads it over expected remaining life years, accounting for discounting.

In a simplified constant-value model:

$$
VSL=VSLY\sum_{t=1}^{T}\frac{1}{(1+r)^t}
$$

Here the model assumes a uniform undiscounted monetary value per additional year; remaining life years and discounting determine the aggregate.

Crucially, **VSLY is often derived from VSL rather than independently estimated**, inheriting substantial assumptions. Constant VSLY across age or context is not a universally accepted welfare measure.

### 5.3 Quality-Adjusted Life Years (QALYs)

Health economists frequently ask a different question: how much do treatments change the **quantity and quality of life**?

A QALY combines duration with health-related quality-of-life weights:

$$
QALYs=\sum_t u_t\Delta t
$$

Conventionally, full health receives a weight of 1 and death 0; certain health states may be rated below 0.

| Outcome | Duration | Health weight | QALYs |
|---|---:|---:|---:|
| Perfect health | 10 years | 1.0 | 10 |
| Moderate health impairment | 10 years | 0.7 | 7 |
| Severe health impairment | 10 years | 0.3 | 3 |

The question becomes: **How many quality-adjusted years does a treatment generate, and at what cost?**

For a new treatment:

$$
ICER=\frac{C_{\mathrm{new}}-C_{\mathrm{standard}}}{Q_{\mathrm{new}}-Q_{\mathrm{standard}}}
$$

The *incremental cost-effectiveness ratio* is usually reported as money per QALY gained.

It is **not itself a monetary value of an identified person's life**. It measures the additional resources required for an additional unit of health benefit.

#### Where do QALY weights come from?

This connects back to preference inference. Health economists use **time trade-off**, **standard gamble**, and discrete choice experiments.

For a **time trade-off**, suppose a respondent is indifferent between ten years in an impaired health state and seven years in perfect health. Under the conventional model, that impaired state receives a weight of approximately 0.7.

For a **standard gamble**, respondents choose between living with an impaired health state for certain and a risky intervention yielding perfect health with one probability and death otherwise. The probability at which they're indifferent can identify a utility weight under expected-utility assumptions.

The UK's **National Institute for Health and Care Excellence (NICE)** uses QALYs as its preferred generic outcome measure, with public-preference-based utility weights and choice-based elicitation methods. The **EQ-5D** is its preferred general health-related quality-of-life instrument for adults.

**QALY weights are not necessarily derived from monetary preferences.** They are often derived from preferences among health outcomes; costs are analyzed separately.

### 5.4 Disability-Adjusted Life Years (DALYs)

DALYs are related but measure **health losses** instead of health gains:

$$
DALY=YLL+YLD
$$

- **YLL:** Years of life lost due to premature mortality.
- **YLD:** Years lived with disability, weighted by severity.

One DALY represents approximately one year of healthy life lost.

DALYs are widely used for population health and burden-of-disease research, comparing diseases, injuries, and public-health interventions. They are not inherently monetary, though they can be used within economic evaluation frameworks.

### 5.5 Human capital approaches

This is an older and more controversial approach when someone attempts to use it as the total value of a human life.

A human-capital calculation may estimate the discounted expected stream of future earnings:

$$
HC=\sum_{t=1}^{T}\frac{E(Y_t)}{(1+r)^t}
$$

Such a calculation may be useful for particular lost-earnings or economic damage questions, depending on the institutional and legal context.

But if it is used to measure a **whole person's value**, it produces obvious ethical and economic problems. Someone earning $500,000 annually would appear more valuable than a retiree, a child, an unpaid caregiver, or someone unable to work.

**Human capital is not VSL.** VSL concerns money-risk tradeoffs, not the stream of wages someone would otherwise have earned.

Similarly, **life insurance payouts** represent contractual coverage and insurance demand, not a universal market price of human life.

### 5.6 Comparing health and mortality metrics

| Measure | Fundamental object | Monetary by definition? |
|---|---|---|
| VSL | WTP for small mortality-risk reductions | Yes |
| VSLY | Monetary value assigned to an additional statistical life year | Yes |
| QALY | Health-adjusted life years gained | No |
| DALY | Healthy life years lost | No |
| Cost per QALY | Resources required per additional health benefit | Yes, per health unit |
| Human capital | Discounted expected economic production or earnings | Yes |
| Cost of illness | Medical costs and certain productivity losses | Yes |
| Life insurance payout | Contractual financial protection | Yes, but not a welfare valuation |

The EPA cautions that **cost-of-illness** calculations do not capture full WTP to avoid illness, including pain, suffering, and other welfare effects.

### Resources for further study — Section 5

1. **[EPA, Mortality Risk Valuation](https://www.epa.gov/environmental-economics/mortality-risk-valuation)** — **Best conceptual introduction** to VSL and why agencies are not valuing identified individuals' lives. Contains the 100,000-people example.
2. **[U.S. DOT, Guidance on Valuation of a Statistical Life](https://www.transportation.gov/office-policy/transportation-policy/revised-departmental-guidance-on-valuation-of-a-statistical-life-in-economic-analysis)** — Official U.S. transportation policy guidance and base-year VSL figures, including the 2025-base-year amount effective March 2026.
3. **[EPA, Guidelines, Appendix B and Chapter 7](https://www.epa.gov/environmental-economics/guidelines-preparing-economic-analyses-3rd-edition)** — Review mortality-risk valuation evidence, its applications, and broader health benefits. The EPA page includes direct chapter and appendix links.
4. **[Viscusi and Aldy, “The Value of a Statistical Life: A Critical Review of Market Estimates Throughout the World” (2003)](https://www.nber.org/papers/w9487)** — Landmark review of wage-risk studies, meta-analysis, identification, age, and policy applications. *Technical research paper.*
5. **[NICE, *Health Technology Evaluations: The Manual*, Economic Evaluation](https://www.nice.org.uk/process/pmg36/chapter/economic-evaluation-2/)** — QALYs, EQ-5D, choice-based health utility, costs, and ICERs in actual appraisal practice.
6. **[NICE, *Developing NICE Guidelines*, Section 7: Incorporating Economic Evaluation](https://www.nice.org.uk/process/pmg20/chapter/incorporating-economic-evaluation)** — Read the explanations of time trade-off, standard gamble, and QALY use.
7. **[WHO, Global Health Estimates: DALY Methodology](https://www.who.int/docs/default-source/gho-documents/global-health-estimates/ghe2019_daly-methods.pdf)** — The construction of YLL, YLD, and DALYs, with methods and assumptions.
8. **[WHO, DALY Indicator Definition](https://www.who.int/data/gho/indicator-metadata-registry/imr-details/disability-adjusted-life-year-%28daly%29-estimates-for-0-4-year-olds)** — Short definitional overview before the WHO technical paper.
9. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Chapters 5, 7, and 11 connect risk valuation, longevity and health, and hedonic wages. *Technical book.*
10. **[EPA, *Valuing Mortality Risk Reductions for Environmental Policy* (2010 white paper)](https://19january2021snapshot.epa.gov/environmental-economics/valuing-mortality-risk-reductions-environmental-policy-white-paper-2010_.html)** — Technical and ethical challenges in applying and transferring VSL estimates; an older but influential institutional treatment.

**Check your understanding:** Why is a VSL not a life-insurance payout? What assumptions are needed to transform VSL into VSLY? Why is a QALY weight of 0.7 not automatically a statement that someone's life is worth 70% as much as another person's?

---
## 6. The deeper methodological problems

The methods become more interesting when we scrutinize what it means to estimate a preference coefficient or apply one person's tradeoffs to the public as a whole.

### 6.1 Does observed behavior reveal genuine preferences?

Traditional revealed preference often assumes people act consistently, are reasonably informed, and face meaningful choices.

Imagine a low-income worker accepts a dangerous job because no safer employment is available. The choice may reveal a tradeoff under a **restrictive choice set**, but it does not necessarily reveal how much the worker would value safety if adequate alternatives existed.

Likewise, a person who does not buy insurance may be risk tolerant, financially constrained, poorly informed, or unable to obtain affordable coverage.

**Observed choices identify preferences only in conjunction with assumptions about constraints, information, beliefs, and the choice environment.**

That limitation matters when transferring valuations between populations.

### 6.2 Does WTP measure welfare or purchasing power?

Suppose two people care intensely about preserving an Arctic ecosystem.

One earns $50,000 and would pay $100 to preserve it. The other earns $5 million and would pay $10,000.

Does the second person value the ecosystem 100 times more in any morally meaningful sense? Not necessarily.

Monetary WTP reflects **both preferences and wealth**. A conventional unweighted benefit-cost analysis aggregates dollar-denominated WTP, giving richer people potentially more influence through greater ability to pay.

**Distributional weighting**, equity analysis, and different social welfare functions can address aspects of this problem, but they require additional ethical choices.

### 6.3 Are preferences stable and context independent?

People may report different valuations depending on framing.

One survey asks how much someone would pay to prevent an oil spill. Another asks how much compensation they require to permit it. A third frames the policy as protecting endangered wildlife.

These changes can produce substantially different responses.

This matters because contingent valuation assumes a carefully designed survey can recover economically meaningful preferences. Researchers therefore test **scope sensitivity, payment mechanisms, information presentation, substitutes, and other features**.

### 6.4 Can we add together every benefit estimate?

Not necessarily.

Imagine a forest improves air quality, lowering mortality risks and raising nearby house prices. If the housing premium already reflects some expected health improvement, adding the **full** housing premium to the **full** mortality-risk benefits can double-count the same gain.

The same issue arises with environmental production, recreation, and passive-use value.

A sound benefit-cost analysis requires a precise account of **what each estimate includes**.

### 6.5 Does assigning a monetary value make destruction acceptable?

This returns to the Kantian concern.

Suppose estimated public WTP to preserve an ecosystem totals $500 million. A proposed development would generate $700 million in net benefits **before accounting for environmental loss**.

Under a simple aggregate-WTP benefit-cost criterion, the development might appear worthwhile.

But other considerations can constrain the decision: irreversible ecological loss, endangered-species protections, Indigenous rights, distributional concerns, uncertainty, and the view that some things **should not be exchanged for money**.

**Economic valuation can inform tradeoffs without independently establishing their moral legitimacy.**

### 6.6 Valuation is not the same as discounting

One technical distinction is important if we talk about net present value:

**NPV is not fundamentally a method for discovering preferences or valuing human life. It aggregates monetary benefits and costs that occur at different dates.**

For a policy:

$$
NPV=\sum_{t=0}^{T}\frac{B_t-C_t}{(1+r)^t}
$$

The benefits and costs generally need to be estimated using **other methods first**.

Discounting can dramatically affect results for climate change, biodiversity preservation, and health benefits occurring many decades in the future.

The discount rate itself poses economic and ethical questions, especially in intergenerational contexts.

### Resources for further study — Section 6

1. **[EPA, Guidelines, Chapters 6, 7, 10, and 11](https://www.epa.gov/environmental-economics/guidelines-preparing-economic-analyses-3rd-edition)** — Discounting, benefit estimation, environmental justice and distribution, and communicating uncertainty and results.
2. **[EPA, Guidelines, Chapter 6: Discounting Future Benefits and Costs](https://www.epa.gov/system/files/documents/2024-12/chapter-6-guidelines-for-preparing-economic-analyses_final_508-compliant.pdf)** — **Primary reading** on discount-rate choice, private versus social discounting, and intergenerational concerns.
3. **[IPBES, *Diverse Values and Valuation of Nature* (2022)](https://www.ipbes.net/the-values-assessment)** — Ethical, cultural, plural, and nonmonetary values omitted from narrow WTP calculations.
4. **[John O'Neill, *Markets, Deliberation and Environment* (2007)](https://www.routledge.com/Markets-Deliberation-and-Environment/ONeill/p/book/9780415397124)** — **Philosophical counterpoint**: the moral and political limits of monetary valuation, including biodiversity and deliberation. *Book.*
5. **[Stanford Encyclopedia of Philosophy, “Intrinsic vs. Extrinsic Value”](https://plato.stanford.edu/entries/value-intrinsic-extrinsic/)** — Conceptual background for the claim that the intrinsic worth of something is not reducible to exchange value.
6. **[National Research Council, *Valuing Ecosystem Services* (2005)](https://www.nationalacademies.org/publications/11139)** — Especially useful on double counting, the limits of replacement costs, and integrating ecological and economic assessments.
7. **[U.S. Office of Management and Budget, Circular A-4 (2003), official circulars collection](https://www.whitehouse.gov/omb/information-resources/guidance/circulars)** — Regulatory-analysis context, sensitivity analysis, transparency, and distributional effects. Check the **applicable version** for a particular historical or current policy analysis, since executive-branch guidance can change.
8. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Chapters 3, 4, and 6 on welfare, measurement, and aggregation across time. *Technical book.*

**Check your understanding:** Why might aggregate WTP depend on the income distribution even if ecological consequences are identical? How can a hedonic housing estimate overlap with a health-risk estimate? What ethical judgment is implicit in choosing a social discount rate for effects a century from now?

---

## 7. Putting the methods together: An Arctic oil-spill assessment

Suppose a regulator must assess whether to require costly prevention technology on Arctic oil tankers. The technology would cost **$40 million upfront** and would reduce the likelihood and expected severity of future spills.

A rigorous policy evaluation would:

1. Establish the **counterfactual baseline** without the technology.
2. Model changes in spill **probabilities and severity**.
3. Predict physical environmental and health outcomes under the baseline and policy.
4. Choose valuation methods that correspond to the affected **benefit endpoints**.
5. Aggregate benefits carefully, excluding overlaps.
6. Discount and compare future benefits and costs at consistent dates.
7. Examine uncertainty, distribution, legal requirements, and consequences that cannot be monetized.

### 7.1 Illustrative regulatory assessment matrix

| Consequence | Potential valuation approach |
|---|---|
| Lost commercial fishing productivity | Production function / market model |
| Lost recreational opportunities | Travel cost / site choice |
| Ecological existence and bequest losses | Stated preference |
| Human mortality risk | VSL-based valuation |
| Nonfatal health effects | Morbidity WTP / QALY analysis |
| Habitat restoration requirements | Habitat Equivalency Analysis |
| Future benefits and costs | Discounted welfare analysis |

These methods must be selected based on the actual outcomes of interest, and **overlapping benefits must not be double counted**.

The regulator could compare the expected prevention benefits with costs, while explicitly reporting uncertainty, distribution, statutory constraints, and nonmonetized effects.

### 7.2 Prevention benefits versus legal liability

**Determining liability after an actual spill is separate from the preventive benefit-cost calculation.** Restoration damages, statutory rules, enforcement probabilities, negligence, monetary penalties, and other legal mechanisms matter.

An additional, simplified deterrence problem illustrates why **compensation** and **incentives** must be distinguished.

Suppose an action imposes $100 million in social damage and there is only a 25% chance of holding the responsible firm financially liable. For a risk-neutral firm facing a fine upon detection, expected liability equals harm when:

$$
P(\text{liability})\times F=H
$$

Thus:

$$
0.25\times F=\$100\text{ million}
$$

$$
F=\$400\text{ million}
$$

This is a **stylized deterrence calculation**, not a statement that environmental law automatically imposes damages multiplied by the inverse detection probability. Real-world policy must consider cleanup and restoration duties, ability to pay, enforcement, risk aversion, criminal and civil regimes, and statutory limits.

Other tools include **mandatory insurance**, **prevention standards**, **cleanup obligations**, and **restoration requirements**. Regulators may use a portfolio of mechanisms rather than monetary penalties alone.

This is the logic of **internalizing externalities**: making private decision-makers confront costs they would otherwise impose on other people.

Yet even a perfectly calibrated penalty may not make an **irreversible** environmental loss morally acceptable.

### 7.3 Applied study exercise

Imagine you have estimates for a proposed prevention measure. Before computing NPV, make a worksheet with columns for: physical endpoint; affected population; baseline change; method; unit value or utility measure; timing; uncertainty; overlap with other endpoints; and legal relevance. Then write two different memos:

- a **benefit-cost memo** asking whether the prevention measure's expected aggregate social benefits exceed its cost; and
- a **natural-resource liability memo** asking which restoration measures or damages the responsible party may owe under the applicable law.

The difference between those memos is substantive, not merely a difference in presentation.

### Resources for further study — Section 7

1. **[EPA, *Guidelines for Preparing Economic Analyses*, 3rd ed.](https://www.epa.gov/environmental-economics/guidelines-preparing-economic-analyses-3rd-edition)** — Work through Chapters 3–7, 10–11 for a full regulatory sequence: need for action, baseline, policy options, valuation, discounting, and presentation.
2. **[NOAA, Natural Resource Valuation](https://darrp.noaa.gov/economics/valuation)** — How damage assessment uses actual recreation behavior and stated nonuse preferences.
3. **[NOAA, Habitat Equivalency Analysis](https://darrp.noaa.gov/economics/habitat-equivalency-analysis)** — Restoration scaling rather than direct dollar valuation of the entire ecosystem.
4. **[NOAA, *Scaling Compensatory Restoration Actions*](https://repository.library.noaa.gov/view/noaa/71576)** — Technical NRDA process under the U.S. Oil Pollution Act. Read for how assessment objectives shape the methodology.
5. **[NOAA, *Economic Valuation of Natural Resources: A Handbook*](https://coastalscience.noaa.gov/data_reports/economic-valuation-of-natural-resources-a-handbook-for-coastal-resource-policymakers/)** — Case studies connecting valuation techniques to actual resource-management choices.
6. **[National Research Council, *Valuing Ecosystem Services*](https://www.nationalacademies.org/publications/11139)** — Conceptual justification and warnings when combining different kinds of value.
7. **[NOAA, Deepwater Horizon Lost Recreational Use Assessment](https://darrp.noaa.gov/oil-spills/noaa-economist-shares-data-deepwater-horizon-lost-recreational-use-assessment)** — A real-world study involving tens of thousands of interviews and observations of coastal recreation.
8. **[EPA, Guidelines, Chapter 6 on Discounting](https://www.epa.gov/system/files/documents/2024-12/chapter-6-guidelines-for-preparing-economic-analyses_final_508-compliant.pdf)** — Timing, uncertainty, and intergenerational implications of long-lived restoration and prevention benefits.

**Check your understanding:** What is the baseline against which lost recreation is estimated? Why is the sum of a recreation-demand estimate and a total WTP survey estimate potentially wrong? Which questions belong in a damage assessment and which in an ex ante regulatory benefit-cost analysis?

---

## 8. Preference, welfare, and moral judgment

A crucial distinction is worth making:

> **Putting a monetary price on a decision involving something valuable is not the same as establishing the ultimate worth of that thing.**

Economists sometimes speak as though valuation methods estimate *the value* of a good. More precisely, we can distinguish three layers:

### Layer 1 — Preference identification

**What tradeoffs would individuals actually make, given their resources, information, and available choices?**

Revealed- and stated-preference methods seek to answer questions at this level.

### Layer 2 — Social welfare aggregation

**Whose preferences count? How do we aggregate them? Should a dollar of benefit to a billionaire receive the same social weight as a dollar of benefit to someone in poverty?**

Welfare economics addresses this layer, including income effects, distributional weights, uncertainty, and aggregation across people and time.

### Layer 3 — Ethical and political judgment

**Even when aggregate benefits can be estimated, should a proposed tradeoff be permitted? Are there rights or obligations that override the calculation?**

Moral and political philosophy assess this layer along with the premises underlying the first two.

A regulator assigning a monetary value to statistical mortality-risk reduction is not necessarily claiming that human beings are commodities. An economist measuring WTP for wilderness protection is not necessarily asserting that the wilderness is *only* worth that amount.

They are making tradeoffs legible within a **particular decision-making framework**.

But the most important question is whether that framework has the moral authority to determine which tradeoffs are permissible.

This is where economics stops being merely optimization and becomes political philosophy.

One further insight follows:

**Choosing aggregate willingness to pay as the social objective is itself a normative choice.**

Economics does not escape philosophy simply by measuring preferences accurately. It becomes better at answering a particular class of questions **once its philosophical and institutional assumptions have been specified**.

This is also why a Kantian claim—that human beings possess dignity rather than a price—need not conflict with all practical risk-reduction budgeting. One can treat some acts as categorically impermissible **while** comparing the costs and benefits of lawful safety interventions under finite resources. At the same time, pretending that every moral commitment can be converted into money would ignore what the original objection is about.

### Resources for further study — Section 8

1. **[John O'Neill, *Markets, Deliberation and Environment*](https://www.routledge.com/Markets-Deliberation-and-Environment/ONeill/p/book/9780415397124)** — **Best philosophical companion** to the technical valuation material; addresses the limits of markets and the monetary valuation of biodiversity.
2. **[Stanford Encyclopedia of Philosophy, “Intrinsic vs. Extrinsic Value”](https://plato.stanford.edu/entries/value-intrinsic-extrinsic/)** — Clarifies intrinsic value, instrumental value, and related philosophical distinctions.
3. **[IPBES, *Diverse Values and Valuation of Nature*](https://www.ipbes.net/the-values-assessment)** — Multidisciplinary institutional perspective on plural values, nonmonetary measures, and just decision processes.
4. **[EPA, Mortality Risk Valuation FAQ](https://www.epa.gov/environmental-economics/mortality-risk-valuation)** — Explicitly distinguishes pricing statistical risk reductions from pricing individual human beings, and explains why ethical concerns remain relevant.
5. **[National Research Council, *Valuing Ecosystem Services*](https://www.nationalacademies.org/publications/11139)** — A scientific-policy view of how economic and ecological valuation should inform decisions without purporting to settle every ethical question.
6. **[Freeman, Herriges, and Kling, *The Measurement of Environmental and Resource Values*](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917)** — Provides the formal welfare theory to contrast with philosophical critiques. *Technical book.*
7. **[OECD, *Handbook of Biodiversity Valuation*](https://www.oecd.org/content/dam/oecd/en/publications/reports/2002/03/handbook-of-biodiversity-valuation_g1gh28a2/9789264175792-en.pdf)** — Helps distinguish an empirical measure of WTP from claims about intrinsic ecological worth.

**Check your understanding:** Why could a regulator use VSL without accepting that people are transferable commodities? What is the difference between estimating a tradeoff and deciding whether it is morally permissible? Which aspects of valuation belong to positive economics, and which require explicit normative commitments?

---

## 9. Cross-section reading pathway and consolidated references

### A guided route through the literature

If you're reading sequentially rather than treating the guide as a reference book, this is a useful progression:

| Phase | Main goal | Start with | Then deepen with |
|---|---|---|---|
| **1. Foundations** | Understand WTP, WTA, consumer surplus, and welfare measurement | NOAA Natural Resources Handbook; EPA Appendix A | Freeman–Herriges–Kling, Chapters 2–4 |
| **2. Revealed preference** | Infer values from travel, housing, safety, and production | EPA Chapter 7; NOAA Valuation page | Rosen (hedonics), Train (choice models), Afriat (theory) |
| **3. Stated preference** | Learn contingent valuation and attribute-based choice | EPA Chapter 7; NRC Chapter 4 | NOAA Panel (1993), Carson–Groves (2007) |
| **4. Ecological damage** | Match endpoints to tools and restoration obligations | OECD TEV; NOAA HEA | NOAA restoration-scaling guidance; NRC report |
| **5. Health and mortality** | Distinguish VSL, VSLY, QALY, DALY, human capital | EPA VSL FAQ; NICE manual | Viscusi–Aldy, EPA Appendix B, WHO methods |
| **6. Ethics and policy** | Evaluate aggregation, discounting, uncertainty, and legitimacy | EPA Chapters 6 and 10; IPBES Values | O'Neill; SEP on intrinsic value |

### The core three resources

If you choose only three starting points, use:

1. **[EPA, *Guidelines for Preparing Economic Analyses* (2024)](https://www.epa.gov/environmental-economics/guidelines-preparing-economic-analyses-3rd-edition)** — Especially **Chapter 7**, **Appendix A**, and **Appendix B**. This is the main methodological spine of the guide.
2. **[NOAA, *Economic Valuation of Natural Resources: A Handbook for Coastal Resource Policymakers*](https://coastalscience.noaa.gov/data_reports/economic-valuation-of-natural-resources-a-handbook-for-coastal-resource-policymakers/)** — Applied entry point with travel cost, contingent valuation, and resource-management examples.
3. **[U.S. DOT, Guidance on VSL](https://www.transportation.gov/office-policy/transportation-policy/revised-departmental-guidance-on-valuation-of-a-statistical-life-in-economic-analysis)** — Concrete example of mortality-risk valuation in public policy.

### Major books and reports in one place

- **A. Myrick Freeman III, Joseph A. Herriges, and Catherine L. Kling (2014)**, [*The Measurement of Environmental and Resource Values: Theory and Methods*, 3rd ed.](https://www.routledge.com/The-Measurement-of-Environmental-and-Resource-Values-Theory-and-Methods/III-Herriges-Kling/p/book/9781315780917). Graduate-level treatment of nearly every valuation method in this guide.
- **John O'Neill (2007)**, [*Markets, Deliberation and Environment*](https://www.routledge.com/Markets-Deliberation-and-Environment/ONeill/p/book/9780415397124). Ethical and political-philosophical examination of markets and environmental value.
- **Kenneth Train (2009)**, [*Discrete Choice Methods with Simulation*, 2nd ed.](https://eml.berkeley.edu/books/choice2.html). Free author-hosted technical chapters on logit, nested logit, mixed logit, estimation, and identification.
- **National Research Council (2005)**, [*Valuing Ecosystem Services: Toward Better Environmental Decision-Making*](https://www.nationalacademies.org/publications/11139). Interdisciplinary report on the strengths and limitations of valuation.
- **OECD (2002)**, [*Handbook of Biodiversity Valuation*](https://www.oecd.org/content/dam/oecd/en/publications/reports/2002/03/handbook-of-biodiversity-valuation_g1gh28a2/9789264175792-en.pdf). Total economic value and the use/nonuse taxonomy.
- **IPBES (2022)**, [*Diverse Values and Valuation of Nature*](https://www.ipbes.net/the-values-assessment). A plural-values perspective spanning ecology, society, and ethics.
- **U.S. EPA (2024)**, [*Guidelines for Preparing Economic Analyses*, 3rd ed.](https://www.epa.gov/environmental-economics/guidelines-preparing-economic-analyses-3rd-edition). Core policy-analysis framework.
- **NOAA (2018 resource listing)**, [*Economic Valuation of Natural Resources: A Handbook for Coastal Resource Policymakers*](https://coastalscience.noaa.gov/data_reports/economic-valuation-of-natural-resources-a-handbook-for-coastal-resource-policymakers/). Coastal valuation case studies.
- **NICE**, [*Health Technology Evaluations: The Manual*, Economic Evaluation](https://www.nice.org.uk/process/pmg36/chapter/economic-evaluation-2/). QALY, EQ-5D, ICER, and health technology appraisal methodology.
- **WHO**, [*Global Health Estimates: DALY Methodology*](https://www.who.int/docs/default-source/gho-documents/global-health-estimates/ghe2019_daly-methods.pdf). Formal construction of the disability-adjusted life year.

### Foundational articles in one place

- **Sherwin Rosen (1974)**, [“Hedonic Prices and Implicit Markets: Product Differentiation in Pure Competition.”](https://doi.org/10.1086/260169) Hedonic-price theory.
- **S. N. Afriat (1967)**, [“The Construction of Utility Functions from Expenditure Data.”](https://doi.org/10.2307/2525382) Rationalizing revealed-preference data.
- **Gordon M. Becker, Morris H. DeGroot, and Jacob Marschak (1964)**, [“Measuring Utility by a Single-Response Sequential Method.”](https://doi.org/10.1002/bs.3830090304) Incentivized elicitation.
- **Richard T. Carson and Theodore Groves (2007)**, [“Incentive and Informational Properties of Preference Questions.”](https://doi.org/10.1007/s10640-007-9124-5) Survey incentives and preference revelation.
- **Kenneth Arrow, Robert Solow, and other panel members (1993)**, [*Report of the NOAA Panel on Contingent Valuation*.](https://repository.library.noaa.gov/view/noaa/60900/noaa_60900_DS1.pdf) Seminal survey-design assessment.
- **W. Kip Viscusi and Joseph E. Aldy (2003)**, [“The Value of a Statistical Life: A Critical Review of Market Estimates Throughout the World.”](https://www.nber.org/papers/w9487) Broad review of wage-risk VSL estimation.

### Final synthesis

The most important distinction to carry through all of this is **the difference between recovering an individual's willingness to pay and constructing a socially justified measure of value**.

It is central to everything from an Arctic oil spill to mortality-risk regulation. **Economic valuation does not simply discover an intrinsic price hidden inside nature or human beings.** It estimates defined tradeoffs under assumptions about preferences, constraints, institutions, time, and social aggregation. Those estimates can be indispensable for rational decisions without being an exhaustive account of moral worth.

This is precisely the tension between economic optimization and Kantian ethics that motivated the discussion in the first place.

---

*Source note: Source links in this guide point to publishers, government agencies, professional research repositories, or academic references. Their descriptions and subject matter were checked as of October 2026. Current agency values or regulatory rules should be checked again when performing an actual analysis.*
