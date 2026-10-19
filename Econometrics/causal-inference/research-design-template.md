# Causal Research Design and Replication Template

> Duplicate this form before starting an empirical project. Design claims must precede estimator selection. Clearly flag untestable identifying assumptions.

## 1. Question and setting

- **Substantive decision:**
- **Units, setting and target population:**
- **Intervention/treatment (including versions and timing):**
- **Comparison/control condition:**
- **Outcomes, measurement and follow-up horizon:**
- **Why the question matters:**

## 2. Counterfactual and target estimand

- **Potential outcomes or SCM notation:**
- **Estimand:** ATE / ATT / ITT / LATE / CATE / RD local effect / dynamic regime / other.
- **Population, subgroup, and timing of estimand:**
- **Treatment assignment versus actual treatment received:**
- **SUTVA/consistency and interference considerations:**

$$
\tau_{\mathrm{target}}=\text{[write the counterfactual contrast here]}.
$$

## 3. Identification and causal model

- **DAG or assignment mechanism (attach graph):**
- **Core assumptions (in words and mathematics):**
- **Why the design plausibly satisfies those assumptions:**
- **Main threats: omitted variables, selection, timing, spillovers, anticipation, noncompliance, attrition, measurement error:**
- **What is empirically assessable versus untestable:**
- **What causal quantity is identified under the assumptions:**

## 4. Data and research workflow

- **Raw source(s), collection date, access/ethics:**
- **Observation unit, number of clusters, time range and panel structure:**
- **Sample restrictions and exclusions (explain before viewing results):**
- **Covariate definitions and treatment timing:**
- **Missing data and potential censoring:**
- **Data dictionary and transformations:**

## 5. Estimation and uncertainty

- **Estimator and identifying functional:**
- **Model specification / nuisance estimation:**
- **SE/CI or uncertainty strategy, including cluster/randomization unit:**
- **Pre-specified primary outcome and subgroup analyses:**
- **Software and version / random seed / environment:**

## 6. Diagnostics and falsification

- **Design-specific diagnostic 1 (e.g. overlap/balance, pre-trend, cutoff density, first stage):**
- **Design-specific diagnostic 2:**
- **Negative control, placebo or falsification exercise:**
- **Alternative plausible specifications / sensitivity to unmeasured confounding:**
- **Multiple testing / selective reporting considerations:**
- **Generalization and external validity:**

## 7. Findings and interpretation

- **Primary estimate, units and uncertainty:**
- **What the evidence supports conditionally on assumptions:**
- **What it *does not* support:**
- **Heterogeneity and equity implications:**
- **Robustness conclusion (not merely a checkmark):**
- **Decision implications / further data needed:**

## 8. Replication and disclosure

- **Reproduction command/notebook:**
- **Directory and data availability:**
- **Source paper/notebook and deviations from original:**
- **Conflicts, data limitations and relevant unverified assumptions:**
- **Final one-paragraph abstract:**
