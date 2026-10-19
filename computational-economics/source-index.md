# Computational Economics — Source Index

This index maps the **existing archive** to the 24-week program in [README.md](README.md). Linked local files are confirmed by exact pathname in `computational-economics.zip`. Files remain in your existing `computational-economics/` folder; extract the three-guide ZIP into the parent of that folder. The index is selective: it does not claim all archived notebooks are required.

## How to use the sources

- **Core:** CompEcon notebooks, Jackson networks, Miller–Page book, and curated links in `resources.md`.
- **Support:** numerical-methods and linear-algebra repositories when a solver or mathematical result needs attention.
- **Lab bank:** PythonNumericalDemos; mostly generic geostatistics/data-science demonstrations, so adapt methods to an economic question rather than treating original domain examples as economic content.
- **External:** AGT, mechanism design, QuantEcon, and computational social science are linked curricula, not files inside the ZIP.

## A. Programming, algorithms, numerical methods (Weeks 1–4)

| Resource | Focus |
|---|---|
| [Computer programming and model setup](CompEcon/01_introduction.ipynb) | Start with modeling and reproducibility; use notebooks 02–08 selectively if new to Python. |
| [Algorithms and computational thinking](CompEcon/09_algorithms.ipynb) | Complexity and algorithm design. |
| [Binary search](CompEcon/11_alg_binary_search.ipynb) | Simple correctness and complexity exercise. |
| [Root finding](CompEcon/13_bisections_newton.ipynb) | Bisection/Newton and diagnostic comparison. |
| [Arrays and vectorization](CompEcon/14_numpy.ipynb) | Numerical implementation. |
| [Matrix and data computing](CompEcon/15_pandas.ipynb) | Data handling as a computational research skill. |
| [Constrained allocation / LP](CompEcon/18_linear_prog.ipynb) | Optimization and shadow values. |
| [Numerical errors](intro-numerical-methods/04_error.ipynb) | Conditioning, floating point, truncation. |
| [Root finding and optimization](intro-numerical-methods/05_root_finding_optimization.ipynb) | Numerical alternative to CompEcon. |
| [Linear algebra and QR](intro-numerical-methods/11_LA_QR.ipynb) | Stable least squares. |
| [Conditioning and stability](intro-numerical-methods/12_LA_conditioning_stability.ipynb) | Essential reliability concepts. |
| [Eigenanalysis](intro-numerical-methods/13_LA_eigen.ipynb) | Foundation for Markov and network spectra. |

## B. Stochastic and dynamic economics (Weeks 5–8)

| Resource | Focus |
|---|---|
| [Markov transitions](CompEcon/20_markov.ipynb) | Transitions as economic state dynamics. |
| [Stationary distributions](CompEcon/21_stationary.ipynb) | Long-run distributions and conditions. |
| [Fixed point approximation](CompEcon/22_succ_aprx.ipynb) | Successive approximation. |
| [Multivariate Newton](CompEcon/23_mutivariate_newton.ipynb) | Equilibrium systems. |
| [Dynamic programming](CompEcon/27_dp_intro.ipynb) | Bellman operator and decisions. |
| [Rust replacement background](CompEcon/28_zurcher.ipynb) | Dynamic discrete choice model. |
| [Rust replacement implementation](CompEcon/29_zurcher_code.ipynb) | Structural model example. |
| [Cake eating](CompEcon/30_cake_ongrid.ipynb) | Finite grid control. |
| [Consumption/savings](CompEcon/35_consumption_savings.ipynb) | Heterogeneous forward simulations. |
| [Simulation foundations](CompEcon/36_simulations.ipynb) | Replications and Monte Carlo. |
| [DP theory](CompEcon/37_dp_theory.ipynb) | Contraction and fixed points. |
| [Euler equation time iteration](CompEcon/39_euler_timeiter.ipynb) | Faster solution methods. |
| [Endogenous grid method](CompEcon/41_egm.ipynb) | Interpolation in dynamic choice. |
| [EGM implementation](CompEcon/42_egm_code.ipynb) | Compare solver accuracy. |
| [Policy iteration](CompEcon/43_policy_iter.ipynb) | Alternative Bellman solver. |
| [Simulated moments](CompEcon/45_msm_estimation.ipynb) | Model–data matching. |
| [Nested fixed point](CompEcon/46_nfxp_estimation.ipynb) | Structural likelihood and computation. |
| [Rust (1987) article](CompEcon/_static/pdf/Rust_ECMA1987.pdf) | Original structural estimation research; paper included in archive. |
| [NFXP computational note](CompEcon/_static/pdf/Rust_nfxp_man_2000.pdf) | Algorithmic and implementation context. |

## C. Complex adaptive systems and computational social science (Weeks 9–12, 18, 23)

| Resource | Focus |
|---|---|
| [Miller–Page textbook](Complex%20Adaptive%20Systems%20An%20Introduction%20to%20Computational%20Models%20of%20Social%20Life.pdf) | Core conceptual source: chapters 2–11 are especially relevant. |
| [Monte Carlo methods demo](PythonNumericalDemos/Interactive_Monte_Carlo_Simulation.ipynb) | Stochastic replication tools. |
| [Uncertainty checking demo](PythonNumericalDemos/Interactive_Uncertainty_Checking.ipynb) | Model validation practice. |
| [Simulation demo](PythonNumericalDemos/Interactive_Simulation.ipynb) | Generic simulation scaffolding. |
| [Shapley value demo](PythonNumericalDemos/Interactive_Shapley_Values.ipynb) | Game-theoretic attribution; optional. |
| [Network sampling demo](PythonNumericalDemos/Interactive_Sampling_Methods.ipynb) | Selection and observation model. |
| [Decision making demo](PythonNumericalDemos/Interactive_DecisionMaking.ipynb) | Decision rule and uncertainty exercise. |

## D. Networks: topology, formation, diffusion, learning and games (Weeks 13–17)

| Resource | Focus |
|---|---|
| [Jackson archived syllabus](networks/1.Introduction%2C%20Empirical%20Background%20and%20Definitions/Networks-Online-Syllabus.pdf) | Authoritative seven-unit network-course outline. |
| [Network definitions](networks/1.Introduction%2C%20Empirical%20Background%20and%20Definitions/3.%20Background%20Definitions%20and%20Notation.ipynb) | Graph metrics and network data. |
| [Degree distributions](networks/1.Introduction%2C%20Empirical%20Background%20and%20Definitions/9.%20Degree%20Distributions.ipynb) | Degree heterogeneity. |
| [Clustering](networks/1.Introduction%2C%20Empirical%20Background%20and%20Definitions/10.%20Clustering.ipynb) | Local network patterns. |
| [Centrality](networks/2.%20Background%2C%20Definitions%2C%20and%20Measures%20Continued/3.%20Centrality%20Measures-%20Degree%2C%20Closeness%2C%20Decay%20and%20Betweenness.ipynb) | Compare measures and meanings. |
| [Eigenvector centrality](networks/2.%20Background%2C%20Definitions%2C%20and%20Measures%20Continued/4.%20Centrality%20-%20Eigenvector%20Measures.ipynb) | Spectral influence. |
| [Random networks and thresholds](networks/2.%20Background%2C%20Definitions%2C%20and%20Measures%20Continued/7.%20Random%20Networks%20-%20Thresholds%20and%20Phase%20Transitions.ipynb) | Phase transitions. |
| [Preferential attachment](networks/3.%20Random%20Networks/3.3%20Preferential%20Attachment.ipynb) | Growing networks. |
| [Block models](networks/3.%20Random%20Networks/3.6%20Block%20Models.ipynb) | Latent community structure. |
| [ERGM introduction](networks/3.%20Random%20Networks/3.7%20ERGMs.ipynb) | Statistical network specification. |
| [SUGM introduction](networks/3.%20Random%20Networks/3.10%20SUGMs.ipynb) | Subgraph models. |
| [Strategic formation](networks/4.%20Strategic%20Network%20Formation/1.%20Strategic%20Network%20Formation.ipynb) | Incentives and endogenous links. |
| [Pairwise stability](networks/4.%20Strategic%20Network%20Formation/2.%20Pairwise%20Stability%20and%20Efficiency.ipynb) | Stability versus welfare. |
| [Connections model](networks/4.%20Strategic%20Network%20Formation/3%20Connections%20Model.ipynb) | Strategic link costs/benefits. |
| [Diffusion overview](networks/5.%20Diffusion%20on%20Networks/1.%20Diffusion.ipynb) | Adoption and contagion. |
| [Bass adoption model](networks/5.%20Diffusion%20on%20Networks/2.%20Bass%20Model.ipynb) | Product adoption. |
| [SIS contagion model](networks/5.%20Diffusion%20on%20Networks/5.%20SIS%20Model.ipynb) | Diffusion across social connections. |
| [Fitting diffusion to data](networks/5.%20Diffusion%20on%20Networks/8.Fitting%20a%20Diffusion%20Model%20to%20Data.ipynb) | Calibration and limits. |
| [Financial contagion](networks/5.%20Diffusion%20on%20Networks/10.%20Application-%20%20Financial%20Contagions%20%20Simulations.ipynb) | Systemic risk scenario lab. |
| [DeGroot learning](networks/6.%20Learning%20on%20Networks/2.%20DeGroot%20Model.ipynb) | Non-Bayesian linear belief updates. |
| [DeGroot convergence](networks/6.%20Learning%20on%20Networks/3.%20Convergence%20in%20DeGroot%20Model.ipynb) | Spectral and asymptotic reasoning. |
| [Information aggregation](networks/6.%20Learning%20on%20Networks/7.%20Information%20Aggregation.ipynb) | Truth versus consensus. |
| [Strategic complements/substitutes](networks/7.%20Games%20on%20Networks/2.%20Complements%20and%20Substitues.ipynb) | Local strategic externalities. |
| [Multiple equilibria](networks/7.%20Games%20on%20Networks/4.%20Multiple%20Equilibria.ipynb) | Multiplicity and dynamics. |
| [Linear-quadratic game](networks/7.%20Games%20on%20Networks/7.%20A%20linear%20quadratic%20model.ipynb) | Closed-form and numerical equilibria. |

## E. Scientific computing depth — selective, not core

| Resource | Focus |
|---|---|
| [Numerical methods README](Numerical-Analysis-Python/README.md) | Course structure. |
| [Differential equation textbook](Numerical-Analysis-Python/Numerical_Analysis_for_Differential_Equations.pdf) | Methods, proofs and problem sets; consult as needed. |
| [Runge–Kutta implementation](Numerical-Analysis-Python/Chapter%2003%20-%20Runge%20Kutta/302_4th%20Order%20Runge%20Kutta.ipynb) | ODE solver example. |
| [Numerical stability](Numerical-Analysis-Python/Chapter%2005%20-%20IVP%20Consistent%20Convergence%20Stability/503_Stability.ipynb) | Convergence is more than a plot. |
| [Heat-equation solver](Numerical-Analysis-Python/Chapter%2008%20-%20Heat%20Equations/803_Heat%20Equation-Crank_Nicolson.ipynb) | PDE extension, outside main sequence. |

## F. External courses, research programs and scholars (from `resources.md`)

The archive lists these learning paths. The descriptions below summarize what their current sites offer, not content asserted to be stored offline. External syllabi and dependency versions may change.

| Site | Recommended use | Core weeks |
|---|---|---|
| [QuantEcon](https://quantecon.org/) — Sargent, Stachurski and collaborators | Markov chains, dynamic programming, quantitative economics in Python/Julia and advanced computational tools | 5–8; advanced track |
| [QuantEcon Dynamic Programming](https://dp.quantecon.org/) | Bellman operators, fixed points, recursive decisions | 6–8 |
| [Foundations of Computational Economics](https://fedor.iskh.me/compecon) | Companion course for archived `CompEcon/` notebooks | 1–8 |
| [Matthew O. Jackson: Social and Economic Networks](https://www.coursera.org/learn/social-economic-networks) | Supplement to full locally archived network notebooks/slides | 13–17 |
| [Computational Social Science (Chris Bail)](https://cbail.github.io/comp_soc_grad/Home.html) | Collection of online/digital traces, text data and methodological ethics | 11, 23 |
| [Pablo Barberá computational social science code](http://pablobarbera.com/POIR613/code.html) | Alternative workflows for digital trace research | 11 |
| [Mechanism Design (Dickerson & Knittel)](https://mech-design.github.io/) | Matching, truthful allocation, fairness, combinatorial mechanisms | 20–22 |
| [Tim Roughgarden CS364A (2013)](https://timroughgarden.org/f13/f13.html) | Lectures and exercises on auctions, price of anarchy, equilibrium computation, no-regret dynamics | 19–22 |
| [Algorithmic Game Theory (Nisan–Roughgarden–Tardos–Vazirani)](https://timroughgarden.org/books.html) | Formal reference (see course page for reading/notes links) | 19–22 |
| [Society for Computational Economics](https://comp-econ.com/) | Research context and additional computational economics work | Advanced |
| [Columbia Algorithmic Game Theory for OR](https://www.columbia.edu/~ck2945/courses/f25_8100/) | Optimization-oriented comparison | Advanced |
| [Cornell CS 6840 Algorithmic Game Theory](https://www.cs.cornell.edu/courses/cs6840/2025fa/lectures/) | Advanced theoretical game/algorithm methods | Advanced |

**People and perspective:** Matthew O. Jackson (networks and incentives), Tim Roughgarden (algorithmic game theory), Constantinos Daskalakis (equilibrium computation), Adam Wierman (networks, markets, computation), Thomas J. Sargent and John Stachurski (computational dynamic economics), John H. Miller and Scott E. Page (complex adaptive social systems). The scholars named in `resources.md` are reference anchors, not necessarily authors of the local PDFs.

## G. Recommended reading tracks

**Minimal practical route (12–14 weeks):** CompEcon weeks 1–8 → Jackson weeks 13, 15–17 → mechanism weeks 19–20 → one integrated project. This accelerates coverage but sacrifices depth in adaptation and computational social science.

**Strong programmer / economist:** skim notebooks 01–16; focus weeks 3–8, 13–24; prove the main results in [mathematical-reference.md](mathematical-reference.md).

**Systems science / computer science:** begin Miller–Page chapters 2–7 and Jackson Weeks 1–3 in the archived course; then return to CompEcon for Bellman operators and structural estimation before tackling market design.

**Econometric researcher:** prioritize model validation, structural identification, statistics of networks, diffusion model calibration, digital trace selection, and robustness. Pair with the Causal Inference and Bayesian guides; computational experiments do not replace identification arguments.

## H. Source-provenance and adaptation notes

- The archived networking content is a historical Jackson Coursera course whose own syllabus covers seven teaching weeks plus exam. The 24-week integrated guide intentionally redistributes it rather than claiming it was a 24-week course.
- The local Miller–Page *Complex Adaptive Systems* is a conceptual book, not a ready-made notebook series; implementation exercises in the README are proposed new work.
- Several methods (VCG, matching, no-regret, price of anarchy, computational social science) require external online teaching material from the links above.
- Some notebooks originate in different scientific domains and may require older dependencies, optional data files or porting. Preserve license notices and track modifications.
- For any missing files or links in a future reorganization, consult the exact paths in this index and check the repository before substituting an unrelated notebook.

**Index validation:** all 68 curated local file paths listed above occur in the provided ZIP.
