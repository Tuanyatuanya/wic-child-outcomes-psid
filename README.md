# Prenatal WIC Participation and Children's Academic Achievement

**A three-part policy research series in R using the Panel Study of Income Dynamics – Child Development Supplement (PSID-CDS, 1997)**
*MSSP 8970 Applied Linear Modeling · University of Pennsylvania · Fall 2025 · sole author*

The Special Supplemental Nutrition Program for Women, Infants, and Children (WIC) supports low-income pregnant women and young children. This series asks whether **prenatal WIC participation is associated with children's later reading and math achievement**. It moves from a hierarchical model, to assumption diagnostics and model correction, to moderation analysis.

**Stack:** R · `lm` · car · lmtest · sandwich (robust SEs) · psych · stargazer · tidyverse · knitr

---

## The series
| # | Question | Methods | Key result | Code & output | Report |
|---|---|---|---|---|---|
| 1 | Do prenatal **WIC and AFDC** participation predict children's **reading** scores beyond age, income, birth weight and home environment? | Hierarchical OLS, ΔR², standardized coefficients | Both programs are associated with *lower* reading scores (WIC β = −3.12, p < .001; AFDC β = −2.40, p = .02). Together they add only **ΔR² = 0.013** to the controls (R² 0.178 → 0.191), with N = 1,845. | [01](analysis/01_reading_wic_afdc.md) | [PDF](reports/01_WIC_AFDC_reading_achievement.pdf) |
| 2 | Is the WIC–**math** association robust once the linear-model assumptions are checked? | Component-residual plots, Breusch–Pagan and White tests, Q–Q plots, Cook's distance, then asinh(income), HOME score and **HC3 robust SEs** | Heteroskedasticity was confirmed (BP = 164.9, p < .001). After correction, WIC remains negative (β = −2.24, robust p < .001), and the HOME environment score is a strong positive predictor (β = 0.69). Adding HOME significantly improves fit (nested F = 71.1, p < .001; adj. R² 0.865 → 0.869). | [02](analysis/02_math_wic_diagnostics.md) | [PDF](reports/02_WIC_math_achievement_diagnostics.pdf) |
| 3 | Does the WIC–math association **vary by family income, race, or child age**? | Interaction models with centered and log-transformed moderators | All three interactions are significant. The negative WIC association is **stronger at higher family income** (β = −0.91, p = .009) and for **older children** (β = −0.44 per year, p < .001), and **smaller for Black than White children** (β = +2.18, p = .014). N = 1,848. | [03](analysis/03_moderation.md) | [PDF](reports/03_WIC_moderation_effects.pdf) |

## Interpretation
The consistently **negative** WIC coefficient should not be read as WIC harming children. WIC eligibility is means-tested, so participation marks economic disadvantage that the available controls only partly capture (**selection bias**). The moderation results describe *where* this gap is larger or smaller, not where WIC works better or worse. Estimating a causal effect would need a design that addresses selection, such as sibling fixed effects, eligibility-threshold discontinuities, or program-rollout variation.

## What this series demonstrates
- Building a model step by step, with an explicit justification for every addition (ΔR², nested F-tests)
- Treating **assumption diagnostics as part of the analysis**: heteroskedasticity tests, influence measures, transformations and robust standard errors
- Moderation analysis with centered predictors and careful interpretation of interaction terms
- Writing up results for a policy audience, while being explicit about the limits of observational data

## Reproduce
The PSID-CDS extract is **not included**: PSID data may not be redistributed. Register at [psidonline.isr.umich.edu](https://psidonline.isr.umich.edu/), build the extract with the variables named in each analysis, and save it as `data/good.csv`. Then:
```bash
cd analysis
for f in 01_reading_wic_afdc 02_math_wic_diagnostics 03_moderation; do
  Rscript -e "knitr::knit('$f.Rmd', output = '$f.md')"
done
```
Required packages: `knitr`, `dplyr`, `tidyr`, `ggplot2`, `car`, `lmtest`, `sandwich`, `psych`, `stargazer`.
