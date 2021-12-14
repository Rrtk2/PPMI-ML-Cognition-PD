# PPMI-ML-Cognition-PD
Machine Learning-based Prediction of Cognitive Outcomes in de novo Parkinson's Disease

[![GitHub License](https://img.shields.io/github/license/Rrtk2/PPMI-ML-Cognition-PD)](https://github.com/Rrtk2/PPMI-ML-Cognition-PD/blob/master/LICENSE.md) ![](https://img.shields.io/badge/Status-Submitting-green) [![GitHub Watches](https://img.shields.io/github/watchers/Rrtk2/PPMI-ML-Cognition-PD.svg?style=social&label=Watch&maxAge=2592000)](https://github.com/Rrtk2/PPMI-ML-Cognition-PD/watchers) 


## Abstract
**Background:** Cognitive impairment is a debilitating symptom in Parkinson’s disease (PD), with high variability in onset and the course of progression.
 
**Objectives:** To establish an accurate multivariate machine learning (ML) model to predict cognitive outcome in newly diagnosed PD.

**Methods:** We used baseline clinical and biological measures from the Parkinson’s Progression Markers Initiative (PPMI) cohort as predictive variables. Annual cognitive assessments over an eight-year time span were used to define two cognitive outcomes of i) dementia conversion, and ii) cognitive impairment. Selected variables were organized into three subsets of clinical, biofluid (CSF and serum) and genetic/epigenetic (blood) measures and tested using four different ML algorithms to predict individual cognitive function in PD.

**Results:** For both cognitive outcomes, irrespective of the ML algorithm used, the models consisting of the clinical subset of variables performed best, with high specificity and the largest area under the curve (AUCs) (0.88 -– 0.92) and Matthews Correlation Coefficient (MCCs) (0.57 –- 0.80). Notably, the cognitive impairment outcome showed better sensitivity than dementia conversion outcome (0.72 – 0.81 vs 0.29 – 0.64, respectively). We observed a marginal improvement in the prediction performance when clinical, biofluid, and epigenetic/genetic variables were all included in one model. Several CSF variables and an epigenetic marker showed high predictive weighting in multiple models only when included alongside clinical variables.

**Conclusion:** Within the generated models, clinical predictors appear to play a more prominent role than biological or genetic predictors. ML algorithms can accurately predict cognitive impairment and dementia in de-novo PD up to eight years before cognitive decline.

## How is data processed and analyzed?
TBA.

## Contact
Ehsan Pishva ([ehsanpishva](https://github.com/ehsanpishva)) - e.pishva@maastrichtuniversity.nl 

## Who is involved, and what are their roles.
Joshua Harvey ([JoshHarveyGit](https://github.com/JoshHarveyGit)) - Undertook data analysis, support with data review, wrote the first draft of the manuscript

Rick Reijnders ([Rrtk2](https://github.com/Rrtk2)) - Undertook data analysis, support with data review, wrote the first draft of the manuscript

Ehsan Pishva ([ehsanpishva](https://github.com/ehsanpishva)) - Conceived and directed the project, wrote the first draft of the manuscript

Annelien Duits, Byron Creese - Were involved in the selection of the clinical predictors and outcome
Rachel Cavil, Sebastian Köhler, Ali Torkamani - Provided advice on data analysis.
Gemma Shireby - Contributed to generating polygenic scores

Joshua Harvey, Rick Reijnders, Ehsan Pishva, Katie Lunnon, Albert FG Leentjens, Lars Eijssen, Bart PF Rutten, Byron Creese, Annelien Duits - Contributed to the interpretation of the results
All authors provided critical feedback on the manuscript and approved the final submission


## Status of project
Submitting.


## Licencing and authors
All code and documents in the PPMI-ML-Cognition-PD folder was created by [these author(s)](/AUTHORS.md).

The project's source code is freely reusable under the [MIT License](/LICENSE.md).
