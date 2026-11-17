# Code for "Revisiting scaling models of trees"

This repository contains code and computational outputs supporting the
analyses reported in the manuscript **"Revisiting scaling models of
trees."** The shared code and results cover the **Changbaishan (CBS)
site**.

## Repository structure

``` text
.
├── 1-CalculateObservedExponents (CBS)
├── 2-CalculateArchitecturalParameters (CBS Trees 1-3)
├── 3-ConstrainedModels (CBS)
└── 4-MainFigures
```

### 1. `1-CalculateObservedExponents (CBS)`

Code for estimating the observed scaling exponents and their
corresponding **95% Bayesian credible intervals (BCIs)** using data from
the CBS site.

### 2. `2-CalculateArchitecturalParameters (CBS Trees 1-3)`

Code for calculating the branching-network architectural parameters
required by the scaling models:

-   **Symmetric networks:** the two network characteristic parameters,
    *a* and *b*, used in the GWBE model.
-   **Asymmetric networks:** the four group-specific ratios, βᵤ, βᵥ, γᵤ,
    and γᵥ, used in the asymmetric GWBE model.

Three CBS trees are provided as demonstration examples to illustrate the
parameter-calculation procedures. For access to more comprehensive data,
please contact the authors.

### 3. `3-ConstrainedModels (CBS)`

Computational results for the WBE, GWBE, and asymmetric GWBE models
based on the CBS site.

### 4. `4-MainFigures`

Code used to generate the figures presented in the main text.

## Notes

-   The code and computational results shared here are for the
    Changbaishan (CBS) site.
-   Before running the scripts, check the input and output paths and
    update them as needed for your local environment.
-   For methodological details, including the definitions of the scaling
    models, parameter calculations, and statistical procedures, please
    refer to the Methods section and the SI Appendix of the manuscript.
