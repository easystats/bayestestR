# print.describe_posterior

    Code
      describe_posterior(m, verbose = FALSE)
    Output
      Summary of Posterior Distribution
      
      Parameter   | Median |         95% CI |     pd |          ROPE | % in ROPE
      --------------------------------------------------------------------------
      (Intercept) |   0.96 | [-0.81,  2.51] | 90.00% | [-0.10, 0.10] |     2.54%
      child       |  -1.16 | [-1.36, -0.94] |   100% | [-0.10, 0.10] |        0%
      camper      |   0.73 | [ 0.54,  0.91] |   100% | [-0.10, 0.10] |        0%
      
      Parameter   |  Rhat | ESS
      -------------------------
      (Intercept) | 1.011 | 110
      child       | 0.996 | 278
      camper      | 0.996 | 271

---

    Code
      describe_posterior(m, effects = "all", component = "all", verbose = FALSE)
    Output
      Summary of Posterior Distribution
      
      Parameter   | Median |         95% CI |     pd |          ROPE | % in ROPE
      --------------------------------------------------------------------------
      (Intercept) |   0.96 | [-0.81,  2.51] | 90.00% | [-0.10, 0.10] |     2.54%
      child       |  -1.16 | [-1.36, -0.94] |   100% | [-0.10, 0.10] |        0%
      camper      |   0.73 | [ 0.54,  0.91] |   100% | [-0.10, 0.10] |        0%
      
      Parameter   |  Rhat | ESS
      -------------------------
      (Intercept) | 1.011 | 110
      child       | 0.996 | 278
      camper      | 0.996 | 271
      
      # Fixed effects (zero-inflated)
      
      Parameter   | Median |         95% CI |     pd |          ROPE | % in ROPE
      --------------------------------------------------------------------------
      (Intercept) |  -0.48 | [-2.03,  0.89] | 78.00% | [-0.10, 0.10] |    10.59%
      child       |   1.85 | [ 1.19,  2.54] |   100% | [-0.10, 0.10] |        0%
      camper      |  -0.88 | [-1.61, -0.07] | 98.40% | [-0.10, 0.10] |     0.85%
      
      Parameter   |  Rhat | ESS
      -------------------------
      (Intercept) | 0.997 | 138
      child       | 0.996 | 303
      camper      | 0.996 | 292
      
      # Random effects (conditional) (SD/Cor: persons)
      
      Parameter   | Median |         95% CI |   pd |          ROPE | % in ROPE
      ------------------------------------------------------------------------
      (Intercept) |   1.42 | [ 0.71,  3.58] | 100% | [-0.10, 0.10] |        0%
      
      Parameter   |  Rhat | ESS
      -------------------------
      (Intercept) | 1.010 | 126
      
      # Random effects (zero-inflated) (SD/Cor: persons)
      
      Parameter   | Median |         95% CI |   pd |          ROPE | % in ROPE
      ------------------------------------------------------------------------
      (Intercept) |   1.30 | [ 0.63,  3.41] | 100% | [-0.10, 0.10] |        0%
      
      Parameter   |  Rhat | ESS
      -------------------------
      (Intercept) | 0.996 | 129

