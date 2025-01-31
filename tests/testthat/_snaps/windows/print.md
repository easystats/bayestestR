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
      
      Parameter   |  Rhat |    ESS
      ----------------------------
      (Intercept) | 1.011 | 110.00
      child       | 0.996 | 278.00
      camper      | 0.996 | 271.00

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
      
      Parameter   |  Rhat |    ESS
      ----------------------------
      (Intercept) | 1.011 | 110.00
      child       | 0.996 | 278.00
      camper      | 0.996 | 271.00
      
      # Fixed effects (zero-inflated)
      
      Parameter   | Median |         95% CI |     pd |          ROPE | % in ROPE
      --------------------------------------------------------------------------
      (Intercept) |  -0.48 | [-2.03,  0.89] | 78.00% | [-0.10, 0.10] |    10.59%
      child       |   1.85 | [ 1.19,  2.54] |   100% | [-0.10, 0.10] |        0%
      camper      |  -0.88 | [-1.61, -0.07] | 98.40% | [-0.10, 0.10] |     0.85%
      
      Parameter   |  Rhat |    ESS
      ----------------------------
      (Intercept) | 0.997 | 138.00
      child       | 0.996 | 303.00
      camper      | 0.996 | 292.00
      
      # Random effects (conditional) Intercept: persons
      
      Parameter |    Median |         95% CI |     pd |          ROPE | % in ROPE
      ---------------------------------------------------------------------------
      persons.1 |     -0.99 | [-2.68,  0.80] | 92.00% | [-0.10, 0.10] |     2.12%
      persons.2 | -4.65e-03 | [-1.63,  1.66] | 50.00% | [-0.10, 0.10] |    13.98%
      persons.3 |      0.69 | [-0.95,  2.34] | 79.60% | [-0.10, 0.10] |     5.08%
      persons.4 |      1.57 | [-0.05,  3.29] | 96.80% | [-0.10, 0.10] |     1.27%
      
      Parameter |  Rhat |    ESS
      --------------------------
      persons.1 | 1.007 | 106.00
      persons.2 | 1.013 | 109.00
      persons.3 | 1.010 | 114.00
      persons.4 | 1.009 | 114.00
      
      # Random effects (zero-inflated) Intercept: persons
      
      Parameter | Median |         95% CI |     pd |          ROPE | % in ROPE
      ------------------------------------------------------------------------
      persons.1 |   1.10 | [-0.23,  2.72] | 94.80% | [-0.10, 0.10] |     3.39%
      persons.2 |   0.18 | [-0.94,  1.58] | 63.20% | [-0.10, 0.10] |    14.83%
      persons.3 |  -0.30 | [-1.79,  1.02] | 64.00% | [-0.10, 0.10] |    12.29%
      persons.4 |  -1.45 | [-2.90, -0.10] | 98.00% | [-0.10, 0.10] |        0%
      
      Parameter |  Rhat |    ESS
      --------------------------
      persons.1 | 0.997 | 166.00
      persons.2 | 0.996 | 154.00
      persons.3 | 0.997 | 154.00
      persons.4 | 1.000 | 189.00
      
      # Random effects (conditional) SD/Cor: persons
      
      Parameter   | Median |         95% CI |   pd |          ROPE | % in ROPE
      ------------------------------------------------------------------------
      (Intercept) |   1.42 | [ 0.71,  3.58] | 100% | [-0.10, 0.10] |        0%
      
      Parameter   |  Rhat |    ESS
      ----------------------------
      (Intercept) | 1.010 | 126.00
      
      # Random effects (zero-inflated) SD/Cor: persons
      
      Parameter   | Median |         95% CI |   pd |          ROPE | % in ROPE
      ------------------------------------------------------------------------
      (Intercept) |   1.30 | [ 0.63,  3.41] | 100% | [-0.10, 0.10] |        0%
      
      Parameter   |  Rhat |    ESS
      ----------------------------
      (Intercept) | 0.996 | 129.00

