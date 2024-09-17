# equivalence test, rstanarm

    Code
      print(out)
    Output
      # Test for Practical Equivalence
      
        ROPE: [-0.18 0.18]
      
      Parameter   |       H0 | inside ROPE |        95% HDI
      -----------------------------------------------------
      (Intercept) | Rejected |      0.00 % | [-2.68, -0.50]
      size        | Accepted |    100.00 % |  [-0.04, 0.07]
      period2     | Rejected |      0.00 % | [-1.61, -0.36]
      period3     | Rejected |      0.00 % | [-1.77, -0.40]
      period4     | Rejected |      0.00 % | [-2.52, -0.76]
      
      

---

    Code
      print(out)
    Output
      # Test for Practical Equivalence
      
      Parameter   |        H0 | inside ROPE |        95% HDI |          ROPE
      ----------------------------------------------------------------------
      (Intercept) | Undecided |     15.82 % | [-2.68, -0.50] | [-1.00, 1.00]
      size        |  Accepted |    100.00 % |  [-0.04, 0.07] | [-0.10, 0.10]
      period2     |  Rejected |      0.00 % | [-1.61, -0.36] |  [0.00, 2.00]
      period3     |  Accepted |    100.00 % | [-1.77, -0.40] | [-2.00, 0.00]
      period4     |  Rejected |      0.00 % | [-2.52, -0.76] | [-0.10, 0.10]
      
      

# equivalence test, df

    Code
      print(out)
    Output
      # Test for Practical Equivalence
      
        ROPE: [-0.10 0.10]
      
      Parameter   |       H0 | inside ROPE |        95% HDI
      -----------------------------------------------------
      (Intercept) | Rejected |      0.00 % | [-2.68, -0.50]
      size        | Accepted |    100.00 % |  [-0.04, 0.07]
      period2     | Rejected |      0.00 % | [-1.61, -0.36]
      period3     | Rejected |      0.00 % | [-1.77, -0.40]
      period4     | Rejected |      0.00 % | [-2.52, -0.76]
      
      

---

    Code
      print(out)
    Output
      # Test for Practical Equivalence
      
      Parameter   |        H0 | inside ROPE |        95% HDI |          ROPE
      ----------------------------------------------------------------------
      (Intercept) | Undecided |     15.82 % | [-2.68, -0.50] | [-1.00, 1.00]
      size        |  Accepted |    100.00 % |  [-0.04, 0.07] | [-0.10, 0.10]
      period2     |  Rejected |      0.00 % | [-1.61, -0.36] |  [0.00, 2.00]
      period3     |  Accepted |    100.00 % | [-1.77, -0.40] | [-2.00, 0.00]
      period4     |  Rejected |      0.00 % | [-2.52, -0.76] | [-0.10, 0.10]
      
      

