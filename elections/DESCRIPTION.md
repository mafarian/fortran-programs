Hey!

Based on 2 data sources (**Ibope** and **Datafolha**), we collected the voting intention to 2018 Brazil's president
from differents dates, and built an optimization problem to predict the election's result. With the sources's informations,
we used the [Spectral Projected Gradient](fortran-programs/elections/subrotspg.for) method to calculate a matrix that fitted
into the data pattern. So, applying that matrix (transference of votes's array) to the last voting intention, we obtained a prediction to the election's result. 

> The complete report is [here](https://www.overleaf.com/read/vdzhmrgpdcnv) and some crutial corrections is
[here](https://www.overleaf.com/read/jhbmnnpjvfvs), both in **Portuguese**.

## Importants variables ##

* Objective function and variables are [here](https://www.overleaf.com/read/vdzhmrgpdcnv) on the second section (_Modelagem_). Where:
  - _Yi(t)_: the percentage of votes of the candidate _i_ in research _t_
  - _Xij_: the percentage of votes of the candidate _i_ trasnferred by candidate _j_
  - _m_: total number of candidates, including null/blank and undecided votes
  - _p_: total number of researches used as input data 
  - _gama_: weight associated to the constraint, to ensure that it will be fulfilled
  
  
--
*Stark*.
