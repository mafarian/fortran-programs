Hey!

Based on 2 data sources (**Ibope** and **Datafolha**), we collected the voting intention to 2019 Brazil's president
from differents dates, and built an optimization problem to predict the election's result. With the sources's informations,
we used the [Spectral Projected Gradient](fortran-programs/elections/subrotspg.for) method to calculate a matrix that fitted
into the data pattern. So, applying that matrix to the last voting intention, we obtained a prediction to the election's result.

> The complete report is [here](https://www.overleaf.com/read/vdzhmrgpdcnv) and some crutial corrections is
[here](https://www.overleaf.com/read/jhbmnnpjvfvs), both in **Portuguese**.

--
*Stark*.
