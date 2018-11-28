# ps1_R

run python in r 
```{python engine.path="C:\\Program Files\\Python36\\python.exe"}
print("hi")
import numpy as np
```

run stata in r
```{r setup, echo=FALSE, message=FALSE}
library(Statamarkdown)
stataexe="C:/VApps/Stata_SE/15/StataSE-64.exe"
knitr::opts_chunk$set(engine.path=stataexe,comment="")
#knitr::opts_chunk$set(echo = TRUE)
```
