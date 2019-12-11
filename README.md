A Shiny app to solve the traveling salesman problem with Tabu Search.

To run on your local machine, paste the following into your R console:

```R
install.packages(c("shiny", "maps", "geosphere"), repos="http://cran.rstudio.com/")
library(shiny)
runGitHub("tsp-ts", "rodrigovmoraes", ref="TSP-TS")
```

![](https://github.com/rodrigovmoraes/tsp-ts/blob/TSP-TS/screenshot.gif?raw=true)
