# plotGrouper



This package was designed as a tool for generating figure-ready graphs from data in an excel file. It borrows heavily from packages developed by others, including the `ggplot2` and `dplyr` from the tidyverse and batch statistical calculations from `ggpubr`.

Plots can be made using combinations of geoms such as

![alt text](https://raw.githubusercontent.com/jdgagnon/groupPlot/master/Examples/Bar_example.png)



![alt text](https://raw.githubusercontent.com/jdgagnon/groupPlot/master/Examples/Violin_example.png)



![alt text](https://raw.githubusercontent.com/jdgagnon/groupPlot/master/Examples/Box_example.png)



![alt text](https://raw.githubusercontent.com/jdgagnon/groupPlot/master/Examples/Crossbar_example.png)


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

If you do not already have R installed, or your version is out of date, download the latest version [Here](https://cran.r-project.org). 



Next, install the latest version of [RStudio Desktop](https://www.rstudio.com/products/rstudio/#Desktop).

Next, download the sample data [excel file](https://github.com/MrTcell/groupPlot/blob/master/iris.xlsx)

Finally, open RStudio and paste the following code in your R console and run it in order to install the required packages.

```
install.packages("devtools")
devtools::install_github("tidyverse/ggplot2")
install.packages("tidyverse")
install.packages("shiny")
install.packages("Hmisc")
install.packages("scales")
install.packages("readxl")
install.packages("gridExtra")
install.packages("egg")
install.packages("ggpubr")
install.packages("shinyjs")
install.packages("shinythemes")
install.packages("colourpicker")
install.packages("shiny")
```

### Installing & Running

After installing the `shiny` package, you are ready to run `groupPlot`.

To initialize the shiny app, paste the following code in your R console and run it.

```
shiny::runGitHub("plotGrouper", "jdgagnon")
```

Once the web app opens, browse to wherever you downloaded the example data file, "iris.xlsx", and open it.


## Authors

* **John Gagnon**

## License

GNU GPL-3.0-or-later

[licence](https://www.gnu.org/licenses/gpl.txt)

## Acknowledgments & Packages Used

* tidyverse
* Hmisc
* readxl
* gridExtra
* egg
* ggpubr
* shinyjs
* shinythemes
* colourpicker
