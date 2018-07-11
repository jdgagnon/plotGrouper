<img align = "left" src="www/logo_small.png" width ="150"/>
<br>
This package was designed as a tool for generating figure-ready graphs from data in an excel file. It borrows heavily from packages developed by others, including the `ggplot2` and `dplyr` from the tidyverse and batch statistical calculations from `ggpubr`.
<br><br>
Plots can be made using combinations of geoms including bar, violin, box, crossbar, density, point, line, and errorbar.
<br><br>
<img src="www/Bar_example.png" width ="350"/>&nbsp; <img src="www/Violin_example.png" width ="350"/>
<br><br><br><br>
<img src="www/Box_example.png" width ="350" />&nbsp; <img src="www/Crossbar_example.png" width ="350"/>



## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

If you do not already have R installed, or your version is out of date, download the latest version [Here](https://cran.r-project.org). 



Next, install the latest version of [RStudio Desktop](https://www.rstudio.com/products/rstudio/#Desktop).

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

After installing the `shiny` package, you are ready to run `plotGrouper`.

To initialize the shiny app, paste the following code in your R console and run it.

```
shiny::runGitHub("plotGrouper", "jdgagnon")
```

Once the web app opens, you can access the `iris` dataset by clicking the iris button to learn how to use the app.


## Authors

* **John Gagnon**

## License

GNU GPL-3.0-or-later

[licence](https://www.gnu.org/licenses/gpl.txt)

## Acknowledgments

Logo designed by Jarrett Dow
