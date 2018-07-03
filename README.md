# groupPlot

Tool for generating figure-ready graphs from data in an excel file. 

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

If you do not already have R installed, or your version is out of date, download the latest version here: 

https://cran.r-project.org

Next, install the latest version of RStudio Desktop: 

https://www.rstudio.com/products/rstudio/#Desktop

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
instal.packages("shiny")
```

### Installing & Running

After installing the `shiny` package, you are ready to run `group_plot` and it will prompt you to install any additional packages you are missing.
To initialize the shiny app, paste the following code in your R console and run it.

```
shiny::runGitHub("group_plot", "MrTcell")
```
## Authors

* **John Gagnon**

## License


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
