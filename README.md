# rqafordiscretedata Repository

## About

This repository contains R code for conducting discrete recurrence quantification analysis, specifically code to support your use of the *crqa* R package (Coco & Dale, 2014). In addition to the helper code, we have included demonstration files, files from a tutorial presented at CogSci2021, and an interactive app for building an example recurrence plots.

### Project organizers

<https://github.com/enmccormick> 

<https://github.com/leslieblaha>

### Major changes

None at the moment.

## Files and folders included

### Meta-data files

**README.md**: This file.

**LICENSE.txt**: This code is available under the GNU General Public License, version 3 or higher ([see here](./LICENSE.txt)).

### Helper code

The **folder "helper_scripts"** contains [arqahelper.R](./helper_scripts/arqahelper.R). This file contains multiple functions that interface with the *crqa* package and various *tidyverse* packages (Wickham et al., 2019) to help you create recurrence plots and manage recurrence statistics for multiple recurrence plots. This script is not an R package.

### Demonstrations of the code

The **folder "tutorial_examples"** contains [RQA_DemonstrationCode.html](./tutorial_examples/RQA_DemonstrationCode.html). This file demonstrates (and briefly explains) how to use the functions in the helper code to conduct discrete recurrence quantification analysis.

This folder also contains the RMarkdown file used to create the demonstration HTML file. The RMD file is here: [RQA_DemonstrationCode.Rmd](./tutorial_examples/RQA_DemonstrationCode.Rmd). 

In order to re-compile the RMD file yourself, you must have:

-   installed the *crqa* and *tidyverse* packages in R

-   downloaded the helper code (arqahelper.R) into the same folder as the RMD file

### CogSci2021 Tutorial files

The **folder "tutorial_examples"** also contains files from a 3-hour tutorial given at CogSci2021, titled "Practical Interpretation and Insights with Recurrence Quantification Analysis for Decision Making Research."

This includes the two-page proceedings describing the workshop: [CogSci2021_Tutorial_Proceedings.pdf](./tutorial_examples/CogSci2021_Tutorial_Proceedings.pdf)

The lecture slides used in the workshop: [CogSci2021_Tutorial_Lecture_Slides.pdf](./tutorial_examples/CogSci2021_Tutorial_Lecture_Slides.pdf)

The exercises used by participants in the tutorial: [cogsci2021_tutorial_exercises.html](./tutorial_examples/cogsci2021_tutorial_exercises.html)

And the RMD file used to generate the exercise HTML file: [cogsci2021_tutorial_exercises.Rmd](./tutorial_examples/cogsci2021_tutorial_exercises.Rmd)

### R Shiny application for interactive recurrence plotting

The **folder "interactive_plots_shinyapp"** contains the file [app.R](./interactive_plots_shinyapp/app.R). This file is a R Shiny application that allows you to interactively edit a single recurrence plot. 

We recommend using this tool to help develop you visual expertise in reading and understanding recurrence plots. We feel that interactively creating and modifying a variety of recurrence plots can help researchers make the connection between a given plot and the event sequence producing it.

## References

1. Coco, M. I. & Dale, R. (2014). Cross-recurrence quantification analysis of categorical and continuous time series: An R package. *Frontiers in Psychology, 5,* 510.

2. Wickham et al., (2019). Welcome to the tidyverse. *Journal of Open Source Software, 4(43),* 1686, https://doi.org/10.21105/joss.01686
