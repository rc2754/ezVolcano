# ezVolcano
Shiny R app for charting interactive volcano plots. Most aesthetic settings are customizable. Click on data points to add/remove annotation labels, which can be repositioned by dragging.

Online version is available here: 

## Table of Contents
- [Local setup](#local-setup)
- [Usage](#usage)
- [Authors](#authors)
- [Contact](#contact)
- [License](#license)

## Local setup
In addition to the online version, you can download the scripts and run them locally through RStudio. 

1. Download `server.R` and `ui.R` and place these scripts in a folder
2. Open RStudio and run the app with the following code:

```bash
library(shiny)
setwd("C:/Users/chpsh/Desktop/ezVolcano")
runApp()
```
Change `setwd` to wherever you placed the scripts.

You'll need these R packages installed:

```bash
install.packages(c("shiny", "plotly", "data.table", "dplyr", "shinyjs", "readxl"))
```

## Usage
Use the `Load data file` button to load your data file. It can be `.txt` `.csv` `.tsv` or `.xslx` (first sheet only).

1. First column must be your gene name or other identifier
2. Second column must be your fold change or other effect size
3. Third column must be your p-value or other statistical significance value

Additional columns are ignored.

Use the `Save plot` button to save all of your datapoints, labels, and aesthetic settings. 
Use the `Save configuration` button to save only your aesthetic settings. 
These save files can be reloaded for later use.

Use the camera button on the Plotly toolbar to export as .PNG file.

Example files are available for download in the app.

## Authors
`ezVolcano` was developed by Richard Chen at the Proteomics Core Facility of Brown University.

`ezVolcano` uses source code from the `easylabel` R package by Myles Lewis (https://github.com/myles-lewis/easylabel) for interactive labels.

## Contact
Open an issue on Github or contact `richard_chen1@brown.edu` with any questions.

## License
`The MIT License (MIT)`

Copyright (c) 2025 Richard Chen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

