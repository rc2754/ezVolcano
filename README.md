# ezVolcano
Highly customizable tool for charting interactive volcano plots. Most aesthetic settings are customizable. Click on datapoints to add/remove annotation labels; drag them around to reposition. Alternatively, you can use the batch input button or the selectizeInput.

Use the camera button on the Plotly toolbar to export as .PNG file.

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

## Usage
## Authors
This project was developed by Richard Chen in the Proteomics Core Facility at Brown University.

Source code from the `easylabel` R package developed by Myles Lewis (https://github.com/myles-lewis/easylabel) was used for interactive labels in ezVolcano.

## Contact


## License


