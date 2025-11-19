# DP2: Pena Distance Methodology Shiny App

## Description
This Shiny application computes the DP2 index (Pena Distance Methodology) for multidimensional analysis. It allows normalization using minimum or maximum reference values, exports results to Excel, and visualizes them on an interactive map.

## Features
- Compute DP2 index for selected variables
- Choose normalization reference (minimum or maximum)
- Export results to Excel
- Interactive map visualization using shapefiles
- Clickable polygons to identify municipalities

## Installation
```bash
# Clone the repository
git clone https://github.com/yourusername/dp2-shiny-app.git
Or download the repository: Click Code → Download ZIP, extract it and open app.R in RStudio.
# Install required R packages
install.packages(c("shiny", "sf", "readxl", "writexl", "ggplot2", "dplyr", "tidyverse", "leaflet"))
```

## Usage
Run the app locally:
```R
shiny::runApp("app.R")
```
shinyapps.io: the app is published with a stable URL: https://coro-chasco.shinyapps.io/dp2-shiny-app/

## Input Files
- **Excel file** containing variables for DP2 calculation, the first of which is “CODE” (join key)
- **Shapefile** with polygons and fields:
  - `CODE` (join key)
  - `MUNICIPIO` (for identification)

## File Structure
```
dp2-shiny-app/
│
├── app.R                # Main Shiny app
├── data/                # Input data files
├── www/                 # CSS or images
└── README.md            # Documentation
```

## Output file
The application generates an Excel file named DP2_outcomes_iterations.xlsx containing the following sheets:
- **Final Index**
  - ID: Join key (same as CODE in the shapefile).
  - DP2: Final DP2 index value (scaled as IBS = 100 × DP2 / mean).
- **Correlations**
  - Correlation matrix between all input variables.
- **Correlations with DP2**
  - Variable: Name of the variable.
  - Correlation: Correlation with the DP2 index.
- **Final Weights**
  - Variable: Name of the variable.
  - Weight: Final weight assigned in the DP2 calculation.
  - Orden: Order of inclusion in the iterative algorithm.
- **Iteration_X (for each iteration)**
  - ID: Join key.
  - DP2: Intermediate DP2 values for that iteration.
  - Variable / Weight: Weights at that iteration.
  - Orden: Variable order at that iteration.

## Citation
Chasco, C., & Sánchez, B. (2025). *DP2: Pena Distance Methodology (1.0.0)* [Data set]. B2SHARE.  
https://doi.org/10.23728/b2share.1e1hy-ta519  
*Developed with the assistance of Copilot AI (2025).*

## License
This repository uses **dual licensing**:
- **Code**: [MIT License](https://opensource.org/licenses/MIT)
- **Documentation and data**: [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/)

## Methodology
The DP2 (Pena Distance) methodology is an iterative approach that assigns weights to partial indicators based on their correlation with a global index, eliminating redundant variance and ensuring multidimensional comparability. It is widely used for quality of life and socioeconomic indicators.

For a detailed explanation of the method, see the full document:  
[DP2_methodology.pdf](https://github.com/corochasco/dp2-shiny-app/blob/main/DP2_methodology.pdf)

### Convergence Settings
The DP2 algorithm is iterative and stops when the difference between two consecutive iterations is below a tolerance (`tol`) or when the maximum number of iterations (`max_iter`) is reached.

- **tol**: Convergence threshold (default: 1e-6). Lower values require stricter convergence.
- **max_iter**: Maximum iterations allowed (default: 100).

If the algorithm does not converge within `max_iter`, the app will display a warning and return the best approximation. You can increase `max_iter` or adjust `tol` to improve convergence.

---

## Screenshot
![DP2 Shiny App Screenshot](www/screenshot.png)
