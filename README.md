# Bitcoin Transaction per Block Analysis

This repository contains the code and data for the analysis and modeling of Bitcoin transactions per block. The project is structured to follow a comprehensive data analysis and machine learning pipeline using `targets` in R.

## Repository Structure

- `/_targets/` - Directory for 'targets' package outputs
  - `_targets.R` - Main targets pipeline script
- `/.gitignore` - Git ignore file
- `/data/` - Directory for raw and processed data
- `/docs/` - Documentation files
- `/output/` - Directory for output files
- `/scripts/` - Directory for R scripts
  - `0-data-raw-extraction.R` - Script for raw data extraction
  - `1-data-preprocessing.R` - Script for data preprocessing
  - `1-exploratory-data-analysis.R` - Script for exploratory data analysis
  - `2-model-evaluation.R` - Script for model evaluation
- `/ml-reg-btc-tx-per-block.Rproj` - RStudio project file
- `/run-pipeline.R` - Script to run the entire pipeline
- `/setup-packages.R` - Script to install and load required packages
- `/README.md` - This README file

## Setup Instructions

### Prerequisites

Make sure you have the following software installed:

- [R](https://cran.r-project.org/)
- [RStudio](https://www.rstudio.com/)
- [Git](https://git-scm.com/)

### Cloning the Repository

To clone the repository to your local machine, run the following command:

```sh
git clone https://github.com/davlaj/ml-reg-btc-tx-per-block.git
```

### Installing Required Packages

Open the RStudio project file ml-reg-btc-tx-per-block.Rproj in RStudio. Install the required packages by sourcing the setup-packages.R script:

```sh
source("setup-packages.R")
```

This script will install and load all the necessary packages for the project.

### Running the Pipeline

The entire data analysis and modeling pipeline is managed using the targets package. To run the pipeline, execute the run-pipeline.R script:

```sh
source("run-pipeline.R")
```

## Scripts Overview

- **0-data-raw-extraction.R**: Extract raw data from the Bitcoin node.
- **1-data-preprocessing.R**: Preprocess the extracted data.
- **1-exploratory-data-analysis.R**: Perform exploratory data analysis.
- **2-model-evaluation.R**: Evaluate models on the preprocessed data.
- **run-pipeline.R**: Run the entire data analysis and modeling pipeline.
- **setup-packages.R**: Script to install and load required packages.

## Usage

1. Ensure you have the necessary packages installed by running:
    ```r
    source("setup-packages.R")
    ```

2. Run the entire pipeline:
    ```r
    source("run-pipeline.R")
    ```

This will execute the entire data analysis and modeling pipeline, generating the necessary output files in the `output/` directory.

### Output
The results of the analysis and modeling are stored in the output/ directory. This includes visualizations, summary statistics, and model performance metrics.

## Contributing
If you would like to contribute to this project, please fork the repository and submit a pull request with your changes.

## License
This project is licensed under the MIT License.

## Contact
For any questions or issues, please open an issue in the GitHub repository.
