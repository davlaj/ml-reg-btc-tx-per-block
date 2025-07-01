# Bitcoin Transaction per Block Analysis

This repository contains the code and data for modeling Bitcoin transactions per block. The goal is to predict the number of transactions in the next Bitcoin block. The project is structured to follow a comprehensive data analysis and machine learning pipeline using the `targets` package in R.

## Repository Structure

- `_targets.R` - Main targets pipeline script
- `/.gitignore` - Git ignore file
- `/data/` - Directory for raw and processed data
- `/docs/` - Documentation files
- `/output/` - Directory for output files
  - `exploratory-data-analysis/` - Contains outputs from `/scripts/1-exploratory-data-analysis.R`, including visualizations and statistics
  - `model-evaluation/` - Contains output files from `/scripts/2-model-evaluation.R`, including performance metrics and model comparisons
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

Ensure you have the following software installed:

- [R](https://cran.r-project.org/)
- [RStudio](https://www.rstudio.com/)
- [Git](https://git-scm.com/)

### Setting Up the RStudio Project

#### Cloning the Repository

Before cloning the repository, you need a GitHub personal access token. Follow the instructions [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) to create one.

1. Open RStudio.
2. Go to `File` -> `New Project`.
3. Select `Version Control` -> `Git`.
4. In the "Repository URL" field, paste: `https://github.com/davlaj/ml-reg-btc-tx-per-block.git`.
5. In the authentication fields, use your GitHub username and the personal access token as the password.
6. Choose the directory where you want to clone the repository and click `Create Project`.

This will clone the repository and set up a new RStudio project linked to this Git repository.

#### Installing Required Packages

Once the project is set up:

1. Open the RStudio project file `ml-reg-btc-tx-per-block.Rproj`. This sets the working directory to the project's root folder automatically and ensures that all paths in the scripts run correctly relative to the project directory.
2. Run the `setup-packages.R` script located in the main directory. The following command installs and loads all necessary packages.

```sh
source("setup-packages.R")
```

#### Setting up environment variables

To securely connect to your Bitcoin node, create a file named `.Renviron` in the project root directory and add:

```
BTC_RPC_USER=your_rpc_username
BTC_RPC_PASS=your_rpc_password
```

This file is automatically read by R when you open the project.
Make sure **not to commit this file** by keeping `.Renviron` listed in your `.gitignore`.

## Scripts Overview

- **0-data-raw-extraction.R**: Extract raw data from the Bitcoin node.
- **1-data-preprocessing.R**: Preprocess the extracted data.
  - Preprocessing steps:
      - 0\. Formatting
      - 1\. Missing Values
      - 2\. Temporal Feature Engineering
      - 3\. Outliers
      - 4\. Interaction and Polynomial Feature Engineering
      - 5\. Encoding
      - 6\. Data Integrity Checks
- **1-exploratory-data-analysis.R**: Perform exploratory data analysis.
  - Output files are numbered according to the preprocessing steps
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

Additionally, it is possible to run each script independently or execute specific parts of a script by manually managing the dependencies.

### Output
The results of the analysis and modeling are stored in the output/ directory. This includes visualizations, summary statistics, and model performance metrics.

## Contributing
If you would like to contribute to this project, please fork the repository and submit a pull request with your changes.

## License
This project is licensed under the MIT License.

## Contact
For any questions or issues, please open an issue in the GitHub repository.

## Roadmap / In progress

This project is a work in progress. Upcoming enhancements may include:

- Adding advanced modeling techniques (e.g., GAMs, hyperparameter tuning)
- Improving output visualizations and documentation
- Building an interactive dashboard for exploration
- Refining pipeline automation and reproducibility

The current version already demonstrates a complete and functional pipeline from data extraction to model evaluation.
