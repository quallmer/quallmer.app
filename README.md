# quallmer.app

Interactive Shiny Application for Quallmer Validation

## Overview

`quallmer.app` provides an interactive Shiny application for working with qualitative data coded using the `quallmer` package. The app supports:

- **Manual coding**: Code qualitative data directly in an intuitive interface
- **Review LLM-generated coding**: Examine and validate AI-produced annotations
- **Compute agreement metrics**: Calculate inter-rater reliability and validate against gold standards

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("SeraphineM/quallmer.app")
```

## Usage

```r
library(quallmer.app)

# Launch the quallmer app
qlm_app()
```

### Using the Sample Data

The package includes sample movie review data to help you learn the app:

```r
# Get the path to the sample data file
sample_file <- system.file("extdata", "sample_data.rds", package = "quallmer.app")

# Launch the app
qlm_app()

# In the app, upload the file at the path shown by:
print(sample_file)
```

The sample dataset contains 20 movie reviews coded by three coders with a gold standard, perfect for testing inter-rater reliability and gold-standard validation features.

## Requirements

This package requires the `quallmer` package to be installed.

## Documentation

For more information about the quallmer ecosystem, see the main [quallmer package documentation](https://quallmer.github.io/quallmer/).
