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

# Launch the validation app
validate_app()
```

## Requirements

This package requires the `quallmer` package to be installed.

## Documentation

For more information about the quallmer ecosystem, see the main [quallmer package documentation](https://seraphinem.github.io/quallmer/).
