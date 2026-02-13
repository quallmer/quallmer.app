# quallmer.app 0.1.0

* Initial CRAN submission.
* Companion package to [quallmer](https://github.com/quallmer/quallmer) providing
  an interactive Shiny application for manual coding, LLM output review,
  and inter-rater reliability calculation.
* Three operating modes:

  - **Manual coding (blind)**: Text-focused interface for human annotation
  - **LLM checking**: Review and validate LLM-generated annotations
  - **Validation/Agreement**: Compute inter-rater reliability and gold-standard validation metrics

* Supports multiple measurement levels: nominal, ordinal, interval, and ratio.
* Computes reliability metrics including Krippendorff's alpha, Cohen's/Fleiss' kappa,
  ICC, and percent agreement.
* Automatic progress saving with resume capability.
* Includes sample dataset for demonstration and testing.
