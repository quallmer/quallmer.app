# Create sample validation dataset for quallmer.app

set.seed(42)

# Sample movie review texts
texts <- c(
  "This movie was absolutely fantastic! Loved every minute.",
  "Terrible film, waste of time and money.",
  "Pretty good overall, though some parts dragged.",
  "Worst movie I've ever seen. Completely unwatchable.",
  "Masterpiece! A must-see for everyone.",
  "Not bad, but nothing special either.",
  "Brilliant performances and great storytelling.",
  "Boring and predictable from start to finish.",
  "Enjoyable entertainment, would recommend.",
  "Disappointing given all the hype.",
  "Absolutely amazing! Best film of the year.",
  "Mediocre at best, forgettable.",
  "Excellent direction and cinematography.",
  "Painful to watch, saved only by one good scene.",
  "Solid movie, good for a rainy day.",
  "Horrible acting and weak plot.",
  "Surprisingly good! Exceeded my expectations.",
  "Just okay, nothing memorable.",
  "Stunning visuals and powerful message.",
  "Complete disaster from beginning to end."
)

# Create a dataset with multiple coders and a gold standard
# Sentiment: positive, negative, neutral
sample_data <- data.frame(
  .row_id = 1:20,
  text = texts,
  # Gold standard (ground truth)
  gold_sentiment = c(
    "positive", "negative", "positive", "negative", "positive",
    "neutral", "positive", "negative", "positive", "negative",
    "positive", "neutral", "positive", "negative", "neutral",
    "negative", "positive", "neutral", "positive", "negative"
  ),
  # Coder 1 (human coder with high accuracy)
  coder1_sentiment = c(
    "positive", "negative", "positive", "negative", "positive",
    "neutral", "positive", "negative", "positive", "neutral",  # one error
    "positive", "neutral", "positive", "negative", "neutral",
    "negative", "positive", "neutral", "positive", "negative"
  ),
  # Coder 2 (LLM coder with moderate accuracy)
  coder2_sentiment = c(
    "positive", "negative", "neutral", "negative", "positive",  # one error
    "positive", "positive", "negative", "positive", "negative",  # one error
    "positive", "positive", "positive", "negative", "neutral",  # one error
    "negative", "positive", "neutral", "positive", "negative"
  ),
  # Coder 3 (another LLM with slightly lower accuracy)
  coder3_sentiment = c(
    "positive", "negative", "positive", "negative", "positive",
    "neutral", "positive", "neutral", "positive", "negative",  # one error
    "positive", "neutral", "positive", "negative", "positive",  # one error
    "negative", "positive", "positive", "positive", "negative"  # one error
  ),
  stringsAsFactors = FALSE
)

# Save the dataset for package data (accessed via data(sample_data))
usethis::use_data(sample_data, overwrite = TRUE)

# Also save as .rds for uploading through the app UI
saveRDS(sample_data, file = "inst/extdata/sample_data.rds")
