# Netflix Recommender (R)

A personalized movie recommendation system built in R, using Netflix's movie metadata and simulated user viewing history.

## Features

- User preference modeling using genre, rating, runtime, release year, and content rating.
- Dual recommender systems:
  - **Variance-based weighting**
  - **Confidence Interval-based weighting**
- Genre similarity & content rating affinity scoring
- Visualization of user genre preferences (3D pie chart) and rating distribution (histogram)
- Top 50 most popular movies across users

## Stack

- **Language**: R
- **Libraries**: `ggplot2`, `plotrix`
- **Data**: Netflix titles + simulated user data

## Run the Project

1. Place `netflix_titles.csv` and `random_user_movies_updated.csv` into the `datasets/` folder.
2. Open `recommender.R` in RStudio.
3. Run the script to view recommendations and generate visualizations.

## Output Example

- Top recommended movies per user
- Genre preference distribution
- 50 most popular movies across users
