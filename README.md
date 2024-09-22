# NetflixData-Shiny-app  

This repository contains a dataset of Netflix titles and an `R` script for analyzing the data. The analysis explores various aspects of the Netflix dataset such as genres, countries of production, and release dates. The script uses R for data manipulation and visualization.

## Project Structure

- **app.R**: The R script that contains the code for analyzing the Netflix dataset.
- **netflix_titles.csv**: The dataset used for the analysis, containing information on Netflix shows and movies.

## Dataset

The dataset `netflix_titles.csv` contains the following fields:
- `show_id`: Unique ID for each show.
- `type`: The type of content (Movie or TV Show).
- `title`: Title of the content.
- `director`: Director of the content (if available).
- `cast`: Cast of the content.
- `country`: The country where the content was produced.
- `date_added`: The date when the content was added to Netflix.
- `release_year`: The release year of the content.
- `rating`: The rating given to the content (e.g., PG, R).
- `duration`: Duration of the content (in minutes for movies and seasons for TV shows).
- `listed_in`: Genres the content is listed in.
- `description`: A short description of the content.

## How to Run the Code

1. Clone the repository:
    ```bash
    git clone https://github.com/your-username/netflix-titles-analysis.git
    ```

2. Ensure you have R installed along with the necessary packages (e.g., `tidyverse`, `ggplot2`).

3. Run the `app.R` file in your R environment to perform the analysis.

## Analysis Highlights

The analysis focuses on:
- **Content Distribution**: Analyze the number of TV shows vs. movies.
- **Country-wise Production**: Explore the countries that produce the most content.
- **Genre Exploration**: Visualize the distribution of content by genre.
- **Release Year Trends**: Study trends in content release over the years.

## Results

You can find detailed insights and visualizations in the `app.R` script. The key takeaways include:
- A growing trend of Netflix content releases over the years.
- The dominance of specific genres like documentaries, comedies, and dramas.
- The United States being the top producer of Netflix content.

## Future Improvements

- Include more detailed content rating analysis.
- Develop a recommendation system based on genre or other factors.
- Explore further insights on global vs. regional content distribution.

## License

This project is licensed under the Apache License - see the LICENSE file for details.
