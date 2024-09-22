# NetflixData-Shiny-app  [**Readme 中文**](./ReadMe-Zh.md)
<div style="display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px;"> <img src="https://github.com/user-attachments/assets/3f938dc4-7650-44c5-8858-b49f66b2249c" height="200px" width="300px"> <img src="https://github.com/user-attachments/assets/54890027-d6de-4b24-98ac-b453c56881e8"  height="200px" width="300px"> <img src="https://github.com/user-attachments/assets/4c6aa405-4c71-4237-bad4-84e2a36d9b3b"  height="200px" width="300px"> <img src="https://github.com/user-attachments/assets/b2a83550-d373-4748-b396-d49ecfcf4b39"  height="200px" width="300px"> <img src="https://github.com/user-attachments/assets/66fe20e3-1f4f-4f10-860d-11b6d78baccc"  height="200px" width="300px"> <img src="https://github.com/user-attachments/assets/551c0a43-78eb-4cd8-8d35-e3e32de32261"  height="200px" width="300px"> </div>


This repository contains a dataset of Netflix titles and an `R` script for analyzing the data. The analysis explores various aspects of the Netflix dataset such as genres, countries of production, and release dates. The script uses R for data manipulation and visualization.

## Features and Functionalities

1. **Download Functionality**:  
   - Allows users to download data files or specific reports generated from the app.
   - Download options are available in CSV or PDF format for customized views of the dataset.

2. **Sorting Options**:  
   - Users can sort tables or visualizations by different criteria (e.g., country, numerical data, dates).
   - Sorting can be applied to both ascending and descending orders.

3. **Page Navigation**:  
   - Provides tabs or buttons to navigate between different sections of the application (e.g., Overview, Detailed Analysis, and Reports).
   - Smooth transitions between different parts of the app using Shiny's reactive elements or WebGL transitions.

4. **Interactive Bar Chart with Click-to-Explore**:  
   - The bar chart contains data related to various countries.
   - Users can click on a bar (e.g., the number in a bar chart) to navigate to a specific country’s detailed data view or to filter content for that country.

5. **Filtering Options**:  
   - Users can apply filters by categories such as region, year, or data range.
   - These filters dynamically update charts and tables.

6. **Dynamic Data Visualization**:  
   - Real-time updates in visualizations as users interact with the application (e.g., charts updating based on filters or input changes).

7. **Hover Tooltips**:  
   - Hovering over a data point, bar, or chart element displays detailed information or statistics related to that specific element.
   
8. **Data Export**:  
   - Export functionality for charts and tables, allowing users to download visualizations as images or save reports.
   
9. **Search Functionality**:  
   - A search bar allows users to search specific countries, data points, or sections within the dataset.

10. **Country-Specific Details**:  
    - Upon selecting or clicking on a specific country in a chart, detailed data for that country is displayed, including various metrics such as population, GDP, or other relevant data.

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
