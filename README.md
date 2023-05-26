# Landscape of Construction Sector in Greater Philadelphia 

This project maps out the construction landscape of Greater Philadelphia as part of the collaboration between the PAGE team and Research team at the Economy League of Greater Philadelphia. 

## Project Description 

Federal government's response to the COVID-19 pandemic and the focus of the Biden-Harris administeration on revitalizing infrastructure across cities in the U.S. means that that local governments have unprecedented availability of funds to improve and create new infrastructure projects in their communities. 

This project aims to understand the current and historical landscape of the construction sector in Greater Philadelphia in terms of the gaps in diversity, inclusion and equity (DEI) in this sector. We lay out the size of the construction sector by employment and the number of employers by ethnicity in construction in Greater Philadelphia. By identifying the gaps in employment and minority-owned businesses who are engaged in real estate and construction industry, we aim to highlight how federal funding can be used to fill these gaps in Greater Philadelphia. 

We hope to present these results at the Real Estate and Construction event hosted by PAGE in August 2023 to an construction experts, employers and policy makers. 

## Install and Run the Project 

We conducted our analysis using the following publicly available datasets: 

- [American Community Survey (ACS)](https://www.census.gov/programs-surveys/acs/): Construction Sector Size and Employment Demographics
- [Integrated Public Use Micrdata Series USA (IPUMS)](https://usa.ipums.org/usa/): Construction Employer Demographics
- [Current Employment Statistics (CES)](https://www.bls.gov/data/): Overal Employment Trends in Construction Sector
- [Quarterly Census of Empployment and Wages (QCEW)](https://www.bls.gov/data/): Wages in Construction Sector
- [Consumer Price Index - Urban (CPI)](https://www.bls.gov/data/): CPI index to adjust wages for inflation 
- [Federal Awards Data Archive](https://www.usaspending.gov/download_center/award_data_archive): Data on all federal awards and grants for FY 2023



In order to run this project on your local repository, please follow these steps: 

1. `1.Data_Wrangle.R`: This R file contains the code for importing all datasets from publicly available websites and wrangling them for visualization. All datasets are in the folder `Data`, except for the data extract from IPUMS. The IPUMS data is too large to be directly uploaded to Github. In order to create the same data extract from IPUMS, please run the `read_ipums_ddi("Data/usa_00024.xml")` code using the `ipumsr` package in R. Running `1.Data_Wrangle.R` file will take some time because the large nature of the datasets. After creating your own IPUMS data extract, you should be able to run the entire file in one go without errors. On the other hand, you can run the individual sections of the code as well. 

2. `2a.Visuals_CES.R`: This file visualizes the overall employment trends and size of the construction sector in Greater Philadelphia from 1990 - 2023. 

3. `2b.Visuals_ACS.R`: Visualizations of the employment trends in the construction sector by gender and ethnicity.

4. `2c.Visuals_IPUMS.R`: Visualization of employer trends in the construction sector by gender and ethnicity. 

5. `2d.Visuals_OEWS.R`: Wage trends in the construction sector.

6. `2e.Visuals_GISMaps.R`: Construction trends mapped by county and metropolitan regions in the U.S.

## Credits

The entire data collection, analysis and visualization in this repository was done by Haseeb Bajwa. The final report, based on this analysis, is being collaborated upon with Michael Shields and Kenyatta James. You contact them on Linked In: 

- [Haseeb Bajwa](https://www.linkedin.com/in/haseeb-bajwa/)
- [Michael Shields](https://www.linkedin.com/in/mike-shields-09462715a/)
- [Kenyatta James](https://www.linkedin.com/in/kenyattajames/)

## License 

MIT License

Copyright (c) 2023 Economy League of Greater Philadelphia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
