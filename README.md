# FinancialEvolutionCalculator

An user-friendly RShiny application designed to simulate the financial evolution year-by-year based on paramter values inserted by the user.


## Introduction

FinancialEvolutionCalculator is a web-based dashboard built using RShiny. It offers users a detailed financial calculator aimed to help analyze financial scenarios over the years.

PS.: The application is hosted on the free server provided by the Posit team, which may introduce some latency.


## Home Page

In the home page you'll find in depth explanations about the calculator and all financial parameters used in the calculations.


## Calculator Features
- Under the hood, the calculator uses a complicated mathematical formula with over 50 parameters to calculate several financial metrics year-by-year.
- The key metrics calculated are your Total Asset Value (a.k.a. your net worth), Total Income and Total Expenses.
- The calculator also performs a sensitivity analysis, one parameter at a time, to identify the most important parameters in your financial evolution.


## Installation & Setup
- The app is currently hosted on a free server provided by the Posit team. As a free server, you may experience slower response times during usage.
- In case you'd like to run the app locally simply clone this repository, install dependencies from the renv.lock file and launch the app.

### Usage Guide
- Navigating the App:
  - Home Page:
This is where you'll find detailed explanations about the calculator and in-depth analysis of template scenarios created to highlight the usefulness of the calculator.

  - Financial Calculator:
Select different template scenarios and adjust parameters accordingly to see how choices influence financial evolution over time.

- Interactivity and Visualizations:
Using reactivity, the calculator responds almost immediately to parameter changes by the user by re-calculating the scenario evolution and plotting the key financial parameters.

## Project Structure
A brief overview of the technical development of the calculator is provided in the home page.

## Contributing & Roadmap
I welcome contributions and feedback:

- Contributing:
Feel free to open issues or submit pull requests with improvements.
- Roadmap:
Future updates may include performance enhancements, additional modules, and expanded scenario options.


## License & Acknowledgments
This project is released under [MIT license](LICENSE). Special thanks to the Posit team for providing the free hosting server and to all contributors who have helped develop and refine this application. Special thanks to the professional financial advisors that helped me understand the field.
