# KrakenR: An R Package for Kraken API Integration

<center>

![Kraken Digital Asset Exchange](https://i0.wp.com/blog.kraken.com/wp-content/uploads/2024/09/Blog_Header_1535x700.png?w=1535&ssl=1)

</center>

## Overview

`KrakenR` is an R package that provides a seamless interface for retrieving and interacting with cryptocurrency data from Kraken, a leading cryptocurrency exchange. This package is tailored to offer essential tools for fetching asset prices, trading volumes, and other relevant data, making it easy to conduct analyses within `R`.

`KrakenR` interacts with Kraken's API, which is well-documented [here](https://docs.kraken.com/api/). Users can explore this link to better understand how the package communicates with Kraken's endpoints.

One of the key features of `KrakenR` is the inclusion of a `Shiny App` that can be invoked directly from the package. This interactive application allows users to visualize cryptocurrency data and trends using an intuitive user interface.

### Key Features:

 * Fetch market data such as asset prices, volume, and historical trends from Kraken.
 * Real-time cryptocurrency price tracking.
 * Data wrangling functions to process and clean raw Kraken data.
 * `Shiny app` integration to provide visual analytics for cryptocurrency data.

## Installation
 
There are two ways to install the `KrakenR` package:
 
### Option 1: Direct Installation from GitHub (Recommended)
 
The package can be installed directly from this repository using `devtools`. This version ensures all project requirements are met, including the ability to launch the Shiny app from the package.
 
```R
devtools::install_github(repo = "ptds2024/group6-project", subdir = "package")

```

### Option 2: Installation from CRAN (Not Recommended)
 
Alternatively, the package can be installed from CRAN, but this version does not include the Shiny app integration. To avoid missing features, we recommend using the GitHub installation method.
 
```R
install.packages("KrakenR")

```
> **Note**: The CRAN version is a stable release, but it does not fully align with the requirements of the current project, particularly regarding the integration of the Shiny app.

## Usage

Once you have installed the `KrakenR` package, you can immediately begin exploring its features for interacting with Kraken's cryptocurrency data. The package offers a comprehensive set of functions for fetching market data, such as asset pairs, ticker information, and more. Additionally, you can visualize real-time data using the integrated Shiny app.

### Step 1: Load the Package

To get started, load the `KrakenR` package into your `R` session:

```R
# Load the package
library(KrakenR)

```

### Step 2: Explore the Vignette and Documentation

For a comprehensive overview of how the package works, explore the vignette, which provides detailed examples and explanations for each function. You can also access the full documentation for specific function references.

```R
# Access the KrakenR vignette for a guided walkthrough
vignette("KrakenR")

# Access the package documentation and search for specific functions
??KrakenR

```

### Step 3: ???? (Start using the Package)

Now that you’ve installed and loaded the `KrakenR` package, it's time to start fetching real data from Kraken! The package offers several functions to retrieve asset pairs, ticker information, and more.

```R
# Fetch all available asset pairs from the Kraken exchange
asset_pairs <- getPairs()

# Fetch ticker information for a specific asset pair, e.g., Bitcoin to USD
ticker_info <- getTicker("XBTUSD")

# Fetch (OHLC) data for a specific asset pair, e.g., Cardano to CHF
getOHLC("ADACHF", interval = "1h", since = "2024-01-01 00:00:00")

```

### Step 4: PROFIT!!!

> **Reference**: "Step 3: ??, Step 4: Profit" is a reference to the popular meme from [Know Your Meme](https://knowyourmeme.com/memes/profit).

## Shiny Application

The `Shiny App` included in `KrakenR` is a user-friendly tool that allows users to visualize live market data fetched from the Kraken exchange. With the app, users can track price trends, view trading volumes, and analyze market movements. The app is integrated directly into the package and can be launched with a single function call.

### Features of the Shiny App:

 * Real-time cryptocurrency data updates.
 * User-friendly interface with charts and visualizations.
 * Ability to filter data based on specific assets, dates, and metrics.

### How to Launch the App:

Once the `KrakenR` package is installed, you can launch the app using the following function:

```R
# Load the package
library(KrakenR)

# Launch the shiny app
runKrakenApp()

```
<center>

>**TO BE CONTINUED**

</center>
