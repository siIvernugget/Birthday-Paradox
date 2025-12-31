# The Birthday Paradox: An Interactive Shiny Application

This repository contains an interactive **R Shiny** app explaining and visualizing the **Birthday Paradox**. The app combines the underlining math, a plot, and a Random-Generator to intuitively demonstrate the birthday paradox.

## App Overview

The Birthday Paradox asks: *How many people need to be gathered in a room for the chance that at least two of them share the same birthday exceeds 50%?* While intuition might suggest a much higher number, the correct answer is only **23**. This app first explains the math behind this result, displays a graph of the probability distribution and finally a random-geneartor, designed to simulate birthdays for any given number of people and highlight matching birthdays red.


## Features

- **Interactive UI:** Built using the `shiny`package in R.
- **Visualizations:** Plot created using the `ggplot2` package.
- **Real-time Simulation:** Uses R's sampling engine to generate and check for duplicates.

## Requirements

To run this app locally, ensure you have **R** installed along with the following libraries:

```r
install.packages(c("shiny", "tidyverse"))
