# Do Voting Systems Affect Segregation
## Schelling segregation model under different voting systems
### By: Julia Baumann, Tadas Gedminas and Axel Purwin

This repository contains code that is necessary for replicating our project: "Do Voting Systems Affect Segregation". The modelling part of the project was done using Python, in particular using the Mesa agent based modelling module. Data analysis on simulation results was performed with R. The paper was written in LaTeX.

In order to replicate and run the project the following requirements are necessary:

 * Python version that is 3.+
 * Python modules: `pandas`, `numpy`, `random`, `time`, `mesa`
 * R 3.4.2 with packages `xtable` and `tidyverse` installed
 * LaTeX library manager to install necessary packages for generating the PDF.
 
The project has the following file structure. First note, that for each different election system there is an individual folder: `X_elections`, where `X` is one of `AUS`, `UK` or `US`. In each election folder there are 3 files:

1) `model_X.py` - python code that defines the setup of the model for each election case;
2) `server.py` - python code used for running the simulation iteractively (explained below);
3) `run.py` - python code used for running the sumulation iteractively (explained below);

`data_collect.py` is python code used for generating simulationd data used for analysis. The code runs election simulation a number of times for a specified number of steps. Note, given the inhrenet randomness associated with running of the simulation, each section before running defines a 'seed', which allows to exactly replicate results observed in our project. The output of `data_collect.py` files are .csv files, which are stored in the `Data` folder, to be used in analysis with R. The naming convention of these files is as follows: `out_x.csv`, where `x` is one of `AUS`, `UK` or `US`, stores results of our 'baseline' simulation; `out_x_th.csv`, where `x` is as before and `th` reflects that the output re-runs the original simulation by changing utility threshold; `out_x_el.csv`, where `x` is as before and `el` reflects that the output re-runs the original simulation by changing utility gains from winning elections; `out_x_nb.csv`, where `x` is as before and `nb` reflects that the output re-runs the original simulation by changing neighborhood utility gains; and `out_x_1000.csv`, where `x` is as before, but the baseline simulation is extended to 1000 steps, to include 'long-run' dynamics.

`adj_output.R` is R code used for analysing simulation data. Note an important point before running R code: make sure to set working directory to the folder which contains the project and that the two libraries `xtable` and `tidyverse` are installed. The R code first cleans the data and calculates relevant measures described in our paper. The output of the R code is contained in `Plots` and `Tables` folders. The naming convention is the same as in the data file with few differences. Files that begin with the exception of additional tags. `agg` vs. `grp` indicate whether ratios calculated are aggregated over all groups or group specific (refer to paper for the differences). In the `Plots` folder in addition to indicator measures additional plots are included. `box_plot` shows the uncertainty behind simulation results, as it reports the simulation result distribution after 100 steps, as oppose to reporting the mean values. `election_plot` calculates the fraction of locations in our simulation that have changing election winners. The plot is only done for the final case.

The project also contains tools for running the election simulations interactively. This can be achieved by first setting your working environment to the folder that contains the project and after running command `python x_elections/run.py`, where `x` is one of `AUS`, `UK` or `US`. This will run a local server, which open a browser, where simulation results (changing agent location, change in the share of happy agents) can be observed directly. It is also possible to change simulation parameters, whereas the default values match the 'baseline' specification. 
