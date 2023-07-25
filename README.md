# Educational Assortative Mating and Educational Mobility: Agent-based modeling and simulation to explore their relationship

Welcome to the GitHub repository for my Master's thesis project titled "Educational Assortative Mating and Educational Mobility:
Agent-based modeling and simulation to explore their relationship." This repository serves as a central hub for all the code, data, and documentation related to the research conducted during the academic year 2023/2024 at Katholieke Universiteit Leuven (KU Leuven). The primary objective of this thesis is to explore the relationship between educational assortative mating and educational mobility using agent-based modeling (with the `mesa` package).

## Main folders
The repository mainly consists of 5 folders, which include the following elements:
1. Data: both existing data sets utilized to construct the simulation and the data generated from the simulation.
2. Extra: all the R-scripts or Excel files that are used to additionally analyze the simulation data.
3. Images: all the exported images.
4. Notebooks: all the Jupyter notebooks for agent-based models and analysis.
5. Scripts: all the Python scripts utilized in the analysis.

## How to run the simulation
First, make sure to install the requirements:
```
pip install -r requirements.txt
```

Then, run the following:
```
mesa runserver
```
Note that this simulation is based on the base scenario, where agents mate based on their scores and no special preference to homogamy or heterogamy exists. For detailed information about how agent-based models are designed and what I learned from them,  I invite you to explore my thesis.

## Abstract
.......to be added.....


## Further Reading
To construct the agent-based models, the official Github of the `mesa` package immensely helped me: 
https://github.com/projectmesa/mesa-examples.git

Besides, the main inspiration for this simulation design is the research by [Grow & Van Bavel (2015)](https://doi.org/10.1371/journal.pone.0127806): 
Grow A, Van Bavel J (2015) Assortative Mating and the Reversal of Gender Inequality in Education in Europe: An Agent-Based Model. PLOS ONE 10(6): e0127806. https://doi.org/10.1371/journal.pone.0127806 

