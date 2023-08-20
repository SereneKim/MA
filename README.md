# Educational Assortative Mating and Educational Mobility: Agent-based modeling and simulation to explore their relationship

Welcome to the GitHub repository for my Master's thesis project titled "Educational Assortative Mating and Educational Mobility:
Agent-based modeling and simulation to explore their relationship." This repository serves as a central hub for all the code, data, and documentation related to the research conducted during the academic year 2023/2024 at Katholieke Universiteit Leuven (KU Leuven). The primary objective of this thesis is to explore the relationship between educational assortative mating and educational mobility using agent-based modeling (with the `mesa` package).

## Main folders
The repository mainly consists of 6 folders, which include the following elements:
1. Data: both existing data sets utilized to construct the simulation and the data generated from the simulation.
2. Extra: all the R-scripts or Excel files that are used to additionally analyze the simulation data.
3. Images: all the exported images.
4. Notebooks: all the Jupyter notebooks for agent-based models and analysis.
5. Scripts: all the Python scripts utilized in the analysis.
6. Appendix: additional sources that are not the core of the thesis.

## How to run the simulation
First, make sure to install the requirements:
```
pip install -r requirements.txt
```

Then, run the following:
```
mesa runserver
```
Note that this simulation is based on the `base scenario`, where agents mate based on their scores, and no special preference for homogamy or heterogamy exists. 

## Summary of the simulation design
The general flow of the simulation is as follows:
![General flow](https://github.com/SereneKim/MasterThesis_Seorin_Kim/blob/main/images/flowchart_general.png)

The agent's behavioral rules for meeting a spouse in the `base scenario` are as follows:
![Spouse-seeking flow](https://github.com/SereneKim/MasterThesis_Seorin_Kim/blob/main/images/flowchart_spouse.png)

For detailed information about how agent-based models are designed and what I learned from them,  I invite you to explore my thesis.

## Summary of the Thesis
<details close>
  <summary>Click to read the summary of the thesis </summary>
</br>
When addressing the issue of social mobility in society, an interesting angle to consider is marriage. As it is often believed that like marries like, a mating strategy can ensure a safe transmission of one's relevant resources that can guarantee an equal or even higher level of social position for children. This thesis focuses on educational mobility and how educational assortative mating influences intergenerational educational mobility in the long term using a simulation technique called agent-based modeling. Specifically, it delves into whether the prevalence of similarly educated people marrying in society makes it more difficult for individuals to climb the social ladder than the prevalence of couples with different educational backgrounds.
</br> </br>
Agent-based models are created to represent a society where educational opportunities are well distributed among different social strata. The inquiry showed that societies that prefer heterogamous marriages generally demonstrate higher growth in educational levels at the population level compared to societies that prefer homogamous marriages. Regarding absolute mobility in education, heterogamy did not necessarily lead to the highest mobility level; that distinction went to homogamy. However, in the case of homogamy, the high educational mobility appeared to be a result of a few higher social strata dominating higher education, while in heterogamy, people from different backgrounds could benefit from the societal growth of the educational level.
</br> </br>
The thesis leveraged one of the advantages of agent-based modeling: the ability to explore complex realities using a relatively simple model. However, it is important to acknowledge that this simplicity may limit the generalizability or extension of the conclusions to broader contexts. While the findings may be specific to the context and parameters used in the study, they lay the groundwork for future investigations and extensions of the model to address a wide range of research questions in mobility and inequality studies. 
</details>


## Further Reading
To construct the agent-based models, the official Github of the `mesa` package immensely helped me: 
https://github.com/projectmesa/mesa-examples.git

Besides, the main inspiration for this simulation design is the research by [Grow & Van Bavel (2015)](https://doi.org/10.1371/journal.pone.0127806): 
Grow A, Van Bavel J (2015) Assortative Mating and the Reversal of Gender Inequality in Education in Europe: An Agent-Based Model. PLOS ONE 10(6): e0127806. https://doi.org/10.1371/journal.pone.0127806 

