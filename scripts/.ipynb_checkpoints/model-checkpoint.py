"""
    Scenario Base:
            - Note: Less deterministic than the previous versions.
"""

import math
from enum import Enum
import numpy as np
import random
import networkx as nx
import mesa

class State(Enum):
    single = 0
    married = 1
    reproduced = 2

def number_tertiary(model):
    return sum(1 for a in model.grid.get_all_cell_contents() if a.education == 4 or a.education == 5) 

def average_edu(model):
    N = model.num_nodes
    E = sum(1 for a in model.grid.get_all_cell_contents() if a.education == 1)+ sum(1 for a in model.grid.get_all_cell_contents() if a.education == 2)*2 + sum(1 for a in model.grid.get_all_cell_contents() if a.education == 3)*3 + sum(1 for a in model.grid.get_all_cell_contents() if a.education == 4)*4 +sum(1 for a in model.grid.get_all_cell_contents() if a.education == 5)*5
    try:
        return E/N
    except ZeroDivisionError:
        return math.inf

def homogamy(model):
    return sum(1 for a in model.grid.get_all_cell_contents() if a.spouse is not None and a.education == a.spouse.education)

def heterogamy(model):
    return sum(1 for a in model.grid.get_all_cell_contents() if a.spouse is not None and a.education != a.spouse.education)

    
def number_state(model, state):
    return sum(1 for a in model.grid.get_all_cell_contents() if a.state is state)

def number_single(model):
    return number_state(model, State.single)


def number_married(model):
    return number_state(model, State.married)


def number_reproduced(model):
    return number_state(model, State.reproduced)

    

class MarriageModel(mesa.Model):
    def __init__(self, N = 50, avg_node_degree=10, tuition=50, unfairness=0.2):

        self.num_nodes = N
        prob = avg_node_degree / self.num_nodes
        self.G = nx.erdos_renyi_graph(n=self.num_nodes, p=prob)
        self.grid = mesa.space.NetworkGrid(self.G)
        self.schedule = mesa.time.RandomActivation(self)
        self.unfairness=unfairness
        
            
        for i, node in enumerate(self.G.nodes()): 
            agent = MarriageAgent(i, self, State.single)
            self.schedule.add(agent)
            self.grid.place_agent(agent, node)
            
        self.last_child_id = self.num_nodes-1
        self.num_deaths = 0
        self.tuition = tuition
        
        self.datacollector = mesa.DataCollector(
                model_reporters={
                    "Total_Num": lambda m: m.num_nodes,
                    "Single": number_single,
                    "Married": number_married,
                    "Reproduced": number_reproduced,
                    "Average_Edu": average_edu,
                    "Tertiary": number_tertiary,
                    "Homogamy": homogamy,
                    "Heterogamy": heterogamy
                },
                agent_reporters={
                    "Age" : lambda a: a.age,
                    "Education": lambda a: a.education,
                    "Gender": lambda a: a.gender,
                    "Income": lambda a: round(a.income, 3),
                    "Spouse": lambda a: a.spouse.unique_id if a.spouse else None,
                    "Parents": lambda a: a.parents_id,
                    "Generation": lambda a: a.generation,
                    "Cohort": lambda a: a.cohort,
                    "Children": lambda a: a.children,
                    "Weight": lambda a: [round(a.weights[0], 3), round(a.weights[1], 3), round(a.weights[2], 3)],
                    "Capital": lambda a: [round(a.cultural, 3), round(a.economic, 3), round(a.social, 3)],
                    "Brut": lambda a: a.social_brut
                })
        
        self.running = True
        self.datacollector.collect(self)
        
    def singlehood_perc(self):
        try:
            return number_state(self, State.single) / self.num_nodes
        except ZeroDivisionError:
            return math.inf
        
    def step(self):
        # Collect data
        self.datacollector.collect(self)
        # Advance the model by one step
        self.schedule.step()
    
    def run_model(self, n):
        for i in range(n):
            self.step()
        

class MarriageAgent(mesa.Agent):
    
    """ <Initial Values> Agents with an educational level, age, gender, and income.
        * State: single or married without children or married with children. The initial state is single.
        * Spouse: spouse agent. Initial status is None.
        * Parents_id: agent's parents. The initial agents at step 0 dont' have parents.
        * Age: Based on the triangular distribution defined in get_age(self).
        * Children: agent's children (max.2). The initial agents at step 0 have no children.
        * Cohort: Enumerates cohort by get_cohort(self). The lower the cohort number, the older one is. 
        * Gender: 50:50 distribution.
        * Generation: Initial agents are the first generation of the family. (arbitrary)
        * Education: Final educational level of agents, defined by get_education(self). The distribution differs by cohorts.
        * Income: Income prospect of agents, defined by get_income(self). The distribution is influenced by education. 
        
        Note: Since agents' educational level and income do not change over steps/time, 
        these variables represent education- and income "prospects". 
    """

    def __init__(self, unique_id, model, initial_state = State.single):
        super().__init__(unique_id, model)
        '''
            Consider initial agents as the roots of each family genealogy.
        '''
        self.state = initial_state
        self.spouse = None
        self.parents_id = []
        self.cohort = np.random.choice([1,2,3], p=[0.47, 0.43, 0.1])
        self.age = MarriageAgent.get_age(self)
        self.children = []
        self.gender = np.random.choice(["M", "F"], p=[0.5, 0.5]) 
        self.generation = 1
        self.education = MarriageAgent.get_education(self)
        self.income = MarriageAgent.get_income(self)
        self.weights = [random.randint(1, 10), random.randint(1, 10), random.randint(1, 10)] # for [Cultural, Economic, Social]
        self.cultural = (self.education + (self.weights[0]/10*self.education))/10 # 5*2 as denominator
        self.economic = (self.income + self.weights[1]/10*self.income)/1600
        self.social = 0
        self.capital = self.cultural + self.economic + self.social
        self.social_brut = None
        
        
   
    # Age distribution of the intial agents
    def get_age(self):
        if self.cohort == 1:
            random_age = random.randint(40, 59)
            return random_age
        elif self.cohort == 2:
            random_age= random.randint(20, 39)
            return random_age
        else:
            random_age= random.randint(15, 19)
            return random_age
        
    
    def get_education(self):
        isced = list(range(1,6))  
        
        """ Note: Took ESS9 as a reference
            0 = Less than primary education; (Won't add it - following ESS) 
            1 = Primary 
            2 = Lower secondary 
            3 = Upper secondary (or post-secondary non-tertiary education)
            4 = Lower Tertiary (Bachelor + Professional)
            5 = Upper Tertiary (Master +)
        """
                
        if self.gender == 'M' :
            if self.cohort == 1:
                return np.random.choice(isced, p = [0.20, 0.20, 0.28, 0.12, 0.20]) 
            elif self.cohort == 2: 
                return np.random.choice(isced, p = [0.10, 0.17, 0.29, 0.27, 0.17])
            elif self.cohort ==3:
                return np.random.choice(isced, p = [0.04, 0.09, 0.29, 0.33, 0.25])
        else :
            if self.cohort == 1:
                return np.random.choice(isced, p = [0.30, 0.27, 0.25, 0.15, 0.03]) 
            elif self.cohort == 2: 
                return np.random.choice(isced, p = [0.10, 0.16, 0.27, 0.34, 0.13])
            elif self.cohort ==3:
                return np.random.choice(isced, p = [0.01, 0.09, 0.15, 0.50, 0.25])
    
    def get_income(self):
        if self.education == 5:
            return np.random.triangular(left=200, mode=600, right=800)
        if self.education == 4:
            return np.random.triangular(left=100, mode=400, right=800)
        if self.education == 3:
            return np.random.triangular(left=100, mode=300, right=800)
        if self.education == 2:
            return np.random.triangular(left=50, mode=200, right=800)
        if self.education == 1:
            return np.random.triangular(left=0, mode=100, right=800)
    
    def get_social_brut(self):
        adjacent_nodes = self.model.grid.get_neighbors(self.pos, include_center=False)
        social_capital = [a for a in self.model.grid.get_cell_list_contents(adjacent_nodes)
                         if (a.cultural >= self.cultural) or (a.economic >= self.economic)]
        return len(social_capital) 
        
            
    """ < Behaviors>
        * get_femalespouse(self): Male agents' mating behavior to meet a female agent.
        * get_malespouse(self): Female agents' mating behavior to meet a male agent.
        * reproduce(self): Female agents 20< self.age <45 years old reproduce.
    """
    
    def get_spouse(self):
        adjacent_nodes = self.model.grid.get_neighbors(self.pos, include_center=False)
        potential_partners = [
                agent
                for agent in self.model.grid.get_cell_list_contents(adjacent_nodes)
                if agent not in self.children and agent.spouse is None 
                and agent.gender != self.gender 
                and abs(self.age-agent.age) <= 20
                and ((not agent.parents_id and not self.parents_id) or (set(self.parents_id) != set(agent.parents_id)))
                ] 

        for p in potential_partners:
            # No weights considered
            p.score = (1-abs(self.cultural-p.cultural))**(self.weights[0]/10)*(1-abs(self.economic-p.economic))**(self.weights[1]/10)*(1-abs(self.social-p.social))**(self.weights[2]/10)
            if self.gender == "M":
                if 23 <= p.age <= 27:
                    p.score += 0.3
                if self.education == p.education:
                    p.score += 0.5
            else:
                if 0<= p.age-self.age <=10:
                    p.score += 0.3
                if self.education == p.education:
                    p.score += 0.5
                
        # Choose a spouse from potential partners
        if len(potential_partners) > 0:
            # Sort potential partners based on their score in descending order
            potential_partners.sort(key=lambda x: x.score, reverse=True)
            # Choose the partner with the highest score
            spouse = potential_partners[0]
            self.spouse = spouse
            spouse.spouse = self
            self.state = State.married
            spouse.state = State.married
        else:
            self.spouse = None #single
            self.state = State.single
            
        
        
    def reproduce(self):
        # Place the new agent in an empty cell
        empty_cells = [cell for cell in self.model.grid.get_neighbors(self.pos, include_center=False) if self.model.grid.is_cell_empty(cell)]
        if len(empty_cells) > 0:
            self.state = State.reproduced
            self.spouse.state = State.reproduced
            self.model.num_nodes += 1
            
            # if not self.children:
            child = MarriageAgent(self.model.last_child_id + 1, self.model, State.single)
            self.model.last_child_id += 1
            new_pos = random.choice(empty_cells)
            self.model.grid.place_agent(child, new_pos)
            self.model.schedule.add(child)
            self.children.append(child.unique_id)
            self.spouse.children.append(child.unique_id)

            child.parents_id = [self.unique_id, self.spouse.unique_id]
            child.generation = max(self.generation, self.spouse.generation)+1
            child.cohort = max(self.cohort, self.spouse.cohort)+1
            child.age = 0
            child.state = State.single
            child.weights = [random.randint(1, 10), random.randint(1, 10), random.randint(1, 10)]
            child.social =  MarriageAgent.get_social_brut(child) / self.model.num_nodes
            child.social_brut =  MarriageAgent.get_social_brut(child)
            
            #  # Update Education of Children
            # all_agents = [agent for agent in self.model.grid.get_all_cell_contents() 
            #               if agent.step == child.step and agent.unique_id != child.unique_id 
            #               and agent.unique_id not in child.parents_id
            #              ]
            # cohorts = [agent for agent in all_agents if (agent.cohort == child.cohort)]
            # # You can also change to determine this influence by only a certain gendered parent's.
            # influence = round(max(self.cultural, self.spouse.cultural)*max(self.weights[0], self.spouse.weights[0])/10*5)
            # if len(cohorts) > 0: 
            #     cohorts_list = MarriageAgent.get_cohort_avg(cohorts)
            #     cohorts_edu = cohorts_list[-1]
            #     minimum = min(self.education, self.spouse.education, cohorts_edu, influence)
            #     maximum = max(self.education, self.spouse.education, cohorts_edu, influence)
            #     if max(minimum, 1) == min(maximum, 5):
            #         child.education = max(minimum, 1)
            #     else:
            #         child.education = random.randint(max(minimum, 1), 5) #output-wise this makes sense (cohort-averages between 3.5-4)
            # else:
            #     minimum = min(self.education, self.spouse.education, influence)
            #     maximum = max(self.education, self.spouse.education, influence)
            #     if max(minimum, 1) == min(maximum, 5):
            #         child.education = max(minimum, 1)
            #     else:
            #         child.education = random.randint(max(minimum, 1), 5)
            influence = round(max(self.cultural, self.spouse.cultural)*5) # Before added *max(self.weights[0], self.spouse.weights[0])/10
            # random.chioce instead of max(self.cultural, self.spouse.cultura)?
            if self.capital + self.spouse.capital >= 6*self.model.unfairness: # top 80% Depending on this, the evolution changes a lot (A higher the multiplier --> more decreasing evolution). 
                if influence >= 5:
                    child.education = 5
                else:
                    child.education = random.randint(min(influence, 5), 5)
            else:
                if influence <= 1:
                    child.education = 1
                else:
                    child.education = random.randint(1, min(influence, 5)) # Too restrictive? Try: min(influence+1, 5)
                    
            child.cultural = (child.education + (child.weights[0]/10*child.education))/10
            
            # Income chance depending on the education and parents' income prospects
            influence_income = max(self.income, self.spouse.income)*max(self.weights[1], self.spouse.weights[1])/10
            if child.education - max(self.education, self.spouse.education) >= 0 or self.weights[1] + self.spouse.weights[1] >= 12:
                child.income = random.uniform(influence_income, 800)
            else:
                child.income = random.uniform(0, influence_income)
#                 if self.random.random() <= 0.8:
#                     child.income = random.uniform(min(self.income, self.spouse.income), min(max(self.income, self.spouse.income)+150, 800))
#                 else:
#                     child.income = random.uniform(max(min(self.income, self.spouse.income)-150, 0), max(self.income, self.spouse.income))
#             else:
#                 if self.random.random() <= 0.2:
#                     child.income = random.uniform(min(self.income, self.spouse.income), min(max(self.income, self.spouse.income)+150, 800))
#                 else:
#                     child.income = random.uniform(max(min(self.income, self.spouse.income)-150, 0), max(self.income, self.spouse.income))
                            
            child.economic = (child.income + child.weights[1]/10*child.income)/1600
                
    
    def get_cohort_avg(cohorts):
        value = []
        tally = 0
        for c in cohorts:
            tally += c.education
            value.append(tally/len(cohorts))
            return value
            
    def get_cohort_list(self):
        cohorts = [agent for agent in self.model.grid.get_all_cell_contents()] #if self.step == agent.step 
        value = []
        for c in cohorts:
            cohort_num = c.cohort
            value.append(cohort_num)
        return value
            
    
    def step(self):
        if self.social == 0 :
            self.social = MarriageAgent.get_social_brut(self) / self.model.num_nodes
            self.social_brut = MarriageAgent.get_social_brut(self) 
        
        # Increment age (2 years per step)
        self.age += 2 
        
        if self.age >= 65 or self.cohort == min(self.get_cohort_list()):
            if self.random.random() < 0.4: 
                self.model.grid.remove_agent(self)
                self.model.schedule.remove(self)
                self.model.num_nodes -= 1 #then at the end we don't get 0. 
                self.model.num_deaths += 1       
        
        else:
            #First, seek a partner if self.spouse == None.
            if self.spouse is None:
                    self.get_spouse()
               

            # Second, if a partner exists, reproduce (randomly).    
            else:
                if len(self.children) <= 1 and self.gender == 'F' and 20<= self.age <= 45 and self.income+self.spouse.income > self.model.tuition*3:
                    self.reproduce()
                else:
                    return None # No children

        