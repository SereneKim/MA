import math
import mesa
from .model import *

def network_portrayal(G):
    
    def node_color(agent):
        if agent.state == State.married:
            return "#fa4ba3"
        elif agent.state == State.reproduced:
            return "#f7be02"
        else:
            return "#808080"
        # Single = Yellow, Married = Pink (married but no children)
        # {State.married: "#fa4ba3", State.reproduced: "#2554fa"}.get(
        #     agent.state, "#f7be02")

    def edge_color(agent1, agent2):
        # Pink edge if couple
        # if State.married in (agent1.state, agent2.state):
        #     return "#fa4ba3"
        # Blue edge if reproduced (married + children)
        if agent1 and agent2:
            if agent2.unique_id in agent1.children and agent1.state == State.reproduced:
                return "#f7be02"
        else:
            return "#e8e8e8"

    def edge_width(agent1, agent2):
        if agent1 and agent2:
            if State.reproduced or State.married in (agent1.state, agent2.state):
                return 3
        else:
            return 2
    

    def get_agents(source, target):
        if G.nodes[source]["agent"] and G.nodes[target]["agent"]:
            return G.nodes[source]["agent"][0], G.nodes[target]["agent"][0]
        else:
            return None, None

    portrayal = dict()
    portrayal["nodes"] = [
        {
            "size": 6,
            "color": node_color(agents[0]) if agents else None,
            "tooltip": f"id: {agents[0].unique_id}<br>state: {agents[0].state.name}" if agents else None,
        }
        for (_, agents) in G.nodes.data("agent")
    ]

    portrayal["edges"] = [
        {
            "source": source,
            "target": target,
            "color": edge_color(*get_agents(source, target)) if source and target else None,
            "width": edge_width(*get_agents(source, target)) if source and target else None,
        }
        for (source, target) in G.edges
    ]

    return portrayal


network = mesa.visualization.NetworkModule(network_portrayal, 500, 500)

chart = mesa.visualization.ChartModule(
    [
        {"Label": "Single", "Color": "#808080"},
        {"Label": "Married", "Color": "#fa4ba3"},
        {"Label": "Reproduced", "Color": "#f7be02"}
    ]
)

chart2 = mesa.visualization.ChartModule(
    [{"Label": "Average_Edu", "Color": "#fa4ba3"},
     # {"Label": "Tertiary", "Color": "#0000FF"}
    ], data_collector_name="datacollector"
)


def get_singlehood(model):
    perc = model.singlehood_perc()
    perc_text = "&infin;" if perc is math.inf else f"{perc:.2f}"
    singlehood_text = str(number_single(model))
    total_text = str(model.num_nodes)
    return "Singlehood Percentage: {}<br>Number of singles in the system: {}<br>Total number of agents in the system: {}".format(
        perc_text, singlehood_text, total_text
    )


model_params = {
    "title" : mesa.visualization.StaticText("<b>Parameters: </b>"),
    "N": mesa.visualization.Slider(
        "Number of agents",
        500, # default value
        50, # min
        1000, # max
        50, # interval
        description="Choose how many agents to include in the model",
    ),
    "avg_node_degree": mesa.visualization.Slider(
        "Average node degree",
        500, # default value
        10, # min
        500, # max
        10, # interval
        description="Choose how many edges to include per agent (on average)",
    ),
     "tuition": mesa.visualization.Slider(
        "Tuition fee in society",
        50, # default value
        50, # min
        300, # max
        10, # interval
        description="Choose the tuition fee",
    ),
     "unfairness": mesa.visualization.Slider(
        "educational unfairness or inaccessibility to higher education in society",
        0.2, # default value
        0, # min
        1, # max
        0.1, # interval
        description="Choose the educational inaccessibility in society",
    ),
     "Graph_description": mesa.visualization.StaticText("<b>Graph Description: </b> <br> <ul> <li> Light gray node: single agents </li> <li> Pink node: married agents without children (Married) </li> <li> Yellow node: married agents with children (Reproduced) </li> <li> Black node: empty due to agents' death </li> <li> Singlehood Percentage: the number of singles divided by the total number of population in the system </li>  <li>The same color scheme is used in the chart. </li> </ul> <b>Note:</b> The nodes do not necessarily represent one person/agent. After an agent dies and are removed from a node, this node can be filled by a new agent reproduced by a couple. <br> </br> <b>Author:</b> Seorin Kim, Master in Statistics and Data Science (seorin.kim@student.kuleuven.be)")
}

MarriageModel.description = "As part of thesis, this app demonstrates the demographic aspect of the constructed model. The thesis delves into the topic of educational mobility where the main parameters of interest cover educational assortative mating and educational opportunities. The colored edges represent a couple's reproduction and the chart shows the number of singles, couples without children, and couples with children."
server = mesa.visualization.ModularServer(
    MarriageModel,
    [network, get_singlehood, chart, chart2],
    name = "Educational Mobility and Educational Assortative Mating",
    model_params= model_params,
)
server.port = 8521 # The default
