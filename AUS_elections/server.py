from mesa.visualization.ModularVisualization import ModularServer
from mesa.visualization.modules import CanvasGrid, ChartModule, TextElement
from mesa.visualization.UserParam import UserSettableParameter

from mesa.visualization.TextVisualization import (
    TextData, TextGrid, TextVisualization
)

#from model import SchellingModel
from model_aus import SchellingModel_vote


class SchellingTextVisualization(TextVisualization):
    '''
    ASCII visualization for schelling model
    '''

    def __init__(self, model):
        '''
        Create new Schelling ASCII visualization.
        '''
        self.model = model

        grid_viz = TextGrid(self.model.grid, self.ascii_agent)
        happy_viz = TextData(self.model, 'happy')
        self.elements = [grid_viz, happy_viz]

    @staticmethod
    def ascii_agent(a):
        '''
        Minority agents are X, Majority are O.
        '''
        if a.type == 0:
            return 'O'
        if a.type == 1:
            return 'X'
        if a.type == 2:
            return 'L'
        if a.type == 3:
            return 'K'


class HappyElement(TextElement):
    '''
    Display a text count of how many happy agents there are.
    '''
    def __init__(self):
        pass

    def render(self, model):
        return "Happy agents: " + str(model.happy)

def schelling_draw(agent):
    '''
    Portrayal Method for canvas
    '''
    if agent is None:
        return
    portrayal = {"Shape": "circle", "r": 0.5, "Filled": "true", "Layer": 0}

    if agent.type == 1:
        portrayal["Color"] = "Red"

    if agent.type == 2:
        portrayal["Color"] = "Pink"

    if agent.type ==3:
        portrayal["Color"] = "Lightblue"

    if agent.type == 0:
        portrayal["Color"] = "Blue"

    return portrayal

happy_element = HappyElement()
canvas_element = CanvasGrid(schelling_draw, 20, 20, 500, 500)
happy_chart = ChartModule([{"Label": "happy", "Color": "Black"}])

model_params = {
    "height": 20,
    "width": 20,
    "density": UserSettableParameter("slider", "Agent density", 0.7, 0.1, 1.0, 0.1),
    "party_1": UserSettableParameter("slider", "Fraction party 2", 0.3, 0.00, 0.5, 0.05),
    "party_2": UserSettableParameter("slider", "Fraction minority party 3", 0.1, 0.00, 0.2, 0.05),
    "party_3": UserSettableParameter("slider", "Fraction minority party 4", 0.1, 0.00, 0.2, 0.05),
    "homophily": UserSettableParameter("slider", "Homophily", 3, 0, 8, 1)
}

server = ModularServer(SchellingModel_vote,
                       [canvas_element, happy_element, happy_chart],
                       "Schelling", model_params)
server.launch()