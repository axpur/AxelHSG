import random

from mesa import Model, Agent
from mesa.time import RandomActivation
from mesa.space import SingleGrid
from mesa.datacollection import DataCollector


class SchellingAgent(Agent):
    '''
    Schelling segregation agent
    '''
    # Q: What is the purpose of _init_ exactly
    def __init__(self, pos, model, agent_type):
        '''
         Create a new Schelling agent.
         Args:
            unique_id: Unique identifier for the agent.
            x, y: Agent initial location
            agent_type: Indicator for the agent's type (minority=1, majority=0)
        '''
        super().__init__(pos, model)
        self.pos = pos
        self.type = agent_type

        # Determine to which location you belong in terms of X
        if self.pos[0] >= 23:
            self.x = 2
        elif self.pos[0] >= 12:
            self.x = 1
        else:
            self.x = 0

        # Determine to which location you belong in terms of Y
        if self.pos[1] >= 23:
            self.y = 2
        elif self.pos[1] >= 12:
            self.y = 1
        else:
            self.y = 0

        # Determine the location
        self.loc = self.y*3+self.x

        # Tracking the number of different types of agents in location
        if self.type == 0:
            self.model.elections_type0[self.loc] += 1
        else:
            self.model.elections_type1[self.loc] += 1

    def step(self):

        similar = 0  # How many agents around me are similar to me. Initially -1. Done for each agent at every step.
        for neighbor in self.model.grid.neighbor_iter(self.pos):
            if neighbor.type == self.type:
                similar += 1

        # Check whether your type matches the type that won the election in your location
        if self.type == self.model.elections[self.loc]:
            similar = similar + self.model.gamma
        # If unhappy, move:
        if similar < self.model.homophily:
            # Simplifies location adjustment
            self.model.grid.move_to_empty(self)  # If not happy, move to empty cell.
        else:
            # Keep track of happy people
            self.model.happy += 1

        # Determine to which location you belong in terms of X
        if self.pos[0] >= 23:
            self.x = 2
        elif self.pos[0] >= 12:
            self.x = 1
        else:
            self.x = 0

        # Determine to which location you belong in terms of Y
        if self.pos[1] >= 23:
            self.y = 2
        elif self.pos[1] >= 12:
            self.y = 1
        else:
            self.y = 0

        # Determine the location
        self.loc = self.y*3+self.x

        # Tracking the number of different types of agents in location
        if self.type == 0:
            self.model.elections_type0[self.loc] += 1
        else:
            self.model.elections_type1[self.loc] += 1


class SchellingModel_US(Model):
    '''
    Model class for the Schelling segregation model.
    '''

    def __init__(self, height, width, density, minority_pc, homophily, gamma):
        '''
        '''
        # Setting up the Model
        self.height = height
        self.width = width
        self.density = density  # percentage (empty houses)
        self.minority_pc = minority_pc  # percentage minority in the city
        self.homophily = homophily  # number of similar minded person that you want around you
        self.gamma = gamma #weight on the election outcome in utility function
        # Setting up the AGM simulation
        self.schedule = RandomActivation(self)

        # Setting up the grid, using inputs in the function, the torus function
        # seems to be related to how we treat edges, but not sure
        self.grid = SingleGrid(height, width, torus=True)

        # Setting the number of happy people to zero
        self.happy = 0

        self.running = True

        # Setting a variable to store total number of different types of agents
        self.type0 = 0
        self.type1 = 0

        # Setting up lists to store location specific values
        self.elections_type0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_type1 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_type_total = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        # Set up agents
        # We use a grid iterator that returns
        # the coordinates of a cell as well as
        # its contents. (coord_iter)
        for cell in self.grid.coord_iter():
            # For each cell coordinate apply if statements
            x = cell[1]
            y = cell[2]

            # First if statement: take a random number between 0 and 1
            # (random.random command) and check whether that value is
            # below the assigned density.

            # Second if statement: take a random number between 0 and 1
            # and assign the agent type based on the condition
            if random.random() < self.density:
                if random.random() < self.minority_pc:
                    agent_type = 1
                    self.type1 += 1
                else:
                    agent_type = 0
                    self.type0 += 1

                # Refer to the above function related to Agent attributes
                agent = SchellingAgent((x, y), self, agent_type)
                self.grid.position_agent(agent, (x, y))
                self.schedule.add(agent)

        # For each location run the elections (range(9) goes from 0 to 8)
        for i in range(9):
            # Total number of agents in location i
            self.elections_type_total[i] = self.elections_type0[i] + self.elections_type1[i]

            # Election winner criteria
            if self.elections_type1[i] >= self.elections_type0[i]:
                self.elections[i] += 1

        # Storing relevant data for calculating segregation measures
        self.datacollector = DataCollector(
            {"happy": lambda m: m.happy,
            "total_0": lambda m: m.type0,
            "total_1": lambda m: m.type1,
            "location_0": lambda m: m.elections_type0,
            "location_1": lambda m: m.elections_type1,
            "location_total": lambda m: m.elections_type_total,
            "elections": lambda m: m.elections})

    def step(self):
        '''
        Run one step of the model. If All agents are happy, halt the model.
        '''

        # Reseting location specific lists
        self.elections_type0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_type1 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_type_total = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.happy = 0  # Reset counter of happy agents
        self.schedule.step()
        # Reseting location results
        self.elections = [0, 0, 0, 0, 0, 0, 0, 0, 0]

        # Re-running elections after agents have moved
        for i in range(9):
            self.elections_type_total[i] = self.elections_type0[i] + self.elections_type1[i]
            if self.elections_type1[i] >= self.elections_type0[i]:
                self.elections[i] += 1

        # Storing relevant data
        self.datacollector.collect(self)

        if self.happy == self.schedule.get_agent_count():
            self.running = False
