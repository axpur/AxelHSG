import random

from mesa import Model, Agent
from mesa.time import RandomActivation
from mesa.space import SingleGrid
from mesa.datacollection import DataCollector


class SchellingAgent(Agent):
    '''
    Schelling segregation agent
    '''
    # Q: What is the purpose of _init_ exactly see
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
        self.utility = 0
        self.elections_utility = 0
        self.neighbor_types = []
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
            self.model.elections_party0[self.loc] += 1
        elif self.type == 2:
            self.model.elections_center_0[self.loc] += 1 #left wing of the center party
        elif self.type == 3:
            self.model.elections_center_1[self.loc] += 1 #right wing of the center party
        else:
            self.model.elections_party1[self.loc] += 1

    def step(self):
        self.neighbor_types = []
        similar = 0  # How many agents around me are similar to me. Initially -1. Done for each agent at every step.
        self.election_utility = 0
        for neighbor in self.model.grid.neighbor_iter(self.pos):
            self.neighbor_types.append(neighbor.type)
            if neighbor.type == self.type:
                similar += 1
            if (self.type == 1 and neighbor.type == 2) or \
               (self.type == 2 and neighbor.type == 1) or \
               (self.type == 0 and neighbor.type == 3) or \
               (self.type == 3 and neighbor.type == 0):
                similar += self.model.alpha

        # Check whether your type matches the type that won the election in your location
        if (self.type == 1 and self.model.elections[self.loc] == 1) or \
           (self.type == 2 and self.model.elections[self.loc] == 1) or \
           (self.type == 0 and self.model.elections[self.loc] == 0) or \
           (self.type == 3 and self.model.elections[self.loc] == 0):
            similar = similar + 1
            self.election_utility += 1

        self.utility = similar
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
            self.model.elections_party0[self.loc] += 1
        elif self.type == 2:
            self.model.elections_center_0[self.loc] += 1
        elif self.type == 3:
            self.model.elections_center_1[self.loc] += 1
        else:
            self.model.elections_party1[self.loc] += 1


class SchellingModel_US(Model):
    '''
    Model class for the Schelling segregation model.
    '''

    def __init__(self, height, width, density, type_1, type_2, type_3, homophily, alpha):
        '''
        '''
        # Setting up the Model
        self.height = height
        self.width = width
        self.density = density  # percentage (empty houses)
        self.type_1 = type_1  # percentage minority in the city
        self.type_2 = type_2
        self.type_3 = type_3
        self.homophily = homophily  # number of similar minded person that you want around you
        self.alpha = alpha
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
        self.type2 = 0
        self.type3 = 0

        # Setting up lists to store location specific values
        self.elections_party0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_party1 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_center = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_center_0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_center_1 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
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
            random_number = random.random()
            # Second if statement: take a random number between 0 and 1
            # and assign the agent type based on the condition
            if random.random() < self.density:
                if random_number < self.type_1:
                    agent_type = 1
                    self.type1 += 1
                else:
                    if random_number < (self.type_1+self.type_2):
                        agent_type = 2
                        self.type2 += 1 # E: the variable tracks total number of different types, so should be adding one
                    else:
                        if random_number < (self.type_1+self.type_2+self.type_3):
                            agent_type = 3
                            self.type3 += 1 # E: Same as last
                        else:
                            agent_type = 0
                            self.type0 += 1 # E:Missing plus sign

                # Refer to the above function related to Agent attributes
                agent = SchellingAgent((x, y), self, agent_type)
                self.grid.position_agent(agent, (x, y))
                self.schedule.add(agent)

        # For each location run the elections (range(9) goes from 0 to 8)
        for i in range(9):
            #add center left and center right together
            self.elections_type_total[i] = self.elections_party0[i] + self.elections_party1[i] + self.elections_center_1[i] + self.elections_center_0[i]
            if self.elections_party1[i] + self.elections_center_1[i] >= self.elections_party0[i] + self.elections_center_0[i]:
                self.elections[i] += 1

        # Storing relevant data for calculating segregation measures
        self.datacollector = DataCollector(
            {"happy": lambda m: m.happy,
            "total_0": lambda m: m.type0,
            "total_1": lambda m: m.type1,
            "total_2": lambda m: m.type2,
            "total_3": lambda m: m.type3,
            "location_0": lambda m: m.elections_party0,
            "location_1": lambda m: m.elections_party1,
            "location_2": lambda m: m.elections_center_0,
            "location_3": lambda m: m.elections_center_1,
            "location_total": lambda m: m.elections_type_total,
            "elections": lambda m: m.elections},
            {"x": lambda a: a.pos[0], "y": lambda a: a.pos[1],
            "util": lambda a: a.utility, "loc": lambda a: a.loc,
            "type": lambda a: a.type, "neighbor_types": lambda a: a.neighbor_types,
            "elec_util": lambda a: a.election_utility})

    def step(self):
        '''
        Run one step of the model. If All agents are happy, halt the model.
        '''

        # Reseting location specific lists
        self.elections_party0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_party1 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_center_0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_center_1 = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.elections_type_total = [0, 0, 0, 0, 0, 0, 0, 0, 0]
        self.happy = 0  # Reset counter of happy agents
        self.schedule.step()
        # Reseting location results
        self.elections = [0, 0, 0, 0, 0, 0, 0, 0, 0]

        # Re-running elections after agents have moved
        for i in range(9):
            #add center left and center right together
            self.elections_type_total[i] = self.elections_party0[i] + self.elections_party1[i] + self.elections_center_1[i] + self.elections_center_0[i]
            if self.elections_party1[i] + self.elections_center_1[i] >= self.elections_party0[i] + self.elections_center_0[i]:
                self.elections[i] += 1

        # Storing relevant data
        self.datacollector.collect(self)

        if self.happy == self.schedule.get_agent_count():
            self.running = False
