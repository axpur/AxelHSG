from model_us import SchellingModel_US
import random

random.seed(1234)
model = SchellingModel_US(33, 33, 0.7, 0.5, 5)

while model.running and model.schedule.steps < 100:
    model.step()

model_out = model.datacollector.get_model_vars_dataframe()
model_out.to_csv('out.csv')
