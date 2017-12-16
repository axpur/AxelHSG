from US_elections.model_us import SchellingModel_US
from UK_elections.model_uk import SchellingModel_UK
from AUS_elections.model_aus import SchellingModel_AUS
import random
import pandas as pd
import numpy as np

random.seed(1234)

# Running multiple times and storing as a csv file
for i in range(20):
    # Defining all models
    model_us = SchellingModel_US(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, 0.5)
    model_uk = SchellingModel_UK(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, 0.5)
    model_aus = SchellingModel_AUS(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, 0.5)

    while model_us.running and model_us.schedule.steps < 100:
        model_us.step()
    model_us_data = model_us.datacollector.get_model_vars_dataframe()  # Storing data of a single run

    while model_uk.running and model_uk.schedule.steps < 100:
        model_uk.step()
    model_uk_data = model_uk.datacollector.get_model_vars_dataframe()  # Storing data of a single run

    while model_aus.running and model_aus.schedule.steps < 100:
        model_aus.step()
    model_aus_data = model_aus.datacollector.get_model_vars_dataframe()  # Storing data of a single run

    ider = i+1  # Run identifer
    print(ider)

    model_us_data['run'] = np.repeat(ider, len(model_us_data))  # Adding run identifier
    model_us_data['cnt'] = np.repeat("US", len(model_us_data))  # Adding country identifier

    model_uk_data['run'] = np.repeat(ider, len(model_uk_data))  # Adding run identifier
    model_uk_data['cnt'] = np.repeat("UK", len(model_uk_data))  # Adding country identifier

    model_aus_data['run'] = np.repeat(ider, len(model_aus_data))  # Adding run identifier
    model_aus_data['cnt'] = np.repeat("AUS", len(model_aus_data))  # Adding country identifier

    if i == 0:
        model_us_all = model_us_data  # If it is first run assign to the dataframe
        model_uk_all = model_uk_data
        model_aus_all = model_aus_data
    else:
        model_us_all = model_us_all.append(model_us_data)  # Otherwise row bind
        model_uk_all = model_uk_all.append(model_uk_data)
        model_aus_all = model_aus_all.append(model_aus_data)

model_us_all.to_csv('data/out_us.csv')  # Save the file
model_uk_all.to_csv('data/out_uk.csv')  # Save the file
model_aus_all.to_csv('data/out_aus.csv')  # Save the file
