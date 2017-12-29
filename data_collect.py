import random
import numpy as np
import time

from US_elections.model_us import SchellingModel_US
from UK_elections.model_uk import SchellingModel_UK
from AUS_elections.model_aus import SchellingModel_AUS

start_time = time.time()
random.seed(1234)

# Running multiple times and storing as a csv file
for i in range(100):
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
    model_us_data['cases'] = np.repeat(5, len(model_us_data))  # Adding threshhold parameter column

    model_uk_data['run'] = np.repeat(ider, len(model_uk_data))  # Adding run identifier
    model_uk_data['cnt'] = np.repeat("UK", len(model_uk_data))  # Adding country identifier
    model_uk_data['cases'] = np.repeat(5, len(model_uk_data))  # Adding threshhold parameter column

    model_aus_data['run'] = np.repeat(ider, len(model_aus_data))  # Adding run identifier
    model_aus_data['cnt'] = np.repeat("AUS", len(model_aus_data))  # Adding country identifier
    model_aus_data['cases'] = np.repeat(5, len(model_aus_data))  # Adding threshhold parameter column

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
print("Total time generating baseline case: %s" % (time.time() - start_time))

start_time = time.time()
random.seed(12344)
for j in range(4, 7):
    for i in range(100):
        # Defining all models
        model_us = SchellingModel_US(33, 33, 0.7, 0.33, 0.16, 0.17, j, 1, 0.5)
        model_uk = SchellingModel_UK(33, 33, 0.7, 0.33, 0.16, 0.17, j, 1, 0.5)
        model_aus = SchellingModel_AUS(33, 33, 0.7, 0.33, 0.16, 0.17, j, 1, 0.5)

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
        model_us_data['cases'] = np.repeat(j, len(model_us_data))  # Adding threshhold parameter column

        model_uk_data['run'] = np.repeat(ider, len(model_uk_data))  # Adding run identifier
        model_uk_data['cnt'] = np.repeat("UK", len(model_uk_data))  # Adding country identifier
        model_uk_data['cases'] = np.repeat(j, len(model_uk_data))  # Adding threshhold parameter column

        model_aus_data['run'] = np.repeat(ider, len(model_aus_data))  # Adding run identifier
        model_aus_data['cnt'] = np.repeat("AUS", len(model_aus_data))  # Adding country identifier
        model_aus_data['cases'] = np.repeat(j, len(model_aus_data))  # Adding threshhold parameter column

        if i == 0 and j == 4:
            model_us_all_th = model_us_data  # If it is first run assign to the dataframe
            model_uk_all_th = model_uk_data
            model_aus_all_th = model_aus_data
        else:
            model_us_all_th = model_us_all_th.append(model_us_data)  # Otherwise row bind
            model_uk_all_th = model_uk_all_th.append(model_uk_data)
            model_aus_all_th = model_aus_all_th.append(model_aus_data)
    print(j)

model_us_all_th.to_csv('data/out_us_th.csv')  # Save the file
model_uk_all_th.to_csv('data/out_uk_th.csv')  # Save the file
model_aus_all_th.to_csv('data/out_aus_th.csv')  # Save the file
print("Total time generating differences in outcomes by varying threshold utility: %s" % (time.time() - start_time))

start_time = time.time()
random.seed(56789)
for j in [0.5, 1, 1.5]:
    for i in range(100):
        # Defining all models
        model_us = SchellingModel_US(33, 33, 0.7, 0.33, 0.16, 0.17, 5, j, 0.5)
        model_uk = SchellingModel_UK(33, 33, 0.7, 0.33, 0.16, 0.17, 5, j, 0.5)
        model_aus = SchellingModel_AUS(33, 33, 0.7, 0.33, 0.16, 0.17, 5, j, 0.5)

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
        model_us_data['cases'] = np.repeat(j, len(model_us_data))  # Adding threshhold parameter column

        model_uk_data['run'] = np.repeat(ider, len(model_uk_data))  # Adding run identifier
        model_uk_data['cnt'] = np.repeat("UK", len(model_uk_data))  # Adding country identifier
        model_uk_data['cases'] = np.repeat(j, len(model_uk_data))  # Adding threshhold parameter column

        model_aus_data['run'] = np.repeat(ider, len(model_aus_data))  # Adding run identifier
        model_aus_data['cnt'] = np.repeat("AUS", len(model_aus_data))  # Adding country identifier
        model_aus_data['cases'] = np.repeat(j, len(model_aus_data))  # Adding threshhold parameter column

        if i == 0 and j == 0.5:
            model_us_all_el = model_us_data  # If it is first run assign to the dataframe
            model_uk_all_el = model_uk_data
            model_aus_all_el = model_aus_data
        else:
            model_us_all_el = model_us_all_el.append(model_us_data)  # Otherwise row bind
            model_uk_all_el = model_uk_all_el.append(model_uk_data)
            model_aus_all_el = model_aus_all_el.append(model_aus_data)
    print(j)

model_us_all_el.to_csv('data/out_us_el.csv')  # Save the file
model_uk_all_el.to_csv('data/out_uk_el.csv')  # Save the file
model_aus_all_el.to_csv('data/out_aus_el.csv')  # Save the file
print("Total time generating differences in outcomes by varying election utility: %s" % (time.time() - start_time))

start_time = time.time()
random.seed(53202)
for j in [0.25, 0.5, 0.75]:
    for i in range(100):
        # Defining all models
        model_us = SchellingModel_US(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, j)
        model_uk = SchellingModel_UK(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, j)
        model_aus = SchellingModel_AUS(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, j)

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
        model_us_data['cases'] = np.repeat(j, len(model_us_data))  # Adding threshhold parameter column

        model_uk_data['run'] = np.repeat(ider, len(model_uk_data))  # Adding run identifier
        model_uk_data['cnt'] = np.repeat("UK", len(model_uk_data))  # Adding country identifier
        model_uk_data['cases'] = np.repeat(j, len(model_uk_data))  # Adding threshhold parameter column

        model_aus_data['run'] = np.repeat(ider, len(model_aus_data))  # Adding run identifier
        model_aus_data['cnt'] = np.repeat("AUS", len(model_aus_data))  # Adding country identifier
        model_aus_data['cases'] = np.repeat(j, len(model_aus_data))  # Adding threshhold parameter column

        if i == 0 and j == 0.25:
            model_us_all_nb = model_us_data  # If it is first run assign to the dataframe
            model_uk_all_nb = model_uk_data
            model_aus_all_nb = model_aus_data
        else:
            model_us_all_nb = model_us_all_nb.append(model_us_data)  # Otherwise row bind
            model_uk_all_nb = model_uk_all_nb.append(model_uk_data)
            model_aus_all_nb = model_aus_all_nb.append(model_aus_data)
    print(j)

model_us_all_nb.to_csv('data/out_us_nb.csv')  # Save the file
model_uk_all_nb.to_csv('data/out_uk_nb.csv')  # Save the file
model_aus_all_nb.to_csv('data/out_aus_nb.csv')  # Save the file
print("Total time generating differences in outcomes by varying neighborhood utility: %s" % (time.time() - start_time))

start_time = time.time()
random.seed(142340)
# Running multiple times and storing as a csv file
for i in range(10):
    # Defining all models
    model_us = SchellingModel_US(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, 0.5)
    model_uk = SchellingModel_UK(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, 0.5)
    model_aus = SchellingModel_AUS(33, 33, 0.7, 0.33, 0.16, 0.17, 5, 1, 0.5)

    while model_us.running and model_us.schedule.steps < 1000:
        model_us.step()
    model_us_data = model_us.datacollector.get_model_vars_dataframe()  # Storing data of a single run

    while model_uk.running and model_uk.schedule.steps < 1000:
        model_uk.step()
    model_uk_data = model_uk.datacollector.get_model_vars_dataframe()  # Storing data of a single run

    while model_aus.running and model_aus.schedule.steps < 1000:
        model_aus.step()
    model_aus_data = model_aus.datacollector.get_model_vars_dataframe()  # Storing data of a single run

    ider = i+1  # Run identifer
    print(ider)

    model_us_data['run'] = np.repeat(ider, len(model_us_data))  # Adding run identifier
    model_us_data['cnt'] = np.repeat("US", len(model_us_data))  # Adding country identifier
    model_us_data['cases'] = np.repeat(5, len(model_us_data))  # Adding threshhold parameter column

    model_uk_data['run'] = np.repeat(ider, len(model_uk_data))  # Adding run identifier
    model_uk_data['cnt'] = np.repeat("UK", len(model_uk_data))  # Adding country identifier
    model_uk_data['cases'] = np.repeat(5, len(model_uk_data))  # Adding threshhold parameter column

    model_aus_data['run'] = np.repeat(ider, len(model_aus_data))  # Adding run identifier
    model_aus_data['cnt'] = np.repeat("AUS", len(model_aus_data))  # Adding country identifier
    model_aus_data['cases'] = np.repeat(5, len(model_aus_data))  # Adding threshhold parameter column

    if i == 0:
        model_us_1k = model_us_data  # If it is first run assign to the dataframe
        model_uk_1k = model_uk_data
        model_aus_1k = model_aus_data
    else:
        model_us_1k = model_us_1k.append(model_us_data)  # Otherwise row bind
        model_uk_1k = model_uk_1k.append(model_uk_data)
        model_aus_1k = model_aus_1k.append(model_aus_data)

model_us_1k.to_csv('data/out_us_1000.csv')  # Save the file
model_uk_1k.to_csv('data/out_uk_1000.csv')  # Save the file
model_aus_1k.to_csv('data/out_aus_1000.csv')  # Save the file
print("Total time generating 10 runs with 1000 steps: %s" % (time.time() - start_time))
