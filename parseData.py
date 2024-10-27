import csv

# Load data from the CSV file
affecting_storms = []
with open('gulf_storms.csv', mode='r') as file:
    reader = csv.reader(file)
    next(reader)  # Skip header
    for row in reader:
        affecting_storms.append(row)

# Print results
for storm_info in affecting_storms:
    print(f"Storm {storm_info[0]} ({storm_info[1]}) affected {storm_info[2]}")