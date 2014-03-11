import csv
with open('in100K_offenders.csv') as f:
    reader = csv.reader(f)
    for row in reader:
        print row
