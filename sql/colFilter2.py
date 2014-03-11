import csv

inputFile = csv.DictReader(open('in100K_offenders.csv'))

for row in inputFile:
    drips = int(row["DripsPercentage"])
    if drips < 1 :
        print row
