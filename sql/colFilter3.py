import csv

dr = csv.DictReader(open('in100K_offenders.csv'))

outfile = 'out.csv'
with open(outfile,'wb') as fou:
    dw = csv.DictWriter(fou, delimiter=',', fieldnames=dr.fieldnames)
    dw.writerow(dict((fn,fn) for fn in dr.fieldnames))
    for row in dr:
        drips = int(row["DripsPercentage"])
        #if (drips < 1):
        #if (drips > 94):
        if (drips > 0) and (drips < 95):
            dw.writerow(row)
