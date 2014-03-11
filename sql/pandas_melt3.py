import pandas as pd

offenderFrame = pd.read_csv('C:\\temp\\ipython\\in100.csv')
offenderFrame =offenderFrame.drop(['identityField'],1)
offenderFrame =offenderFrame.drop(['OSBuildNumber'],1)

table = pd.pivot_table(offenderFrame, 'TopOffenderActiveTimePercentage', rows=['uSessionId', 'DripsPercentage', 'EnergyChangeRate', 'Duration'], cols='OffenderName')
table.to_csv('C:\\temp\\ipython\\d0_100.csv', sep=",", float_format='%2.0f')

zeroDrips = offenderFrame[offenderFrame.DripsPercentage == 0]
medDrips = offenderFrame[(offenderFrame.DripsPercentage > 0) & (offenderFrame.DripsPercentage < 95) ]
highDrips = offenderFrame[offenderFrame.DripsPercentage > 94]

table = pd.pivot_table(zeroDrips, 'TopOffenderActiveTimePercentage', rows=['uSessionId', 'DripsPercentage', 'EnergyChangeRate', 'Duration'], cols='OffenderName')
table.to_csv('C:\\temp\\ipython\\d0.csv', sep=",", float_format='%2.0f')

table = pd.pivot_table(medDrips, 'TopOffenderActiveTimePercentage', rows=['uSessionId', 'DripsPercentage', 'EnergyChangeRate', 'Duration'], cols='OffenderName')
table.to_csv('C:\\temp\\ipython\\d1_94.csv', sep=",", float_format='%2.0f')

table = pd.pivot_table(highDrips, 'TopOffenderActiveTimePercentage', rows=['uSessionId', 'DripsPercentage', 'EnergyChangeRate', 'Duration'], cols='OffenderName')
table.to_csv('C:\\temp\\ipython\\d95.csv', sep=",", float_format='%2.0f')
