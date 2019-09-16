# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
    
import os
cwd = os.getcwd()
os.chdir("C:/Users/denni/Documents")
cwd



with open('dummy.txt') as file:
	file_contents = file.read()
	print(file_contents)
    
    
import re
import pandas as pd 

rx_dict = {
    'From': re.compile(r'From: (?P<from>.*)\n'),
    
}

rx_dict







f = open("fradulent_emails.txt", "r")
searchlines = f.readlines()
f.close()

state = ""
# initialize bodies with one empty string
bodies= [""]
From= [""]
Date= [""]
Subj= [""]
for line in searchlines:
    if "From r" in line:
        state="dont print"
        # add a new empty string to the list if the last string is non-empty
        if bodies[-1] != "": bodies.append("")
        if From[-1] != "": From.append("")
        if Date[-1] != "": Date.append("")
        if Subj[-1] != "": Subj.append("")
    if line.startswith('From:'):
        From[-1] += line
    if line.startswith('Date:'):
        Date[-1] += line
    if line.startswith('Subject:'):
        Subj[-1] += line
    if state=="print":
        # Concatenate all the lines into one string in the list
        bodies[-1] += line
    if "Status:" in line:
        state="print"

len(bodies)

len(From)

len(Date)

len(Subj)

From=[From[:-1] if From.endswith('\n') else From for From in From]
Subj=[Subj[:-1] if Subj.endswith('\n') else Subj for Subj in Subj]
Date=[Date[:-1] if Date.endswith('\n') else Date for Date in Date]
#bodies=[bodies[:-2] if bodies.endswith('\n') else bodies for bodies in bodies]
#bodies = list(map(str.strip, bodies))

#bodies = list(map(lambda s: s.strip(), bodies))

#stripped_line = [s.rstrip() for s in bodies]

bodies = [i.replace('\n','') for i in bodies]

rows = zip(From,Subj,Date,bodies)

frows = zip(From)

import csv

with open('mail.csv', 'w',newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    #for row in frows:
    #    writer.writerow(row)
    
    
    
    writer.writerow(['From', 'Subject', 'Date', 'Body'])
    for row in rows:
        writer.writerow(row)
        
        
        
        
# Now going to be doing the same thing to the Enron dataset
        


f = open("emails.csv", "r")
searchlines = f.readlines()
f.close()

state = ""
# initialize bodies with one empty string
bodies= [""]
From= [""]
Date= [""]
Subj= [""]
for line in searchlines:
    if line.startswith("\""):
        state="dont print"
        # add a new empty string to the list if the last string is non-empty
        if bodies[-1] != "": bodies.append("")
        if From[-1] != "": From.append("")
        if Date[-1] != "": Date.append("")
        if Subj[-1] != "": Subj.append("")
    if line.startswith('From:'):
        From[-1] += line
    if line.startswith('Date:'):
        Date[-1] += line
    if line.startswith('Subject:'):
        Subj[-1] += line
    if state=="print":
        # Concatenate all the lines into one string in the list
        bodies[-1] += line
    if "X-FileName:" in line:
        state="print"

len(bodies)

len(From)

len(Date)

len(Subj)


From=[From[:-1] if From.endswith('\n') else From for From in From]
Subj=[Subj[:-1] if Subj.endswith('\n') else Subj for Subj in Subj]
Date=[Date[:-1] if Date.endswith('\n') else Date for Date in Date]
#bodies=[bodies[:-2] if bodies.endswith('\n') else bodies for bodies in bodies]
#bodies = list(map(str.strip, bodies))

#bodies = list(map(lambda s: s.strip(), bodies))

#stripped_line = [s.rstrip() for s in bodies]

bodies = [i.replace('\n','') for i in bodies]




rows = zip(From,Subj,Date,bodies)

frows = zip(From)

import csv

with open('cmail.csv', 'w',newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    #for row in frows:
    #    writer.writerow(row)
    
    
    
    writer.writerow(['From', 'Subject', 'Date', 'Body'])
    for row in rows:
        writer.writerow(row)
        















