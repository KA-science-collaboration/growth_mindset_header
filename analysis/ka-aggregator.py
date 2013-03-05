# -*- coding: utf-8 -*-
from random import sample
import csv
from collections import defaultdict
import os

# Constants
raw = '/Users/benjaminhaley/Downloads/gm_ab_perproblem.csv'
aggregate_file = '/Users/benjaminhaley/Downloads/gm.aggregate.csv'
id_dir = '/Users/benjaminhaley/Downloads/gm_by_id/'
header = ['study', 'group', 'id', 'time', 'h_time', 'exercise', 'type', 'seed', 'attempts', 'mode', 'correct', 'proficiency', 'hints', 'elapsed']


"""
# Organize by id
if not os.path.exists(id_dir):
    os.makedirs(id_dir)
    
in_ = csv.reader(open(raw), delimiter = ',')

old_id = ''
#for row in [ in_.next() for _ in range(10002) ]:
for row in in_:
    # skip malformed rows
    if len(row) < len(header):
        print 'missing fields', row
        continue

    # get id
    id_ = row[2]

    # change files
    if old_id != id_:
        out = open(id_dir+str(id_),'a')

    # save
    csv.writer(out).writerow(row)

    # remember
    old_id = i    # skip malformed rows
    if len(row) < len(header):
        print 'missing fields', row
        continue
d_

"""
# Proficiency by id

def aggregate_by_id(in_):
    # convert to 2d array
    data = [r for r in in_]

    #   true intervention time
    intervention_start = 1359187200

    # remove subtest
    subtest = [r[1] for r in data if r[0] == 'growth-mindset-subtest']
    subtest = r[1] if subtest else ''
    data = [r for r in data if r[0] != 'growth-mindset-subtest']

    # find first intervention
    times = [float(r[3]) for r in data]
    intervention = ['fraction' in r[5] for r in data]
    intervention_times = [t for t, i in zip(times, intervention) if i]
    intervention_times = [time for time in intervention_times if time > intervention_start]
    intervention_time = min(intervention_times) if len(intervention_times) > 0 else 0


    # split data into before and after
    times = [t - intervention_time for t in times]
    before = [r for r, time in zip(data, times) if time < 0]
    after = [r for r, time in zip(data, times) if time >= 0]

    # build aggregate
    aggregate = [
        ('group',        data[0][1]),
        ('subtest',      subtest),
        ('id',           hash(data[0][2])),

        ('start',        min(times)),
        ('end',          max(times)),
        ('intervention_time',intervention_time),

        ('interventions',sum([1 for _ in intervention_times])),

        
        ('p_exercises', len(set([r[5] for r in before]))),
        ('a_exercises', len(set([r[5] for r in after]))),
        
        ('p_attempts',   sum([1 for _ in before])),
        ('a_attempts',   sum([1 for _ in after])),
        
        ('p_correct',    sum([int(r[10]) for r in before])),
        ('a_correct',    sum([int(r[10]) for r in after])),
        
        ('p_proficiency',sum([int(r[11]) for r in before])),
        ('a_proficiency',sum([int(r[11]) for r in after])),

        ('p_hints',      sum([int(r[12]) for r in before])),
        ('a_hints',      sum([int(r[12]) for r in after])),
    ]
    
    return aggregate 



id_files = os.listdir(id_dir)
id_files = [ id_file for id_file in id_files if id_file != ".DS_Store" ]
print len(id_files)

with open(aggregate_file, 'w') as csvfile:
    out = csv.writer(csvfile, delimiter=',')

    for i in range(len(id_files)):
#    for i in range(185007,189475):

        # open csv
        in_ = csv.reader(open(id_dir+id_files[i]), delimiter = ',')

        # aggregate
        aggregate = aggregate_by_id(in_)

        # write headers
        if i == 0:
            out.writerow([k for k, v in aggregate])

        # write row
        out.writerow([v for k, v in aggregate])
        
#"""
#"""
        
def test():
    fh = id_dir+'_gae_bingo_random:--6-OAhxRPDWaAbneZ25xKAom2jDQxuT1q1j3te-'
    result = aggregate_by_id(csv.reader(open(fh), delimiter = ','))
    assert(result == [
        ('group', 'control statement'),
        ('subtest', 'science statement'),
        ('id', 1521938818),
        ('start', -1202168.0),
        ('end', 791410.0),
        ('intervention_time', 1359495370.0),
        ('interventions', 11),
        ('p_expercises', 2),
        ('a_expercises', 3),
        ('p_attempts', 16),
        ('a_attempts', 35),
        ('p_correct', 14),
        ('a_correct', 30),
        ('p_proficiency', 1),
        ('a_proficiency', 2),
        ('p_hints', 3),
        ('a_hints', 2)
    ])

test()

#"""
