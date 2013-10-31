"""
Plot correct post vs num_pre.

Use like:
cat gm_process_output_130601.csv | python ~/Dropbox/growth_mindset_header/analysis/gm_post_vs_pre.py 
where the file piped in is the output of gm_process.py, called in student mode
"""

import numpy as np
from collections import defaultdict
import sys
import re
import fileinput

import matplotlib
import matplotlib.pyplot as plt


# split on tab, ',', or \x01 so it works in Hive or via pipes on local machine
linesplit = re.compile('[,\t\x01]')

def main():

  MAX_NUM = 100
  SCL = np.log(2.)
  MAX_USERS_PER_CONDITION = 5e5

  line_count = 0

  post = defaultdict(lambda: np.zeros((MAX_NUM,4,MAX_USERS_PER_CONDITION)))
  useri = defaultdict(lambda: np.zeros((MAX_NUM)).astype(int))

  rows = []
  # accumulate lines, and then call process_user once per user
  for line in fileinput.input():
    line_count += 1

    # skip the header line
    if line_count == 1:
      continue

    # DEBUG
    #if line_count > 1e7:
    #    break

    try:
      rowi = linesplit.split( line )
      row = []
      for el in rowi:
        if el == '':
          el = 0.
        row.append(el)

      #print row[1], row[8], row[9], row[24], row[25], row[32], row[33]

      alt = row[1]
      num_pre = int(row[8])
      correct_pre = float(row[9])*num_pre
      num_post_frac = float(row[24])
      correct_post_frac = float(row[25])*num_post_frac
      num_post_other = float(row[32])
      correct_post_other = float(row[33])*num_post_other
    except:
      print >>sys.stderr, line
      continue

    # print num_pre
    # print np.array([1., num_post_frac, num_post_other, correct_post_frac, correct_post_other]).reshape((1,-1))
    # print post[alt][num_pre,:]

    lnp = int(np.floor(np.log(1. + num_pre)*SCL))

    if lnp >= MAX_NUM:
      print 'MAX_NUM exceeded', lnp, num_pre, SCL

    # print lnp
    # print useri[alt][lnp]
    # print post[alt][lnp,:,useri[alt][lnp]].shape
    # print np.array([num_pre, num_post_frac, num_post_other, correct_post_frac, correct_post_other])

    post[alt][lnp,:,useri[alt][lnp]] = np.array([num_post_frac, num_post_other, correct_post_frac, correct_post_other])
    useri[alt][lnp] += 1
    #  print row

  medians = defaultdict(lambda: np.zeros((MAX_NUM,4)))
  for alt in post.keys():
    for ii in range(MAX_NUM):
      #medians[alt][[ii],:] = np.median((post[alt][[ii],:,:useri[alt][ii]]).astype(float), axis=2)
      #DEBUG
      medians[alt][[ii],:] = np.sum((post[alt][[ii],:,:useri[alt][ii]]>0).astype(float), axis=2)/useri[alt][ii]

  xx = np.exp(np.arange(MAX_NUM)/SCL)

  plt.figure()
  for alt in post.keys():
    plt.loglog( xx, (medians[alt][:,2]+medians[alt][:,3]), '.', label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post')
  plt.legend()
  plt.grid()

  plt.figure()
  alt = 'no header'
  cntrl = (medians[alt][:,2]+medians[alt][:,3])
  for alt in post.keys():
    dat = ((medians[alt][:,2]+medians[alt][:,3])) / cntrl
    plt.semilogx( xx, dat, '.', label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post relative to no header')
  plt.legend()
  plt.grid()

  plt.figure()
  alt = 'no header'
  cntrl = (medians[alt][:,2])
  for alt in post.keys():
    dat = ((medians[alt][:,2])) / cntrl
    plt.semilogx( xx, dat, '.', label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post frac relative to no header')
  plt.legend()
  plt.grid()

  plt.figure()
  alt = 'no header'
  cntrl = (medians[alt][:,3])
  for alt in post.keys():
    dat = ((medians[alt][:,3])) / cntrl
    plt.semilogx( xx, dat, '.', label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post other relative to no header')
  plt.legend()
  plt.grid()


  1/0

  plt.show()



  return


  xx = np.exp(np.arange(MAX_NUM)/SCL)

  plt.figure()
  for alt in post.keys():
    plt.loglog( xx, (post[alt][:,3]+post[alt][:,4])/post[alt][:,0], '.', label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post')
  plt.legend()
  plt.grid()

  plt.figure()
  alt = 'no header'
  cntrl = (post[alt][:,3]+post[alt][:,4])/post[alt][:,0]
  for alt in post.keys():
    dat = (post[alt][:,3]+post[alt][:,4])/post[alt][:,0] / cntrl
    plt.semilogx( xx, dat, label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post relative to no header')
  plt.legend()
  plt.grid()

  plt.figure()
  alt = 'no header'
  cntrl = (post[alt][:,3])/post[alt][:,0]
  for alt in post.keys():
    dat = (post[alt][:,3])/post[alt][:,0] / cntrl
    plt.semilogx( xx, dat, label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post frac relative to no header')
  plt.legend()
  plt.grid()

  plt.figure()
  alt = 'no header'
  cntrl = (post[alt][:,4])/post[alt][:,0]
  for alt in post.keys():
    dat = (post[alt][:,4])/post[alt][:,0] / cntrl
    plt.semilogx( xx, dat, label=alt)
  plt.xlabel('Num problems before experiment')
  plt.ylabel('Num correct after experiment')
  plt.title('pre vs. post other relative to no header')
  plt.legend()
  plt.grid()

  plt.show()


if __name__ == '__main__':
    main()
