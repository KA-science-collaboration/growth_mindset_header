"""
Prints out a table containing the count for number of problems aqnd number of problems correct for various
sets of problems and all the experimental conditions.

Use like:
cat gm_process_output_130601.csv | python ~/Dropbox/growth_mindset_header/analysis/gm_process_alternatives.py 
where the file piped in is the output of gm_process.py, called in student mode
"""

import numpy as np
from collections import defaultdict
import sys
import re

# split on tab, ',', or \x01 so it works in Hive or via pipes on local machine
linesplit = re.compile('[,\t\x01]')

def main():
  print "alternative,num_pre,num_post_frac,num_post_other,correct_pre,correct_post_frac,correct_post_other"

  line_count = 0
  
  dnum = defaultdict(lambda: np.array([0.,0.,0.]))
  dcorrect = defaultdict(lambda: np.array([0.,0.,0.]))

  oldkey = None
  rows = []
  # accumulate lines, and then call process_user once per user
  for line in sys.stdin:
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
      num_pre = float(row[8])
      correct_pre = float(row[9])*num_pre
      num_post_frac = float(row[24])
      correct_post_frac = float(row[25])*num_post_frac
      num_post_other = float(row[32])
      correct_post_other = float(row[33])*num_post_other

      dnum[alt] += np.array([num_pre, num_post_frac, num_post_other])
      dcorrect[alt] += np.array([correct_pre, correct_post_frac, correct_post_other])
    except:
      print >>sys.stderr, line
    #  print row

  for alt in ['no header', 'positive statement', 'science statement', 'growth mindset', 'growth mindset + link']:
    print "%s,%f,%f,%f,%f,%f,%f"%(alt,dnum[alt][0],dnum[alt][1],dnum[alt][2],dcorrect[alt][0],dcorrect[alt][1],dcorrect[alt][2])

  print "normalized"
  for alt in ['no header', 'positive statement', 'science statement', 'growth mindset', 'growth mindset + link']:
  #for alt in dnum:
    print "%s,%f,%f,%f,%f,%f,%f"%(alt,dnum[alt][0]/dnum[alt][0],dnum[alt][1]/dnum[alt][0],dnum[alt][2]/dnum[alt][0],dcorrect[alt][0]/dnum[alt][0],dcorrect[alt][1]/dnum[alt][0],dcorrect[alt][2]/dnum[alt][0])

  print "normalized to control"
  for alt in ['no header', 'positive statement', 'science statement', 'growth mindset', 'growth mindset + link']:
    if alt == 'no header':
      nrm = [dnum[alt][0]/dnum[alt][0],dnum[alt][1]/dnum[alt][0],dnum[alt][2]/dnum[alt][0],dcorrect[alt][0]/dnum[alt][0],dcorrect[alt][1]/dnum[alt][0],dcorrect[alt][2]/dnum[alt][0]]
    print "%s,%f,%f,%f,%f,%f,%f"%(alt,
      dnum[alt][0]/dnum[alt][0]/nrm[0],
      dnum[alt][1]/dnum[alt][0]/nrm[1],
      dnum[alt][2]/dnum[alt][0]/nrm[2],
      dcorrect[alt][0]/dnum[alt][0]/nrm[3],
      dcorrect[alt][1]/dnum[alt][0]/nrm[4],
      dcorrect[alt][2]/dnum[alt][0]/nrm[5])


if __name__ == '__main__':
    main()
