import json
import sys
import re
import optparse
import numpy as np

target_exercises = [
  'adding_and_subtracting_fractions',
  'adding_fractions',
  'adding_fractions_with_common_denominators',
  'changing_fractions_to_decimals_1',
  'changing_fractions_to_percents',
  'changing_percents_to_fractions',
  'comparing_fractions_1',
  'comparing_fractions_2',
  'comparing_improper_fractions_and_mixed_numbers',
  'converting_decimals_to_fractions_1',
  'converting_decimals_to_fractions_2',
  'converting_fractions_to_decimals',
  'converting_mixed_numbers_and_improper_fractions',
  'converting_repeating_decimals_to_fractions_1',
  'converting_repeating_decimals_to_fractions_2',
  'dividing_fractions',
  'dividing_fractions_0.5',
  'dividing_fractions_alternative',
  'dividing_fractions_word_problems',
  'equivalent_fractions',
  'equivalent_fractions_2',
  'expressing_ratios_as_fractions',
  'fractions_cut_and_copy_1',
  'fractions_cut_and_copy_2',
  'fractions_on_the_number_line_1',
  'fractions_on_the_number_line_2',
  'fractions_on_the_number_line_3',
  'fraction_word_problems_1',
  'multiplying_fractions',
  'multiplying_fractions_0.5',
  'multiplying_fractions_word_problems',
  'ordering_fractions',
  'ordering_improper_fractions_and_mixed_numbers',
  'recognizing_fractions',
  'recognizing_fractions_0.5',
  'simplifying_fractions',
  'subtracting_fractions',
  'subtracting_fractions_with_common_denominators',
  ]


# to access input columns
class FieldIndexer(dict):
    def __init__(self, field_names):
        for i, field in enumerate(field_names):
            self.__dict__[field] = i
            self[field] = i

fields_input = ['experiment', 'alternative', 'identity', 'num_coaches', 'max_coach_students', 'time_done', 'dt', 'exercise', 'problem_type', 'seed', 'problem_number', 'topic_mode', 'review_mode', 'correct', 'proficiency', 'hints', 'time_taken']
idx = FieldIndexer(fields_input)


# split on tab, ',', or \x01 so it works in Hive or via pipes on local machine
linesplit = re.compile('[,\t\x01]')

# separator character
sep = ',' # "\t"


# the attributes to look at
attributes = ['num', 'correct', 'proficiencies', 'hints', 'median_time', 'review', 'irt_difficulty']
# the subsets of the data for which to look at each of these attributes
# pre -- before intervention
# post -- after intervention
# post_frac -- after intervention, only the target exercises
# post_other -- post intervention, only the non-target exercises
subsets = ['pre', 'post', 'post_frac', 'post_other']

# load the parameters from an IRT model so that we can look at average exercise difficulty
def generate_exercise_ind():
    """ this function because I stupidly saved the mIRT exercise lookup as a default dict, which needs a function of this name """
    pass
irt = np.load('mirt_data/mirt_file=user_assessment.responses_abilities=1_epoch=1919.npz')
irt_ex_ind = dict(irt['exercise_ind_dict'][()])
irt_couplings = irt['couplings'][()]

def print_header(options):
    if sep != ',':
        # no headers in Hive
        return
    if options.mode=='student':
        print 'experiment, alternative, identity, num_coaches, max_coach_students', 
        for s in subsets:
            for a in attributes:
                print ", %s_%s"%(a, s),
        print

def process_user(rows, options):
    N = len(rows)

    # split data into the different columns
    cols = {}
    for field in fields_input:
        cols[field] = np.asarray([row[idx[field]] for row in rows])

    cols['exposed'] = np.asarray([ex in target_exercises for ex in cols['exercise']])
    # skip users for whom we don't have an exposure time
    if sum(cols['exposed']) == 0:
        return

    cols['time_done'] = cols['time_done'].astype(float)
    cols['time_taken'] = cols['time_taken'].astype(float)
    cols['correct'] = cols['correct'].astype(bool)
    cols['proficiency'] = cols['proficiency'].astype(bool)
    cols['review_mode'] = cols['review_mode'].astype(bool)
    cols['topic_mode'] = cols['topic_mode'].astype(bool)
    cols['hints'] = cols['hints'].astype(int)

    # look up the difficulty for each of the exercises
    cols['irt_difficulty'] = np.zeros(cols['exercise'].shape)
    for ii, ex in enumerate(cols['exercise']):
        #print irt['exercise_ind_dict']
        #print ex
        try:
            ind = irt_ex_ind[ex]
        except KeyError:
            #print ex
            cols['irt_difficulty'][ii] = np.nan
            continue
        cols['irt_difficulty'][ii] = irt_couplings[ind,-1]

    aa = cols['time_done']
    bb = cols['exposed']
    intervention_time = np.min(aa[bb])

    # mark the problems done pre-intervention
    cols['pre'] = (cols['time_done'] < intervention_time)
    
    # and output the aggregated info for the user
    if options.mode=='student':
        print cols['experiment'][0], sep,
        print cols['alternative'][0], sep,
        print cols['identity'][0], sep,
        print cols['num_coaches'][0], sep,
        print cols['max_coach_students'][0],

        for s in subsets:
            if s == 'pre':
                inds = np.nonzero(cols['pre'])[0]
            elif s == 'post':
                inds = np.nonzero(~cols['pre'])[0]
            elif s == 'post_frac':
                inds = np.nonzero(~cols['pre'] & cols['exposed'])[0]
            elif s == 'post_other':
                inds = np.nonzero(~cols['pre'] & ~cols['exposed'])[0]
            for a in attributes:
                if inds.shape[0] == 0:
                    # there's no data for this condition
                    print sep,
                    continue
                if a == 'num':
                    val = inds.shape[0]
                elif a == 'correct':
                    val = np.mean(cols['correct'][inds])
                elif a == 'proficiencies':
                    val = np.mean(cols['proficiency'][inds])
                elif a == 'hints':
                    val = np.mean(cols['hints'][inds])
                elif a == 'median_time':
                    val = np.median(cols['time_taken'][inds])
                elif a == 'review':
                    val = np.mean(cols['review_mode'][inds])
                elif a == 'irt_difficulty':
                    xx = cols['irt_difficulty'][inds]
                    # nansum to ignore exercises we don't know the difficulty of
                    val = np.nansum(xx)/sum(np.isfinite(xx))
                else:
                    print "invalid output column type"

                print sep, val,

        print        
    else:
        print "processing mode not supported yet"


def get_cmd_line_args():
    parser = optparse.OptionParser(
        usage="%prog [options]",
        description="Calculates some per user info for an experiment, and outputs it with different degrees of reduction.")
    parser.add_option("-m", "--mode",
        help="student = one row per student, problem = one row per problem, date = one row per experiment-relative date",
        default="student")
    options, _ = parser.parse_args()
    return options


def main():
    options = get_cmd_line_args()

    print_header(options)

    oldkey = None
    rows = []
    # accumulate lines, and then call process_user once per user
    for line in sys.stdin:
        row = linesplit.split( line )

        newkey = (row[idx.experiment], row[idx.alternative], row[idx.identity])

        if newkey != oldkey and oldkey != None and len(rows) > 0:
            # it's time to process this batch of lines
            # if we've reached the maximum number of exercises to consider,
            # break out and let the end of loop predict do the processing
            process_user(rows, options)
            rows = []     
        oldkey = newkey
        rows.append(row)
    process_user(rows, options)


if __name__ == '__main__':
    main()
