import json
import sys


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


class FieldIndexer:
    def __init__(self, field_names):
        for i, field in enumerate(field_names):
            self.__dict__[field] = i

fields_input = ['experiment', 'alternative', 'identity', 'exercise', 'problem_type', 'seed', 'problem_number', 'time_done', 'topic_mode', 'correct', 'user_id', 'proficiency', 'count_hints', 'time_taken', 'dt']
idx = FieldIndexer(fields_input)

# split on tab, ',', or \x01 so it works in Hive or via pipes on local machine
linesplit = re.compile('[,\t\x01]')

def process_user(rows, options):
    N = len(rows)

    # and split it up into the different components
    cols = {}
    for field in fields_input:
        cols[field] = np.asarray([row[idx[field]] for row in rows])

    cols['exposed'] = np.asarray([ex in target_exercises for ex in cols['exercise']])
    
    inds = nonzero(cols['exposed'])[0]
    if len(inds) == 0:
        cols['time_post'] = zeros((N))
        cols['time_pre'] = zeros((N))
        cols['dt_post'] = zeros((N))
        cols['dt_pre'] = zeros((N))
    else:
        cols['time_post'] = cols['time_done'] - max(cols['time_done'][inds])
        cols['time_pre'] = cols['time_done'] - min(cols['time_done'][inds])
        cols['dt_post'] = cols['dt'] - max(cols['dt'][inds])
        cols['dt_pre'] = cols['dt'] - min(cols['dt'][inds])

    ind_pre = nonzero(cols['time_pre']<0)[0]
    ind_post = nonzero(cols['time_pre']>=0)[0]
    pre_time = mean(log(cols['time_taken'][ind_pre]))
    post_time = mean(log(cols['time_taken'][ind_post]))
    
    if options.mode=='student':
        print cols['experiment'][0], "\t",
        print cols['alternative'][0], "\t",
        print cols['identity'][0], "\t",
        print pre_time, "\t", post_time, "\t",
        print pre_correct, "\t", post_correct, "\t",
        print pre_num, "\t", post_num, "\t",
    elif options.mode=='problem':
        for i in range(N):
            print 
    else:
        print "invalid mode"


def get_cmd_line_args():
    parser = optparse.OptionParser(
        usage="%prog [options]",
        description="Calculates some per user info for an experiment, and outputs it with different degrees of reduction.")
    parser.add_option("-m", "--mode",
        help="student = one row per student, problem = one row per problem",
        default="student")
    options, _ = parser.parse_args()
    return options


def main():
    options = get_cmd_line_args()
    rows = []
    for line in sys.stdin:
        row = linesplit.split( line )

        newkey = (row[idx.experiment], row[idx.alternative], row[idx.identity])

        if newkey != oldkey and oldkey != None and len(lines) > 0:
            # it's time to process this batch of lines
            # if we've reached the maximum number of exercises to consider,
            # break out and let the end of loop predict do the processing
            process_user(lines, options)
            lines = []     
        oldkey = newkey
        rows.append(row)


if __name__ == '__main__':
    main()
