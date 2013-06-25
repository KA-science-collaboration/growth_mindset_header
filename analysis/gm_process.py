import json
import sys
import re
import optparse
import numpy as np
from collections import defaultdict

# When the intervention was deployed
intervention_start = 1359187200

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

timehist = defaultdict(lambda: defaultdict(lambda: defaultdict(float)))
numhist = defaultdict(lambda: defaultdict(lambda: defaultdict(float)))
users_per = defaultdict(float)

# the attributes to look at
# irt_easiness is the bias term from an IRT model trained on all the exercises -- it gives a sense of how easy an exercise is
attributes = ['num', 'correct', 'proficiencies', 'hints', 'median_time', 'review', 'irt_easiness', 'new_exercises']
# the subsets of the data for which to look at each of these attributes
# pre -- before intervention
# post -- after intervention
# post_frac -- after intervention, only the target exercises
# post_other -- post intervention, only the non-target exercises
subsets = ['pre', 'post', 'post_frac', 'post_other']

# load the parameters from an IRT model so that we can look at average exercise easiness
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
        sys.stdout.write( 'experiment,alternative,identity,intervention_time_start,start,stop,num_coaches,max_coach_students' )
        for s in subsets:
            for a in attributes:
                sys.stdout.write( ",%s_%s"%(a, s) )
        sys.stdout.write("\n")

def remove_duplicates(rows):
    # Remove Duplicate rows
    unique_rows = []
    hashes = set([])
    for row in rows:
        h = hash(tuple(row))
        if h not in hashes:
            unique_rows.append(row)
        hashes |= set([h])
    return unique_rows


def process_user(rows, options):

    # Remove Duplicate rows (~0.6%)
    rows = remove_duplicates(rows)

    # Get Sample size
    N = len(rows)

    if N < options.min_problems:
        return

    # split data into the different columns
    cols = {}
    for field in fields_input:
        cols[field] = np.asarray([row[idx[field]] for row in rows])

    # the control statement is broken apart into sub-alternatives
    alt = cols['alternative'][0]
    #print alt
    if alt == 'control statement':
        #pass
        return

    users_per[alt] += 1

    cols['time_done'] = cols['time_done'].astype(float)
    cols['time_taken'] = cols['time_taken'].astype(float)
    cols['correct'] = cols['correct'].astype(bool)
    cols['proficiency'] = cols['proficiency'].astype(bool)
    cols['review_mode'] = cols['review_mode'].astype(bool)
    cols['topic_mode'] = cols['topic_mode'].astype(bool)
    cols['hints'] = cols['hints'].astype(int)

    # Determine exposure to the intervention
    # On exercises completed after the intervention was started
    cols['exposed'] = np.asarray([
        (ex in target_exercises) and (time >= intervention_start)
        for ex, time in zip(cols['exercise'], cols['time_done'])
    ])
    ## skip users for whom we don't have an exposure time
    #if sum(cols['exposed']) == 0:
    #    return


    # look up the easiness for each of the exercises
    cols['irt_easiness'] = np.zeros(cols['exercise'].shape)
    for ii, ex in enumerate(cols['exercise']):
        #print irt['exercise_ind_dict']
        #print ex
        try:
            ind = irt_ex_ind[ex]
        except KeyError:
            # new exercises that we don't have difficulty estimates for
            #print ex
            cols['irt_easiness'][ii] = np.nan
            continue
        cols['irt_easiness'][ii] = irt_couplings[ind,-1]

    # number the problems done
    cols['num'] = np.arange(N)

    # New Exercises
    # determine if a user has tried the exercise before, 0,
    # or if it is new to them, 1.
    cols['new_exercise'] = np.zeros(cols['exercise'].shape)
    attempted = set([])
    for ii, ex in enumerate(cols['exercise']):
        if ex not in attempted:
            cols['new_exercise'][ii] = 1
            attempted |= set([ex])


    # find times
    aa = cols['time_done']
    bb = cols['exposed']
    cc = cols['num']
    start = min(aa)
    stop = max(aa)
    if sum(bb) > 0:
        intervention_time_start = np.min(aa[bb])
        intervention_time_end = np.max(aa[bb])
        intervention_num_start = np.min(cc[bb])
        intervention_num_end = np.max(cc[bb])
    else:
        intervention_time_start = np.inf
        intervention_time_end = np.inf
        intervention_num_start = np.inf
        intervention_num_end = np.inf

    # mark the problems done pre-intervention
    cols['pre'] = (cols['time_done'] < intervention_time_start)

    cols['time_relative_start'] = cols['time_done'] - intervention_time_start
    cols['time_relative_end'] = cols['time_done'] - intervention_time_end
    cols['num_relative_start'] = cols['num'] - intervention_num_start
    cols['num_relative_end'] = cols['num'] - intervention_num_end

    # and output the aggregated info for the user
    if options.mode=='student':
        sys.stdout.write( cols['experiment'][0] + sep )
        sys.stdout.write( cols['alternative'][0] + sep )
        sys.stdout.write( cols['identity'][0] + sep )
        sys.stdout.write( str(intervention_time_start) + sep )
        sys.stdout.write( str(start) + sep )
        sys.stdout.write( str(stop) + sep )
        sys.stdout.write( cols['num_coaches'][0] + sep )
        sys.stdout.write( cols['max_coach_students'][0] )

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
                    val = ""
                elif a == 'num':
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
                elif a == 'irt_easiness':
                    xx = cols['irt_easiness'][inds]
                    # nansum to ignore exercises we don't know the easiness of
                    val = np.nansum(xx)/sum(np.isfinite(xx))
                elif a == 'new_exercises':
                    val = np.mean(cols['new_exercise'][inds])
                else:
                    print >>sys.stderr,"invalid output column type"

                sys.stdout.write( sep + str(val) )

        sys.stdout.write( "\n" )       
    elif options.mode=='date':
        #if True:
        #    inds = np.asarray(range(cols['correct'].shape[0]))
        for inds in range(cols['correct'].shape[0]):
            td = 60*60*24*7 # timescale of interest -- weeks
            #td = 60*60*24   # timescale of interest -- days
            # bin the time relative to first intervention (on a log scale)
            tt = cols['time_relative_start'][inds]
            #time_start_bin = (np.exp(np.round(np.log(np.abs(tt/td)+1)))-1)*np.sign(tt)
            time_start_bin = np.round(tt/td)
            tt = cols['time_relative_end'][inds]
            #time_end_bin = (np.exp(np.round(np.log(np.abs(tt/td)+1)))-1)*np.sign(tt)
            time_end_bin = np.round(tt/td)

            num_start_bin = cols['num_relative_start'][inds]
            num_end_bin = cols['num_relative_end'][inds]

            for a in attributes:
                if a == 'num':
                    val = 1
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
                elif a == 'irt_easiness':
                    xx = cols['irt_easiness'][inds]
                    # nansum to ignore exercises we don't know the easiness of
                    # DEBUG -- not properly dealing with unknown difficulty exercises!
                    val = 0.
                    #if np.isfinite(xx):
                    #    val = xx
                elif a == 'new_exercises':
                    val = np.mean(cols['new_exercise'][inds])
                else:
                    print >>sys.stderr,"invalid output column type", a
                if abs(time_start_bin) <= options.window_size:
                    timehist[(a,'start')][alt][time_start_bin] += val
                if abs(time_end_bin) <= options.window_size:
                    timehist[(a,'end')][alt][time_end_bin] += val
                if abs(num_start_bin) <= options.window_size:
                    numhist[(a,'start')][alt][num_start_bin] += val
                if abs(num_end_bin) <= options.window_size:
                    numhist[(a,'end')][alt][num_end_bin] += val
    else:
        print >>sys.stderr,"unknown processing mode"



def get_cmd_line_args():
    parser = optparse.OptionParser(
        usage="%prog [options]",
        description="Calculates some per user info for an experiment, and outputs it with different degrees of reduction.")
    parser.add_option("-m", "--mode",
        help="student = one row per student, problem = one row per problem, date = plots vs experiment-relative date and plots vs experiment-relative problem_number",
        default="student")
    parser.add_option("-p", "--post_only",
        help="only look at post-intervention data, and up to 10 days prior",
        default=False)
    parser.add_option("-l", "--min_problems",
        help="minimum number of problems to include a user",
        default=0, type=int)
    parser.add_option("-w", "--window_size",
        help="number of time units (problems/weeks/days) away from intervention boundary to include in plots",
        default=500, type=int)
    options, _ = parser.parse_args()
    return options


def make_plots_date():
    print "plotting"

    basename='userandproblem_timecourse'

    # put this here, so code works when run remotely without X as well, for non plotting cases
    # TODO - set the graphics device to work without X
    import matplotlib.pyplot as plt
    plt.ion()

    fig_i = 1

    for divisor in ['problem', 'user']:
        temporal_array = defaultdict(lambda: defaultdict(dict))
        for a in timehist.keys():
            for alt in timehist[a].keys():
                times = sorted(timehist[a][alt].keys())
                temporal_array[a][alt]['times'] = np.asarray(times)
                v = []
                for t in times:
                    if divisor == 'problem':
                        v.append(timehist[a][alt][t] / timehist[('num',a[1])][alt][t])
                    elif divisor == 'user':
                        v.append(timehist[a][alt][t] / users_per[alt])
                temporal_array[a][alt]['value'] = np.asarray(v)

            fig = plt.figure(fig_i)
            plt.clf()
            for alt in temporal_array[a].keys():
                plt.plot(temporal_array[a][alt]['times'], temporal_array[a][alt]['value'], label=alt)
            plt.xlabel('Time (weeks)')
            #plt.xlabel('Time (days)')
            plt.ylabel("%s per %s"%(str(a[0]), divisor))
            plt.title("%s relative to exposure %s"%(str(a[0]), str(a[1])))
            plt.legend(loc='best')
            plt.grid()
            plt.draw()
            fig.savefig("%s_weeks_%s_%s_per_%s_%d.pdf"%(basename, str(a[0]),str(a[1]),divisor,fig_i))

            fig_i += 1


        temporal_array = defaultdict(lambda: defaultdict(dict))
        for a in numhist.keys():
            for alt in numhist[a].keys():
                times = sorted(numhist[a][alt].keys())
                temporal_array[a][alt]['times'] = np.asarray(times)
                v = []
                for t in times:
                    if divisor == 'problem':
                        v.append(numhist[a][alt][t] / numhist[('num',a[1])][alt][t])
                    elif divisor == 'user':
                        v.append(numhist[a][alt][t] / users_per[alt])
                temporal_array[a][alt]['value'] = np.asarray(v)

            fig = plt.figure(fig_i)
            plt.clf()
            for alt in temporal_array[a].keys():
                plt.plot(temporal_array[a][alt]['times'], temporal_array[a][alt]['value'], label=alt)
            plt.xlabel('Problem Number')
            plt.ylabel("%s per %s"%(str(a[0]), divisor))
            plt.title("%s relative to exposure %s"%(str(a[0]), str(a[1])))
            plt.legend(loc='best')
            plt.grid()
            plt.draw()
            fig.savefig("%s_number_%s_%s_per_%s_%d.pdf"%(basename, str(a[0]),str(a[1]),divisor,fig_i))

            fig_i += 1


        temporal_array = defaultdict(lambda: defaultdict(dict))
        for a in numhist.keys():
            for alt in numhist[a].keys():
                times = sorted(numhist[a][alt].keys())
                temporal_array[a][alt]['times'] = np.asarray(times)
                v = []
                for t in times:
                    if divisor == 'problem':
                        v.append(numhist[a][alt][t] / numhist[('num',a[1])][alt][t])
                    elif divisor == 'user':
                        v.append(numhist[a][alt][t] / users_per[alt])
                temporal_array[a][alt]['value'] = np.asarray(v)

            if 'no header' in temporal_array[a].keys():
                control_vals = temporal_array[a]['no header']['value']
            else:
                control_vals = 1.
            fig = plt.figure(fig_i)
            plt.clf()
            for alt in temporal_array[a].keys():
                plt.plot(temporal_array[a][alt]['times'], temporal_array[a][alt]['value']/control_vals, label=alt)
            plt.xlabel('Problem Number')
            plt.ylabel("%s per %s"%(str(a[0]), divisor))
            plt.title("%s relative to exposure %s, relative to control"%(str(a[0]), str(a[1])))
            plt.legend(loc='best')
            plt.grid()
            plt.draw()
            fig.savefig("%s_number_%s_%s_per_%s_control_ratio_%d.pdf"%(basename, str(a[0]),str(a[1]),divisor,fig_i))

            fig_i += 1




def main():
    options = get_cmd_line_args()

    print_header(options)

    line_count = 0

    oldkey = None
    rows = []
    # accumulate lines, and then call process_user once per user
    for line in sys.stdin:
       line_count += 1
       # DEBUG
       if np.mod(line_count, 1e7)==0:
           print "line %g"%(line_count)
           if options.mode=='date':
               make_plots_date()
       #if line_count > 1e7:
       #    break

       row = linesplit.split( line )

       if options.post_only:
           if float(row[idx.time_done]) < (intervention_start - 60*60*24*10):
               continue

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

    if options.mode=='date':
        # put this here, so code works when run remotely without X as well, for non plotting cases
        # TODO - set the graphics device to work without X
        import matplotlib.pyplot as plt
        make_plots_date()
        plt.show()

        #np.save('temporal_hist', temporal_array)


if __name__ == '__main__':
    main()
