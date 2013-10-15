import datetime
import hashlib
import experiments
import gae_bingo.identity
import event_log



# TODO(jascha) site tip could probably use some work

# define the tip strings
tip_strings = {
    'no tip': None,
    'site tip': 'Tip: You can find more information about your students and what they are struggling with by visiting the <a href="https://www.khanacademy.org/coach">coach dashboard</a>.',
    'positive encouragement': 'Science Tip: Recent research shows that students learn more when they are confident they can succeed at the problems they practice. When they have faith in themselves or know that others believe they can be successful, it helps them to be more motivated and not get discouraged. Help your students be confident that they are up to the task, and that you know they can do it.',
    'growth neuroscience tip': 'Science Tip: Recent research shows that students learn more after being taught <em>"Your brain is like a muscle. The more you exercise it, the stronger and smarter it gets"</em>. Let your students know how they made their brains stronger this week!',
    'growth effort tip': 'Science Tip:  Recent research shows that students learn more when they are praised for their effort and hard work.  Students learn less when they are praised for being smart.  Let your students know how hard they worked this week.',
    'growth intelligence tip': 'Science Tip: Recent research shows that students learn more when they are taught that the harder they think the smarter they get.  Students do worse if they believe that intelligence is fixed, and that no matter how hard they try they will stay as smart or dumb as they feel right now.  Let your students know how much smarter they got by thinking hard this week.',
    'growth malleable': 'Science Tip: Recent research shows that students learn more when they are taught that intelligence is malleable and can be grown through hard work. Students do worse if they believe that intelligence is fixed, and so are afraid to make mistakes or ask questions because they will look like they aren\'t smart.  Let your students know that they\'re getting smarter every minute they spend practicing math and tackling tough problems.',
    'growth malleable link': 'Science Tip: Recent research shows that students learn more when they are taught that intelligence is malleable and can be grown through hard work. Students do worse if they believe that intelligence is fixed, and so are afraid to make mistakes or ask questions because they will look like they aren\'t smart.  Let your students know that they\'re getting smarter every minute they spend practicing math and tackling tough problems.  To teach your students more, you can show them <a href="https://www.khanacademy.org/brainworkout_1">this page</a>.',
    'growth malleable nonexpert': 'Tip: Students learn more when they are taught that intelligence is malleable and can be grown through hard work. Students do worse if they believe that intelligence is fixed, and so are afraid to make mistakes or ask questions because they will look like they aren\'t smart.  Let your students know that they\'re getting smarter every minute they spend practicing math and tackling tough problems.',
    'explanation short silent': 'Science Tip: Recent research shows that students learn more when they are taught to pause and think through why they are doing each step in a math problem.  Students do worse when they develop a habit of trying things at random when they get frustrated or confused.  Let your students know that they can learn more by pausing and thinking.',
    'explanation short aloud': 'Science Tip: Recent research shows that students learn more when they are taught to pause and explain out loud why they are doing each step in a math problem.  Students do worse when they develop a habit of trying things at random when they get frustrated or confused.  Let your students know that they can learn more by pausing and explaining aloud.',
    'explanation long aloud': 'Science Tip: Recent research shows that students learn more when they explain their thinking while they are doing the steps of a math problem. Let your students know that if they get stuck, they can help themselves by explaining out loud what they are thinking, or trying to explain their thinking to another person. You can also try more frequently asking your students to provide explanations, letting them know that you\'re not trying to test them, but to help them learn.',
    }
# add formatting to the tip text strings
for tip_key in tip_strings.keys():
    tip_text = tip_strings[tip_key]
    if tip_text is None:
        continue
    chunks = tip_text.split(':', 1)
    if len(chunks) > 1:
        chunks[0] = '<strong>' + chunks[0] + ':' + '</strong>'
        tip_text = chunks[0] + chunks[1]
        tip_strings[tip_key] = tip_text


def hash_to_index(max_length, salt=0):
    """
    Generate a pseudorandom integer >= 0 and < max_length, using a hash function
    so it's reproducible.  The hash function is based on the current week, the
    current user, and a salt input.  For the same user, salt, and max_length the
    random number will only change once per week, at midnight Thursday night/
    Friday morning.
    """

    # we will hash on the year, week number and the coach id to choose the 
    # condition
    today = datetime.date.today()
    # this job is run on saturday right before the week boundary, so push it 
    # in to the future so that the week number stays the same for all  
    # coaches even if the job takes more than a day to run
    day_offset = datetime.timedelta(days=2)
    hash_day = today + day_offset
    year = hash_day.year
    week = hash_day.isocalendar()[1]
    identity = gae_bingo.identity.identity()

    # build a hash from the week number and the user id
    sig = hashlib.md5(str(year) + '/' + str(week) +
                                        str(identity) + str(salt)).hexdigest()
    # and use the hash to choose an element from the tip string dictionary
    sig_num = int(sig, base=16)
    sig_index = sig_num % max_length
    return sig_index


def get_tip_string():
    """
    Generate the tip string to be used for the intervention experiments in the
    coach email.
    """

    # all A/B test weights are given as percentages

    # get A/B test condition
    # TODO(jascha) is 'coach' the core category we want?
    test_condition_top = experiments.CoreMetrics.ab_test(
        'coach email intervention',
        alternative_params={
            'control': 40,
            'growth mindset': 30,
            'explanation effect': 15,
            'combination condition': 15
            },
        core_categories='coach')
    test_condition_text = test_condition_top

    if test_condition_top == 'control':
        test_condition_control = experiments.CoreMetrics.ab_test(
            'coach email intervention - control subtest',
            alternative_params={
                'no tip': 30,
                'site tip': 5,
                'positive encouragement': 5,
                },
            core_categories='coach')
        test_condition_text += '.' + test_condition_control
        tip_text = tip_strings[test_condition_control]
    elif test_condition_top == 'growth mindset':
        test_condition_growth = experiments.CoreMetrics.ab_test(
            'coach email intervention - growth subtest',
            alternative_params={
                'growth submessage': 15,
                'growth framing': 15,
                },
            core_categories='coach')
        test_condition_text += '.' + test_condition_growth
        if test_condition_growth == 'growth submessage':
            test_condition_growth_submessage = experiments.CoreMetrics.ab_test(
                'coach email intervention - growth subtest submessage',
                alternative_params={
                    'growth neuroscience tip': 5,
                    'growth effort tip': 5,
                    'growth intelligence tip': 5,
                    },
                core_categories='coach')
            test_condition_text += '.' + test_condition_growth_submessage
            tip_text = tip_strings[test_condition_growth_submessage]
        elif test_condition_growth == 'growth framing':
            test_condition_growth_framing = experiments.CoreMetrics.ab_test(
                'coach email intervention - growth subtest framing',
                alternative_params={
                    'growth malleable': 5,
                    'growth malleable link': 5,
                    'growth malleable nonexpert': 5,
                    },
                core_categories='coach')
            test_condition_text += '.' + test_condition_growth_framing
            tip_text = tip_strings[test_condition_growth_framing]
    elif test_condition_top == 'explanation effect':
        test_condition_explanation = experiments.CoreMetrics.ab_test(
            'coach email intervention - explanation subtest',
            alternative_params={
                'explanation short silent': 5,
                'explanation short aloud': 5,
                'explanation long aloud': 5,
                },
            core_categories='coach')
        test_condition_text += '.' + test_condition_explanation
        tip_text = tip_strings[test_condition_explanation]
    elif test_condition_top == 'combination condition':
        sig_index = hash_to_index(len(tip_strings), salt='combination')
        test_condition_text += '.' + str(sig_index)
        tip_text = tip_strings[tip_strings.keys()[sig_index]]

    # only show a message at all with 50% probability to allow comparisons of
    # on weeks and off weeks
    if hash_to_index(2, salt='display') == 0:
        test_condition_text += '.no display'
        tip_text = None

    # store the tip text in the KA logs
    event_log.log_event('x.intervention.email.test_condition',
                                                        test_condition_text)
    event_log.log_event('x.intervention.email.tip_text', tip_text)

    return tip_text