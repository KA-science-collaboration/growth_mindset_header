"""
Code to take a list of problem cards, and select and add motivational text to 
each of the problem cards.  For use in the growth mindset A/B test.
"""

import experiments
import random
import event_log

# the names of the exercises being targeted with this intervention
# (currently, this is the fractions topic)
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

# the growth mindset messages
growth_messages = [
    "Remember, the more you practice the smarter you become!",
    "When you learn a new kind of math problem, you grow your math brain!",
    "If this is challenging, you have an opportunity to learn and become"
    " smarter!",
    "Mistakes help you learn. Think hard to learn from them",
    "The more you learn today, the smarter you'll be tomorrow!",
    "The harder you try the better you get!",
    "Did you know... Your brain gets better the more you use it.",
    "If you make a mistake, it's an opportunity to get smarter!",
    "Your brain is like a muscle."
    "  The more you flex it, the more powerful it gets!",
    "Thinking hard makes your brain grow stronger!"
    "  Think hard about these math exercises.",
    "Your brain grows new connections every time you practice."
    "  Help your brain grow new connections!",
    "When you have to think harder, it makes you smarter!",
    "Make your brain more powerful!  Practice hard!",
    "Give your brain a workout.  The more you use it,"
    " the stronger it gets.",
    ]

# the positive, non growth-mindset, messages
positive_messages = [
    "Some of these problems are hard. Do your best!",
    "This might be a tough problem, but we know you can do it!",
    "If at first you don't succeed, try again!",
    "The most effective way to do something is to just do it.",
    "When you get a question wrong, be sure to re-read it carefully.",
    "When you have a hard time focusing,"
    " take to a moment to clear your head and re-focus.",
    "Always do your best and you'll get something out of the experience!",
    "Getting enough rest is important to thinking clearly.",
    "Set your goals high, and don't stop until you get there!",
    "We believe in you!",
    "Avoiding distractions like television makes learning much easier.",
    "Concentrate and you will succeed!",
    "The more exercise you get the better you can concentrate!",
    "Being organized helps you accomplish more!",
    ]

# science messages -- these provide a valence neutral control condition
# (but mostly are just for fun)
science_messages = [
    "Did you know: On Mars you would weigh 1/3 as much as on Earth.",
    "Did you know: An elephant brains weighs 7/2 as much as a human brain.",
    "Did you know: If you stare at blue sky you can see white blood cells"
    " moving in your eye.",
    "Did you know: Sound travels faster in hot air than cold air.",
    "Did you know: A hurricane has as much energy as thousands of"
    " nuclear bombs.",
    "Did you know: If you put an empty soda bottle in the freezer,"
    " it will collapse because cold air takes up less space than"
    " hot air.",
    "Did you know: Red cabbage can detect acids and bases."
    "  Soak red cabbage until the water turns purple."
    "  Try adding lemon or baking soda, and see how the water color changes.",
    "Did you know: Dolphins can blow rings of bubbles underwater."
    "  Can you?",
    "Did you know: Mantis shrimp can detect 12 colors.  Humans can only"
    " detect 3 colors.",
    "Did you know: Electric fish talk to each other and find mates"
    " using electricity.",
    "Did you know: Fat has 9/4 as much energy per gram as carbohydrates or"
    " protein.",
    "Did you know: Your brain uses 1/5 of the energy from the food"
    " you eat.",
    "Did you know: Cows have four times as many stomachs as you.  You"
    " have one stomach.",
    "Did you know: Bees tell each other where to find flowers by wiggling"
    " their butts.",
    "Did you know: Only female mosquitos suck blood.",
    "Did you know: Goldfish don't have eyelids.",
    "Did you know: Boy mice sing courtship songs, but they're too"
    " high pitched for us to hear.",
    "Did you know: Butterflies taste with their hind feet.",
]


def add_header_text_to_cards(card, user_exercise):
    """
    Adds header text to a problem card based on exercise and
    A/B test bucket.
    """

    if not (card.exercise_name in target_exercises):
        card.growthHeader = ""
        return

    # get A/B test condition
    test_condition = experiments.CoreMetrics.ab_test("growth mindset header",
            alternative_params={
                "no header": 5,
                "growth mindset": 1, "growth mindset + link": 1,
                "control statement": 2},
            core_categories='all')

    if test_condition == "control statement":
        # nested experiments because only 4 conditions are supported in
        # GAE/Bingo
        test_subcondition = experiments.CoreMetrics.ab_test(
            "growth mindset header subtest",
            alternative_params={
                "positive statement": 1,
                "science statement": 1},
            core_categories='all')
        test_condition += "." + test_subcondition
        
    if test_condition == "no header":
        card.growthHeader = ""
    elif test_condition == "growth mindset":
        message = random.choice(growth_messages)
        card.growthHeader = "<p><em>" + message + "</em></p>"
    elif test_condition == "growth mindset + link":
        message = random.choice(growth_messages)
        card.growthHeader = ('<p><em>' + message + '</em>'
                             '&nbsp&nbsp&nbsp<FONT SIZE="-5">'
                             '<a href=/brainworkout_1 target="_blank">'
                             'LEARN MORE</a>'
                             '</FONT></p>')
    elif test_condition == "control statement.positive statement":
        message = random.choice(positive_messages)
        card.growthHeader = "<p><em>" + message + "</em></p>"
    elif test_condition == "control statement.science statement":
        message = random.choice(science_messages)
        card.growthHeader = "<p><em>" + message + "</em></p>"
        
    # TODO - this will record events even if the card is never 
    # displayed -- for instance, because the student never goes past 
    # the first card in a group of cards.  When interpreting these 
    # results, will need to do the extra work of matching these up 
    # against problemlog entries, and throwing out the ones which 
    # don't match.
    event_log.log_event('x.mindset.test_condition', test_condition)
    event_log.log_event('x.mindset.exercise_name', card.exercise_name)
    problem_type = getattr(card, 'problem_type', 'UNDEFINED')
    event_log.log_event('x.mindset.problem_type', problem_type)
    total_done = getattr(user_exercise, 'total_done', -1)
    event_log.log_event('x.mindset.total_done', total_done)
    event_log.log_event('x.mindset.message_text', card.growthHeader)
