from os import environ
import json

if environ.get('OTREE_PRODUCTION') not in {None, '', '0'}:
    DEBUG = False
    APPS_DEBUG = False
else:
    DEBUG = True
    APPS_DEBUG = True   # also enables random fill in of forms


# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=1.00, participation_fee=1.50, doc="",
    data_pages_enabled=True,
    num_sample_participants=10,
    consent_additional_message = """""",
    round_values = ["1.00"],
    delay = False,
    consent_link = False,
    consent_url = 'xxxxx'
)

SESSION_CONFIGS = [
    dict(
        name='qsp_recip',
        display_name="QSP Recipient",
        num_demo_participants=3,
        app_sequence=[
                        'prolific_ID_begin',
                        'informed_consent',
                        'qsp_dg_recip_intro',
                        'qsp_dg_recip_id',
                        'dg_recip_survey',
                        'qsp_dg_recip_id_2',
                        'dg_recip_survey_p2',
                        'survey_demographics_qsp',
                        'feedback_survey',
                        'prolific_ID_end'
                        ],
        participation_fee = 2.00,
        recip = True,
        delay = True,
        consent = '',
        consent_url = '',
        consent_link = True,
        p_completion_link = 'xxxxxxxx',
    ),
    dict(
        name='qsp_dictator',
        display_name="QSP Dictator",
        num_demo_participants=1,
        pw = 'qsp_testing',
        app_sequence=[
                    'prolific_ID_begin',
                    'informed_consent',
                    'qsp_dg_dict_intro',
                      'qsp_dg_dict_instructions',
                      'dg_qsp',
                      'dg_qsp_survey',
                      'survey_demographics_qsp',
                        'iat_so',
                    'feedback_survey',
                    'prolific_ID_end'
                      ],
        participation_fee = 2.00,
        recip=False,
        delay = True,
        consent = '',
        consent_url = '',
        consent_link = True,
        p_completion_link = 'xxxxxxxx',
        doc="""
    Edit the p_completion_link variable with the completion code for Prolific session
    """
    )
]

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ROOMS = [
    dict(
        name='econ101',
        display_name='Econ 101 class',
        participant_label_file='_rooms/econ101.txt',
    )
]

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """
Here are some oTree games.
"""
# extra settings for otreeutils
# ROOT_URLCONF = 'iat_so.urls'
# CHANNEL_ROUTING = 'iat_so.routing.channel_routing'
# don't share this with anybody.
SECRET_KEY = '7vfsh(zo@d)v)zizkf#@xqzb3q%juzu65zoh4r+#$tckdfji5r'

INSTALLED_APPS = ['otree',
                  'custom_templates',
                  'django.contrib.humanize',
                  'otreeutils'
                  ]
# inactive session configs
# dict(name='trust', display_name="Trust Game", num_demo_participants=2, app_sequence=['trust', 'payment_info']),
# dict(name='prisoner', display_name="Prisoner's Dilemma", num_demo_participants=2,
#      app_sequence=['prisoner', 'payment_info']),
# dict(name='volunteer_dilemma', display_name="Volunteer's Dilemma", num_demo_participants=3,
#      app_sequence=['volunteer_dilemma', 'payment_info']),
# dict(name='cournot', display_name="Cournot Competition", num_demo_participants=2, app_sequence=[
#     'cournot', 'payment_info'
# ]),
# dict(name='dictator', display_name="Dictator Game", num_demo_participants=2,
#      app_sequence=['dictator', 'payment_info']),
# dict(name='matching_pennies', display_name="Matching Pennies", num_demo_participants=2, app_sequence=[
#     'matching_pennies',
# ]),
# dict(name='traveler_dilemma', display_name="Traveler's Dilemma", num_demo_participants=2,
#      app_sequence=['traveler_dilemma', 'payment_info']),
# dict(name='bargaining', display_name="Bargaining Game", num_demo_participants=2,
#      app_sequence=['bargaining', 'payment_info']),
# dict(name='common_value_auction', display_name="Common Value Auction", num_demo_participants=3,
#      app_sequence=['common_value_auction', 'payment_info']),
# dict(name='bertrand', display_name="Bertrand Competition", num_demo_participants=2, app_sequence=[
#     'bertrand', 'payment_info'
# ]),
# dict(name='public_goods_simple', display_name="Public Goods (simple version from tutorial)",
#      num_demo_participants=3, app_sequence=['public_goods_simple', 'payment_info']),
# dict(name='trust_simple', display_name="Trust Game (simple version from tutorial)", num_demo_participants=2,
#      app_sequence=['trust_simple']),
