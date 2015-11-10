
try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

config = {
    'description': 'Sys 6018 Project',
    'author': 'Marcus Rosti',
    'url': 'github.com/Marcus-Rosty/Sys_6018_Project',
    'download_url': 'github.com/Marcus-Rosty/Sys_6018_Project',
    'author_email': 'mer3ef@virginia.edu',
    'version': '0.0',
    'install_requires': ['json','numpy','scikit'],
    'packages': ['data_parsing','text_miner'],
    'scripts': [],
    'name': 'projectname'
}

setup(**config)