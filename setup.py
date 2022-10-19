#
# setup.py - Python setuptools integration
#

import setuptools

setuptools.setup(
    name='ioda-converters',
    version='1.0.0',
    author='JCSDA',
    description='IODA Converters',
    url='https://github.com/JCSDA-internal/ioda-converters',
    package_dir={'': 'src'},
    packages=setuptools.find_packages(where='src'),
    classifiers=[
        'Development Status :: 1 - Planning',
        'Environment :: Console',
        'Intended Audience :: Science/Research',
        'Programming Language :: Python :: 3 :: Only',
        'License :: OSI Approved :: Apache Software License',
        'Natural Language :: English',
        'Operating System :: OS Independent'],
    python_requires='>=3.6',
    install_requires=[
        'ruamel.yaml',
        'PyYAML',
        'jinja2'
    ],
)
