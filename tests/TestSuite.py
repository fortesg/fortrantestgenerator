#! /usr/bin/python

import unittest

# import your test modules
import TestArgumentList
import TestTemplates
import TestPostProcessor

# initialize the test suite
loader = unittest.TestLoader()
suite  = unittest.TestSuite()

# add tests to the test suite
suite.addTests(loader.loadTestsFromModule(TestArgumentList))
suite.addTests(loader.loadTestsFromModule(TestTemplates))
suite.addTests(loader.loadTestsFromModule(TestPostProcessor))

# initialize a runner, pass it your suite and run it
runner = unittest.TextTestRunner(verbosity=3)
result = runner.run(suite)