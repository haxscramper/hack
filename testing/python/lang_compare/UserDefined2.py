#!/usr/bin/env python


from BaseAnalyzer import *


class UserDefined2(BaseAnalyzer):
    def __init__(self, bd):
        super(UserDefined2, self).__init__(bd)

    def fetch(self):
        print("fetch for user defined 2")
