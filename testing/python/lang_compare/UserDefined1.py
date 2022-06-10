#!/usr/bin/env python


from BaseAnalyzer import *
from common import *


class UserDefined1(BaseAnalyzer):
    def __init__(self, bd):
        super(UserDefined1, self).__init__(bd)

    def fetch(self):
        bd = self.bd
        self.add_table("user1", ["users"])
        for row in [("python", 12), ("cpp", 10), ("js", 8)]:
            bd.execute("insert into user1 (language, users) values (?, ?)", row)
