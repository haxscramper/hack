#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from nltk.parse.corenlp import CoreNLPServer
import os


VERSION = "4.5.1"
STANFORD = os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    f"stanford-corenlp-{VERSION}"
)

# Create the server
server = CoreNLPServer(
   os.path.join(STANFORD, f"stanford-corenlp-{VERSION}.jar"),
   os.path.join(STANFORD, f"stanford-corenlp-{VERSION}-models.jar"),    
)

# Start the server in the background
server.start()
