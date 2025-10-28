import logging

logging.getLogger("urllib3.connectionpool").setLevel(logging.INFO)
logging.getLogger("graphviz._tools").setLevel(logging.INFO)

JSON_PATH = "/home/haxscramper/.local/share/multimc/instances/1.21.1 V2/.minecraft/kubejs/server_scripts/all_recipes.json"
USE_GRAPH_CACHE = True
TEXTURE_DIRECTORY = "/home/haxscramper/.local/share/multimc/instances/1.21.1 V2/.minecraft/icon-exports-x32"
USE_ARANGO_CACHE = False
ARANGO_USER = "root"
ARANGO_PASSWORD = "test123"
ARANGO_PORT = 8529
ARANGO_GRAPH_NAME = "mi_recipes"
ARANGO_QUERY_FILE = "select_recipes.aql"
