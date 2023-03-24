#!/usr/bin/env python

from arango import ArangoClient

# Initialize the ArangoDB client.
client = ArangoClient()

# Connect to "test" database as root user.
db = client.db("test", username="root", password="root")

# Create a new graph named "school" if it does not already exist.
# This returns an API wrapper for "school" graph.
if db.has_graph("school"):
    print("Deleting graph")
    school = db.delete_graph("school")

EDGE = "teach"
FROM = "teachers"
TO = "lectures"
SUP = "mentors"

if db.has_collection(EDGE): db.delete_collection(EDGE)
if db.has_collection(FROM): db.delete_collection(FROM)
if db.has_collection(TO): db.delete_collection(TO)
if db.has_collection(SUP): db.delete_collection(SUP)

school = db.create_graph("school")

# Create an edge definition named EDGE. This creates any missing
# collections and returns an API wrapper for EDGE edge collection.
teach = school.create_edge_definition(
    edge_collection=EDGE,
    from_vertex_collections=[FROM],
    to_vertex_collections=[TO],
)

mentor = school.create_edge_definition(
    edge_collection=SUP,
    from_vertex_collections=[FROM],
    to_vertex_collections=[TO]
)

# Get API wrappers for "from" and "to" vertex collections.
teachers = school.vertex_collection(FROM)
lectures = school.vertex_collection(TO)

# Get the API wrapper for the edge collection.:
teach = school.edge_collection(EDGE)
mentor = school.edge_collection(SUP)

# Insert vertices into the graph.
teachers.insert({"_key": "jon", "name": "Professor Jon", "age": 40})
teachers.insert({"_key": "jim", "name": "Professor Jim", "age": 42})
lectures.insert({"_key": "CSC101", "name": "Introduction to CS", "credits": 10 })
lectures.insert({"_key": "MAT223", "name": "Linear Algebra", "credits": 10 })
lectures.insert({"_key": "STA201", "name": "Statistics", "credits": 20 })

# Insert edges into the graph.
teach.insert({"_from": "teachers/jon", "_to": "lectures/CSC101"})
teach.insert({"_from": "teachers/jon", "_to": "lectures/STA201"})
teach.insert({"_from": "teachers/jon", "_to": "lectures/MAT223"})
teach.insert({"_from": "teachers/jim", "_to": "lectures/MAT223"})
mentor.insert({"_from": "teachers/jim", "_to": "teachers/jon"})



print("done")
