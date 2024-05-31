
def test_gantt_endpoint():
    with TemporaryDirectory() as tmp_dir:
        dir = Path(tmp_dir)
        app = create_app(dir)
        client = app.test_client()

        dir.joinpath("file.org").write_text("""
* [2024-02-12] Something
""")
        
        response = client.get("/gantt_chart/file.org")
        assert response.status_code == 200
        value = json.loads(response.text)
        assert "events" in value
        assert len(value["events"]) == 1
        assert value["events"][0]["start"] == "2024-02-12T00:00:00"

def test_tree_structure_endpoint():
    with TemporaryDirectory() as tmp_dir:
        dir = Path(tmp_dir)
        app = create_app(dir)
        client = app.test_client()
        dir.joinpath("file.org").write_text("""
* Main
** Nested
*** Nested3
** Nested2
* Main2
""")
        response = client.get("/tree_structure/file.org")
        assert response.status_code == 200
        value = json.loads(response.text)

        assert value["name"] == "<document>"
        assert value["subtrees"][0]["name"] == "Main"
        assert value["subtrees"][1]["name"] == "Main2"
        assert value["subtrees"][0]["subtrees"][0]["name"] == "Nested"
        assert value["subtrees"][0]["subtrees"][0]["subtrees"][0]["name"] == "Nested3"
