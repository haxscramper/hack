test:
    uv run python -m pytest -vv -ra \
        --log-level=DEBUG \
        --capture=tee-sys \
        --disable-warnings \
        "tests/gui/test_execute_actions.py" > test_results.tmp.log 2>&1

[env("DISPLAY", ":2")]
test_gui:
    uv run python -m pytest -vv -ra tests/gui \
        --log-level=DEBUG \
        --capture=tee-sys \
        --disable-warnings \
        > test_results.tmp.log 2>&1


# "tests/test_search.py::test_full_text_search" 
# "tests/test_search.py::test_full_text_search"

index:
    uv run src/index_service/cli/cli.py index "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc" 

profile_index:
    uv run py-spy record --format chrometrace -o /tmp/haxdex-perf-index.json -- \
      python src/index_service/cli/cli.py index "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc"


# --indexer file_summary \
# --resource text_summary \
# --resource flm_server \

# --indexer comfy_input \
# --indexer exif_metadata \
# --indexer safetensor \
# --indexer generation_params \

# --enable-cache exif_metadata \
# --indexer exif_metadata \
# --indexer comfy_input \
# --limit-per-path 200 \
# --indexer wd_tags \
# --indexer ffprobe \
# --indexer pdf_pages \

flat_query_view:
    uv run src/index_service/cli/cli.py flat_query_view "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc" 

file_tree:
    uv run src/index_service/cli/cli.py file_tree_view "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc" 

profile_file_tree:
    uv run py-spy record --format chrometrace -o /tmp/haxdex-perf-tree-view.json -- \
      python src/index_service/cli/cli.py file_tree_view "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc"

visual_trash:
    uv run src/index_service/cli/cli.py visual "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc"

gammaray_file_tree:
    uv run gammaray $(uv run which python) src/index_service/cli/cli.py file_tree_view "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc"


run_arango:
    docker run -d -e ARANGO_ROOT_PASSWORD="test" -p 8529:8529 arangodb/enterprise:3.12.9.1 \
        --server.session-timeout 360000 \
        --vector-index
