test:
    uv run python -m pytest -vv -ra \
        --log-level=DEBUG \
        --capture=tee-sys \
        --disable-warnings \
        > test_results.tmp.log 2>&1
        
# "tests/test_search.py::test_full_text_search" 
# "tests/test_search.py::test_full_text_search"

index:
    uv run src/index_service/cli/cli.py "~/defaultdirs/temporary_interchange/content_root_indexing.jsonc" 
        

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

view:
    uv run src/index_service/cli/cli.py view \
        --db-name test_index

run_arango:
    docker run -d -e ARANGO_ROOT_PASSWORD="test" -p 8529:8529 arangodb/enterprise:3.12.9.1 \
        --server.session-timeout 360000 \
        --vector-index
