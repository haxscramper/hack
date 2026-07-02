test:
    uv run python -m pytest -vv -ra -s --log-cli-level=DEBUG --capture=fd --disable-warnings > test_results.tmp.log

index:
    uv run src/index_service/cli/cli.py index \
        --reset True \
        --db-name test_index \
        --indexer document_block \
        --logfile /tmp/index-log.log \
        --perf-trace-file /tmp/indexer-trace-perf.json \
        "~/defaultdirs/temporary_interchange/content_root_for_indexing/"

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
    docker run -d -e ARANGO_ROOT_PASSWORD="test" -p 8529:8529 arangodb/enterprise:3.12.9.1 --server.session-timeout 360000
