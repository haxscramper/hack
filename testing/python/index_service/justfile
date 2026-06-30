etest: 
    uv run python -m pytest -vv -ra -s --log-cli-level=DEBUG --capture=fd --disable-warnings > test_results.tmp.log

index:
    uv run src/index_service/cli/cli.py index \
        --reset True \
        --db-name test_index \
        --indexer comfy_input \
        --indexer exif_metadata \
        --indexer safetensor \
        --indexer generation_params \
        --perf-trace-file /tmp/indexer-trace-perf.json \
        "~/defaultdirs/input" \
        "~/software/ComfyUI/output" \
        "~/software/ComfyUI/models"
        
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
