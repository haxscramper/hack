test: 
    uv run python -m pytest -vv -ra -s --log-cli-level=DEBUG --capture=fd --disable-warnings > test_results.tmp.log

index:
    uv run src/index_service/cli.py \
        --db-name test_index \
        --reset True \
        --indexer exif_metadata \
        --limit 20 \
        "/home/haxscramper/defaultdirs/input/grabber/tensor_saved_high_res_mirror"
