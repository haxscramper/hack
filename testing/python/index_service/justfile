test: 
    uv run python -m pytest -vv -ra -s --log-cli-level=DEBUG --disable-warnings > test_results.tmp.log

index:
    uv run src/index_service/cli.py --db-name test_index "/home/haxscramper/defaultdirs/input/grabber/tensor_saved_high_res_mirror"
