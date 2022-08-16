from forensics import config
import datetime

config.log_info("PY LOG: test from the user code")
config.log_error("PY_ERR")
config.log_warning("PY WARN")


def path_predicate(path: str) -> bool:
    if path.endswith(".nim"):
        return path.startswith("compiler") or path.startswith("rod")

    elif path.endswith(".rod"):
        return path.startswith("nim")

    else:
        return False


visited_years = set()


def sample_predicate(date, author, oid) -> bool:
    if date.year in visited_years:
        return False

    else:
        visited_years.add(date.year)
        return True


def period_mapping(date) -> bool:
    return date.year


config.set_path_predicate(path_predicate)
config.set_sample_predicate(sample_predicate)
config.set_period_mapping(period_mapping)
