#!/usr/bin/env python

import requests
import json

import logging
from rich.logging import RichHandler
from rich.pretty import pprint
from rich.console import Console
from rich.table import Table

logging.basicConfig(
    level="NOTSET",
    format="%(message)s",
    datefmt="[%X]",
    handlers=[RichHandler(rich_tracebacks=True, markup=True, enable_link_path=False)],
)

for name in logging.root.manager.loggerDict:
    logger = logging.getLogger(name)
    logger.setLevel(logging.WARNING)

log = logging.getLogger("rich")
log.setLevel(logging.DEBUG)


GITHUB_API_TOKEN = open("secret.key").read().strip()
GITHUB_GRAPHQL_URL = "https://api.github.com/graphql"

OWNER = "nim-lang"
REPO_NAME = "Nim"

query = """
query ListPRs($owner: String!, $name: String!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequests(first: 100, after: $cursor, states: MERGED) {
      pageInfo {
        hasNextPage
        endCursor
      }
      nodes {
        number
        title
        additions
        deletions
        url
        author {
          login
        }
      }
    }
  }
}
"""


def run_query(query, variables):
    request = requests.post(
        GITHUB_GRAPHQL_URL,
        json={"query": query, "variables": variables},
        headers={"Authorization": f"Bearer {GITHUB_API_TOKEN}"},
    )
    if request.status_code == 200:
        return request.json()
    else:
        raise Exception(
            f"Query failed with status code {request.status_code}: {request.text}"
        )


def get_all_prs(owner, repo_name, max_requests: int = 100000):
    has_next_page = True
    cursor = None
    all_prs = []
    idx = 0

    while has_next_page and idx < max_requests:
        variables = {"owner": owner, "name": repo_name, "cursor": cursor}
        result = run_query(query, variables)
        if "errors" in result:
            pprint(result)
            log.error("Request failed")
            return all_prs

        else:
            prs = result["data"]["repository"]["pullRequests"]
            all_prs.extend(prs["nodes"])
            has_next_page = prs["pageInfo"]["hasNextPage"]
            cursor = prs["pageInfo"]["endCursor"]
            log.info(f"Request for cursor OK, {cursor}")

        idx += 1

    return all_prs


def main():
    all_prs = get_all_prs(OWNER, REPO_NAME, 2)

    # Sort PRs by additions + deletions
    sorted_prs = sorted(
        all_prs,
        key=lambda pr: pr["additions"] + pr["deletions"],
        reverse=True,
    )

    # Print the first 20 PRs
    table = Table(title="Prs ordered by change")
    table.add_column("Number", no_wrap=True, style="cyan", justify="right")
    table.add_column("Author", no_wrap=True)
    table.add_column("Title", no_wrap=True)
    table.add_column("Additions", no_wrap=True, justify="right", style="green")
    table.add_column("Deletions", no_wrap=True, justify="right", style="red")
    table.add_column("Total", no_wrap=True, justify="right", style="magenta")


    for pr in sorted_prs[:40]:
        table.add_row(
            str(pr["number"]),
            str(pr["author"]["login"] if pr["author"] else "<none>"),
            str(pr["title"]),
            str(pr["additions"]),
            str(pr["deletions"]),
            str(pr["deletions"] + pr["additions"])
        )

    console = Console()
    console.print(table)


if __name__ == "__main__":
    main()
