from collections.abc import Sequence
from pathlib import Path

from beartype import beartype
import pygit2

from local_review.models import (
    DiffFile,
    DiffLineKind,
    DiffRow,
    DiffSide,
    ReviewDocument,
)


@beartype
def line_context(
    lines: Sequence[str],
    line_start: int,
    line_end: int,
) -> tuple[list[str], list[str]]:
    before_start = max(0, line_start - 4)
    before = list(lines[before_start:line_start - 1])
    after = list(lines[line_end:line_end + 3])
    return before, after


@beartype
def tree_blob(
    repository: pygit2.Repository,
    commit: pygit2.Commit,
    path: str,
) -> pygit2.Blob:
    try:
        entry = commit.tree[path]
    except KeyError as error:
        raise ValueError(
            f"Path {path!r} does not exist in commit {commit.id}") from error

    value = repository[entry.id]
    if not isinstance(value, pygit2.Blob):
        raise TypeError(
            f"Path {path!r} in commit {commit.id} is not a file blob")

    return value


@beartype
def decode_blob(blob: pygit2.Blob) -> str:
    return bytes(blob.data).decode("utf-8", errors="replace")


@beartype
def resolve_commit(
    repository: pygit2.Repository,
    revision: str,
) -> pygit2.Commit:
    try:
        value = repository.revparse_single(revision)
    except KeyError as error:
        raise ValueError(
            f"Git revision {revision!r} does not exist in {repository.workdir!r}"
        ) from error

    try:
        return value.peel(pygit2.Commit)
    except ValueError as error:
        raise ValueError(
            f"Git revision {revision!r} does not resolve to a commit"
        ) from error


@beartype
def repository_head(repository: pygit2.Repository) -> pygit2.Commit:
    if repository.head_is_unborn:
        raise ValueError(
            f"Repository {repository.workdir!r} has an unborn HEAD")

    value = repository[repository.head.target]
    if not isinstance(value, pygit2.Commit):
        raise TypeError(
            f"Repository HEAD {repository.head.target} is not a commit")

    return value


@beartype
def normalize_diff_content(content: str | bytes) -> str:
    match content:
        case bytes():
            value = content.decode("utf-8", errors="replace")
        case str():
            value = content
    return value.rstrip("\r\n")


@beartype
class GitReviewRepository:

    def __init__(
        self,
        path: Path,
        base_revision: str,
        stored_base_oid: str | None,
    ) -> None:
        repository_path = pygit2.discover_repository(str(path))
        if repository_path is None:
            raise ValueError(f"Path {path} is not inside a Git repository")

        self.repository = pygit2.Repository(repository_path)
        if self.repository.workdir is None:
            raise ValueError(
                f"Repository {repository_path!r} has no working tree")

        self.worktree_path = Path(self.repository.workdir).resolve()
        self.base_revision = base_revision
        head = repository_head(self.repository)

        if stored_base_oid is None:
            configured_base = resolve_commit(
                self.repository,
                base_revision,
            )
            merge_base_oid = self.repository.merge_base(
                configured_base.id,
                head.id,
            )
            if merge_base_oid is None:
                raise ValueError(
                    f"Revision {base_revision!r} and HEAD {head.id} have no merge base"
                )
            self.base_commit_oid = merge_base_oid
        else:
            self.base_commit_oid = pygit2.Oid(hex=stored_base_oid)

        base_value = self.repository[self.base_commit_oid]
        if not isinstance(base_value, pygit2.Commit):
            raise TypeError(
                f"Stored base object {self.base_commit_oid} is not a commit")

        self.base_commit = base_value
        self.head_oid = head.id

    def refresh_head(self) -> bool:
        head = repository_head(self.repository)
        if head.id == self.head_oid:
            return False

        self.head_oid = head.id
        return True

    def status_is_current(self, path: str) -> bool:
        status = self.repository.status_file(path)
        return status == pygit2.enums.FileStatus.CURRENT

    def load_diff(self) -> list[DiffFile]:
        flags = (pygit2.enums.DiffOption.INCLUDE_UNTRACKED
                 | pygit2.enums.DiffOption.RECURSE_UNTRACKED_DIRS)
        diff = self.repository.diff(
            self.base_commit.tree,
            None,
            flags=flags,
            context_lines=3,
            interhunk_lines=0,
        )

        files: list[DiffFile] = []
        for patch in diff:
            rows: list[DiffRow] = []
            additions = 0
            deletions = 0

            for hunk in patch.hunks:
                rows.append(
                    DiffRow(
                        kind=DiffLineKind.HUNK,
                        old_line_number=None,
                        new_line_number=None,
                        old_text=None,
                        new_text=None,
                        header=hunk.header.rstrip("\r\n"),
                    ))

                for line in hunk.lines:
                    text = normalize_diff_content(line.content)
                    match line.origin:
                        case "+":
                            kind = DiffLineKind.ADDITION
                            additions += 1
                            old_text = None
                            new_text = text
                        case "-":
                            kind = DiffLineKind.DELETION
                            deletions += 1
                            old_text = text
                            new_text = None
                        case " ":
                            kind = DiffLineKind.CONTEXT
                            old_text = text
                            new_text = text
                        case _:
                            continue

                    old_line_number = line.old_lineno if 0 < line.old_lineno else None
                    new_line_number = line.new_lineno if 0 < line.new_lineno else None
                    rows.append(
                        DiffRow(
                            kind=kind,
                            old_line_number=old_line_number,
                            new_line_number=new_line_number,
                            old_text=old_text,
                            new_text=new_text,
                        ))

            old_path = patch.delta.old_file.path
            new_path = patch.delta.new_file.path
            display_path = new_path or old_path
            files.append(
                DiffFile(
                    display_path=display_path,
                    old_path=old_path,
                    new_path=new_path,
                    additions=additions,
                    deletions=deletions,
                    rows=tuple(rows),
                ))

        return sorted(files, key=lambda value: value.display_path)

    def source_for_anchor(
        self,
        diff_file: DiffFile,
        side: DiffSide,
    ) -> tuple[str, str, str | None, list[str]]:
        match side:
            case DiffSide.OLD:
                path = diff_file.old_path
                blob = tree_blob(
                    self.repository,
                    self.base_commit,
                    path,
                )
                text = decode_blob(blob)
                return path, str(blob.id), str(
                    self.base_commit.id), (text.splitlines())
            case DiffSide.NEW:
                path = diff_file.new_path
                worktree_file = self.worktree_path / path
                if worktree_file.exists():
                    data = worktree_file.read_bytes()
                    blob_oid = str(pygit2.hash(data))
                    snapshot_oid = (str(self.head_oid)
                                    if self.status_is_current(path) else None)
                    return (
                        path,
                        blob_oid,
                        snapshot_oid,
                        data.decode(
                            "utf-8",
                            errors="replace",
                        ).splitlines(),
                    )

                head = repository_head(self.repository)
                blob = tree_blob(self.repository, head, path)
                return (
                    path,
                    str(blob.id),
                    str(head.id),
                    decode_blob(blob).splitlines(),
                )

    def reconcile_pending(
        self,
        document: ReviewDocument,
    ) -> bool:
        head = repository_head(self.repository)
        changed = False

        for thread in document.threads:
            anchor = thread.anchor
            if anchor.snapshot_commit_oid is not None:
                continue
            if anchor.side != DiffSide.NEW:
                continue

            try:
                blob = tree_blob(
                    self.repository,
                    head,
                    anchor.path,
                )
            except ValueError:
                continue

            if str(blob.id) == anchor.blob_oid:
                anchor.snapshot_commit_oid = str(head.id)
                changed = True
                continue

            lines = decode_blob(blob).splitlines()
            selected_lines = anchor.selected_text.splitlines()
            candidate_lines = self.find_relocation_candidates(
                lines,
                selected_lines,
            )
            relocated = self.select_relocation(
                lines,
                candidate_lines,
                selected_lines,
                anchor.context_before,
                anchor.context_after,
            )
            if relocated is None:
                continue

            anchor.line_start = relocated
            anchor.line_end = relocated + len(selected_lines) - 1
            anchor.blob_oid = str(blob.id)
            anchor.snapshot_commit_oid = str(head.id)
            before, after = line_context(
                lines,
                anchor.line_start,
                anchor.line_end,
            )
            anchor.context_before = before
            anchor.context_after = after
            changed = True

        return changed

    def find_relocation_candidates(
        self,
        lines: Sequence[str],
        selected_lines: Sequence[str],
    ) -> list[int]:
        if not selected_lines:
            return []

        width = len(selected_lines)
        result: list[int] = []
        limit = len(lines) - width + 1
        for index in range(max(0, limit)):
            if list(lines[index:index + width]) == list(selected_lines):
                result.append(index + 1)

        return result

    def select_relocation(
        self,
        lines: Sequence[str],
        candidates: Sequence[int],
        selected_lines: Sequence[str],
        context_before: Sequence[str],
        context_after: Sequence[str],
    ) -> int | None:
        if len(candidates) == 1:
            return candidates[0]
        if not candidates:
            return None

        scores: list[tuple[int, int]] = []
        width = len(selected_lines)
        for candidate in candidates:
            before_start = max(0, candidate - 1 - len(context_before))
            before = list(lines[before_start:candidate - 1])
            after_start = candidate - 1 + width
            after = list(lines[after_start:after_start + len(context_after)])
            score = 0
            for expected, actual in zip(
                    reversed(context_before),
                    reversed(before),
            ):
                if expected != actual:
                    break
                score += 1
            for expected, actual in zip(context_after, after):
                if expected != actual:
                    break
                score += 1
            scores.append((score, candidate))

        scores.sort(reverse=True)
        if len(scores) == 1:
            return scores[0][1]
        if scores[1][0] < scores[0][0]:
            return scores[0][1]
        return None
