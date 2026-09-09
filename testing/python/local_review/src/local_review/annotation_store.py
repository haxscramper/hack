from pathlib import Path

from beartype import beartype

from local_review.models import AnnotationThread, ReviewComment, ReviewDocument


@beartype
class AnnotationStore:

    def __init__(
        self,
        path: Path,
        document: ReviewDocument,
    ) -> None:
        self.path = path
        self.document = document

    def save(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        temporary_path = self.path.with_suffix(f"{self.path.suffix}.temporary")
        data = self.document.model_dump_json(indent=2)
        temporary_path.write_text(f"{data}\n", encoding="utf-8")
        temporary_path.replace(self.path)

    def find_thread(self, thread_id: str) -> AnnotationThread:
        for thread in self.document.threads:
            if thread.id == thread_id:
                return thread

        raise ValueError(f"Annotation thread {thread_id!r} does not exist")

    def find_comment(
        self,
        thread_id: str,
        comment_id: str,
    ) -> ReviewComment:
        thread = self.find_thread(thread_id)
        for comment in thread.comments:
            if comment.id == comment_id:
                return comment

        raise ValueError(
            f"Comment {comment_id!r} does not exist in thread {thread_id!r}")

    def update_comment(
        self,
        thread_id: str,
        comment_id: str,
        text: str,
    ) -> None:
        comment = self.find_comment(thread_id, comment_id)
        comment.text = text
        self.save()

    def append_reply(self, thread_id: str) -> ReviewComment:
        thread = self.find_thread(thread_id)
        comment = ReviewComment()
        thread.comments.append(comment)
        self.save()
        return comment

    def remove_thread(self, thread_id: str) -> None:
        matching = [
            index for index, thread in enumerate(self.document.threads)
            if thread.id == thread_id
        ]
        if not matching:
            raise ValueError(f"Annotation thread {thread_id!r} does not exist")

        self.document.threads.pop(matching[0])
        self.save()
