from pathlib import Path

from index_service.services.core.types import FileHash, FileRef, RootRef


class FileReferenceMixin:

    def _load_roots_from_db(self) -> None:
        for row in self._db.aql.execute("FOR doc IN roots RETURN doc"):
            self.roots[row["_key"]] = Path(row["path"])

    def _load_file_refs_from_db(self) -> None:
        for document in self._db.aql.execute("FOR doc IN files RETURN doc"):
            for path in document["paths"]:
                self.file_refs.add(
                    FileRef.model_validate({
                        "hash": path["hash"],
                        "relative": path["relative"],
                        "root": path["root"],
                    }))

    def _hash(self, path: Path) -> FileHash:
        return self.hash_cache.hash(path)

    def add_root(self, name: str, root: Path) -> RootRef:
        assert name not in self.roots or self.roots[name] == root, (
            f"Duplicate root name {name} with a different path, "
            f"stored {self.roots[name]}, trying to set {root}")

        db_roots = self._db.collection("roots")
        if name in self.roots:
            assert db_roots.has(name), (
                "logical error, roots cache is populated, but the DB does not "
                f"have the value for {name}")
        else:
            self.roots[name] = root
            db_roots.insert({"_key": name, "path": str(root)})

        return RootRef(name=name)

    def get_root(self, name: RootRef) -> Path:
        return self.roots[name.name]

    def get_path(self, ref: FileRef) -> Path:
        return self.roots[ref.root.name].joinpath(ref.relative)

    def get_all_refs(self, hash: FileHash) -> list[FileRef]:
        document = self._db.collection("files").get(hash.hash)
        path_refs = document.get("paths", []) if document else []

        return [
            FileRef(
                hash=FileHash.model_validate(path["hash"]),
                relative=path["relative"],
                root=RootRef.model_validate(path["root"]),
            ) for path in path_refs
        ]

    def as_ref(self, root: RootRef, path: Path) -> FileRef:
        assert root.name in self.roots, (
            f"Unknown root for file ref: '{root}', register root with "
            "`add_root()` first")

        relative = str(path.relative_to(self.get_root(root)))
        full_path = self.get_root(root).joinpath(relative)
        file_hash = self._hash(full_path)

        result = FileRef(
            hash=file_hash,
            relative=relative,
            root=root,
        )

        if result in self.file_refs:
            return result

        result_json = result.model_dump()
        result_json["suffix"] = full_path.suffix
        result_json["name"] = full_path.name

        files = self._db.collection("files")
        document = files.get(file_hash.hash)

        if document is None:
            files.insert({"_key": file_hash.hash, "paths": [result_json]})
        else:
            known_paths = document["paths"]
            already_known = any(entry["relative"] == result_json["relative"] and
                                entry["root"] == result_json["root"]
                                for entry in known_paths)

            if not already_known:
                known_paths.append(result_json)
                files.update({"_key": file_hash.hash, "paths": known_paths})

        self.file_refs.add(result)
        return result

    def get_file_hash(self, ref: FileHash | FileRef) -> str:
        if isinstance(ref, FileHash):
            return ref.hash

        return ref.hash.hash
