#include <sqlite_orm/sqlite_orm.h>
#include <concepts>
#include <iostream>
#include <filesystem>
#include <unordered_map>
#include <experimental/type_traits>

#include "../common.hpp"
#include "../generator.hpp"

/// Helper implementation to pass multiple types around in a 'pack'
template <typename... Args>
struct arg_pack {};

/// Helper type trait to check whether type \param Derived is derived from
/// the \param Base class - including cases when \param Base is an
/// partially specialized type name.
template <template <typename...> class Base, typename Derived>
struct is_base_of_template {
    // A function which can only be called by something convertible to a
    // Base<Ts...>*
    template <typename... Ts>
    static constexpr arg_pack<Ts...> is_callable(Base<Ts...>*);

    // Detector, will return type of calling is_callable, or it won't
    // compile if that can't be done
    template <typename T>
    using is_callable_t = decltype(is_callable(std::declval<T*>()));

    // Is it possible to call is_callable which the Derived type
    static inline constexpr bool
        value = std::experimental::is_detected_v<is_callable_t, Derived>;

    // If it is possible to call is_callable with the Derived type what
    // would it return, if not type is a void
    using type = std::experimental::
        detected_or_t<void, is_callable_t, Derived>;
};

template <template <typename...> class Base, typename Derived>
using is_base_of_template_t = typename is_base_of_template<Base, Derived>::
    type;

/// Convenience helper trait for getting `::value` of the trait check
template <template <typename...> class Base, typename Derived>
inline constexpr bool
    is_base_of_template_v = is_base_of_template<Base, Derived>::value;

using namespace sqlite_orm;


/// This namespace provides implementation of the Data-orited design
/// primitives such as stores and ID type. The implementation is largely
/// based on my understanding of the DOD practices and as such might
/// contain some things that could be implemented differently or ones that
/// are not necessary at all.
namespace dod {
/// DOD Id type
///
/// \note It does not have a default constructor, if you need co construct
/// a an empyt/nil value (not recommended) for some field/argument you can
/// use the static `::Nil` method defined in the derivations produced in
/// the `DECL_ID_TYPE` macro
///
/// \note ID types can be thought of as a pointers or indices into some
/// 'memory' container and as such should only be parametrized using
/// unsigned data types. They *do not* provide any sort of pointer-like
/// arithmentics or any other operations that might fail due to the
/// overflow. Container indices can be freely converted to the pointers.
template <std::integral IdType>
struct [[nodiscard]] Id {
    using id_base_type = IdType;
    /// Create new ID value from the stored ID index.
    Id(IdType in) : value(in + 1) {}


    /// Create new ID object from value, without preemptively incrementing
    /// it
    static Id FromValue(IdType in) {
        Id res{IdType{}};
        res.value = in;
        return res;
    }

    /// Check whether provided value is nil or not
    bool isNil() const { return value == IdType{}; }
    /// Get value stored in the ID  - this one should be used in cases
    /// where ID is converted in some different format (for example printed
    /// out or stored in the database)
    IdType getValue() const { return value; }
    /// Set value of the ID. This should be used for deserialization.
    ///
    /// \note This function allows setting ID to state with zero value,
    /// making it 'nil'
    void setValue(IdType arg) { value = arg; }
    /// Get index of the ID, for accessing the store.
    ///
    /// \warning in case of a 'nil' type this might return an invalid index
    /// (`<0`)
    IdType getIndex() const { return value - 1; }

  protected:
    IdType value;
};

/// Declare new ID type, derived from the `dod::Id` with specified \arg
/// __type as a template parameter. Newly declared type will have a name
/// \arg __name and associated value type `__value`. All three parameters
/// must be specified in order for the definition to be sound.
///
/// Defined type provides implementation of the `::FromValue` and `::Nil`
/// static functions that can be used as an alternative construction
/// methods in various cases.
#define DECL_ID_TYPE(__value, __name, __type)                             \
    struct __value;                                                       \
    struct [[nodiscard]] __name : dod::Id<__type> {                       \
        using value_type = __value;                                       \
        static __name Nil() { return FromValue(0); };                     \
        static __name FromValue(__type arg) {                             \
            __name res{__type{}};                                         \
            res.setValue(arg);                                            \
            return res;                                                   \
        }                                                                 \
        bool operator==(__name other) const {                             \
            return getValue() == other.getValue();                        \
        }                                                                 \
        __name(__type arg) : dod::Id<__type>(arg) {}                      \
    };


/// Concent for base ID type and all it's publcily derived types
template <typename D>
concept IsIdType = is_base_of_template_v<Id, D>;

/// Type trait for accessing value type of the ID type
template <typename T>
struct value_type {
    using type = typename T::value_type;
};

/// Helper alias for accessing value type of the type
template <typename T>
using value_type_t = typename value_type<T>::type;

/// Type trait tructure for inferring id type of the stored type
template <typename T>
struct id_type {
    using type = typename T::id_type;
};

/// Helper alias for accessing id type of the type
template <typename T>
using id_type_t = typename id_type<T>::type;

template <IsIdType Id, typename T>
struct Store {
    Store() = default;

    /// Add value to the storage and return newly created ID
    [[nodiscard]] Id add(const T& value) {
        int index = content.size();
        content.push_back(value);
        return Id(index);
    }

    /// Add new item to the store and return newly created ID
    [[nodiscard]] Id add(const T&& value) {
        int index = content.size();
        content.push_back(value);
        return Id(index);
    }

    T&    at(Id id) { return content.at(id.getIndex()); }
    CR<T> at(Id id) const { return content.at(id.getIndex()); }

    std::size_t size() const { return content.size(); }

    /// Get genetator for all stored indices and pairs
    generator<std::pair<Id, CP<T>>> pairs() const {
        const int size = content.size();
        for (int i = 0; i < size; ++i) {
            co_yield {Id(i), &content.at(i)};
        }
    }

    /// Return generator for stored values
    generator<CP<T>> items() const {
        for (const auto& it : content) {
            co_yield &it;
        }
    }

    /// Insert value into the store at position, either appending to the
    /// internal list or replacing an existing value.
    void insert(Id id, CR<T> value) {
        if (id.getIndex() == content.size()) {
            content.push_back(value);
        } else if (id.getIndex() < content.size()) {
            content[id.getIndex()] = value;
        } else {
            // IMPLEMENT either raise exception or come up with some other
            // way of handing
        }
    }

  private:
    Vec<T> content;
};


/// Interned data store - for values that can be hashed for deduplication.
/// Provided type must be usable as a key for unordered associative
/// continer.
template <IsIdType Id, typename Val>
struct InternStore {
    InternStore() = default;

    std::unordered_map<Val, Id> id_map;
    dod::Store<Id, Val>         content;

    /// Add value to the store - if the value is already contained can
    /// return previous ID
    [[nodiscard]] Id add(CR<Val> in) {
        auto found = id_map.find(in);
        if (found != id_map.end()) {
            return found->second;
        } else {
            auto result = content.add(in);
            id_map.insert({in, result});
            return result;
        }
    }

    bool contains(CR<Val> in) const {
        return id_map.find(in) != id_map.end();
    }

    std::size_t size() const { return content.size(); }
    /// Get mutable reference at the content pointed at by the ID
    Val& at(Id id) { return content.at(id); }
    /// Get immutable references at the content pointed at by the ID
    CR<Val> at(Id id) const { return content.at(id); }

    void insert(Id id, CR<Val> value) {
        if (!contains(value)) {
            content.insert(id, value);
            id_map.insert({value, id});
        }
    }

    /// Return generator of the stored indices and values
    generator<std::pair<Id, CP<Val>>> pairs() const {
        return content.pairs();
    }

    /// Return generator of the stored values
    generator<CP<Val>> items() const { return content.items(); }
};


/// Type trait to check whether provided \param T type is in the \param
/// Pack
template <typename T, typename... Pack>
struct is_in_pack;

template <typename V, typename T0, typename... T>
struct is_in_pack<V, T0, T...> {
    static const bool value = is_in_pack<V, T...>::value;
};

template <typename V, typename... T>
struct is_in_pack<V, V, T...> {
    static const bool value = true;
};

template <typename V>
struct is_in_pack<V> {
    static const bool value = false;
};


/// `::value` accessor for the 'is in pack' type trait
template <typename T, typename... Pack>
inline constexpr bool is_in_pack_v = is_in_pack<T, Pack...>::value;

/// Collecting of several different storage types, used as a boilerplate
/// reduction helper - instead of wrapping around multiple `Store<Id, Val>`
/// fields in the class, adding helper methods to access them via add/at
/// you can write a `MultiStore<>` and list all the required data in the
/// template parameter list. Supports both interned and non-interned
/// storage solutions.
template <typename... Args>
struct MultiStore {
    inline MultiStore() {}

    //// Get reference to the store that is associated with \param Val
    template <typename Val>
    requires is_in_pack_v<Store<id_type_t<Val>, Val>, Args...>
    auto store() -> Store<id_type_t<Val>, Val>& {
        return std::get<Store<id_type_t<Val>, Val>>(stores);
    }

    /// An overload for the interned store case
    template <typename Val>
    requires is_in_pack_v<InternStore<id_type_t<Val>, Val>, Args...>
    auto store() -> InternStore<id_type_t<Val>, Val>& {
        return std::get<InternStore<id_type_t<Val>, Val>>(stores);
    }

    //// Get reference to the store that is associated with \param Val
    template <typename Val>
    requires is_in_pack_v<Store<id_type_t<Val>, Val>, Args...>
    auto store() const -> const Store<id_type_t<Val>, Val>& {
        return std::get<Store<id_type_t<Val>, Val>>(stores);
    }

    /// An overload for the interned store case
    template <typename Val>
    requires is_in_pack_v<InternStore<id_type_t<Val>, Val>, Args...>
    auto store() const -> const InternStore<id_type_t<Val>, Val>& {
        return std::get<InternStore<id_type_t<Val>, Val>>(stores);
    }

    /// Push value on one of the stores, inferring which one based on the
    /// ID
    template <typename Val>
    [[nodiscard]] auto add(CR<Val> t) -> id_type_t<Val> {
        return store<Val>().add(t);
    }

    /// Get value at one of the associated stores, inferring which one
    /// based on the value type of the ID
    template <dod::IsIdType Id>
    auto at(Id id) -> value_type_t<Id>& {
        return store<value_type_t<Id>>().at(id);
    }

    template <typename Id, typename Val>
    void insert(Id id, CR<Val> val) {
        return store<Val>().insert(id, val);
    }

  private:
    std::tuple<Args...> stores; /// List of associated storage contianers
};

}; // namespace dod

namespace std {
template <dod::IsIdType Id>
struct hash<Id> {
    std::size_t operator()(Id it) const {
        // Id uniquely identifies any entry it points to, by defintion, so
        // it can be used as a perfect hash
        return it.getValue();
    }
};
}; // namespace std

template <dod::IsIdType T>
std::ostream& operator<<(std::ostream& stream, T id) {
    if (id.isNil()) {
        stream << "NULL";
    } else {
        stream << id.getValue();
    }
    return stream;
}

namespace sqlite_orm {
template <dod::IsIdType T>
struct type_printer<T> : public integer_printer {};

template <dod::IsIdType T>
struct statement_binder<T> {
    int bind(sqlite3_stmt* stmt, int index, T value) {
        if (value.isNil()) {
            return sqlite3_bind_null(stmt, index);

        } else {
            return statement_binder<typename T::id_base_type>().bind(
                stmt, index, value.getValue());
        }
    }
};

template <dod::IsIdType T>
struct field_printer<T> {
    std::string operator()(T t) const {
        if (t.isNil()) {
            return "NULL";
        } else {
            return field_printer<typename T::id_base_type>()(t.getValue());
        }
    }
};

template <dod::IsIdType T>
struct row_extractor<T> {
    T extract(const char* row_value) {
        return T::FromValue(std::stoi(row_value));
    }

    T extract(sqlite3_stmt* stmt, int columnIndex) {
        return T::FromValue(sqlite3_column_int(stmt, columnIndex));
    }
};
}; // namespace sqlite_orm


namespace ir {

DECL_ID_TYPE(LineData, LineId, std::size_t);
DECL_ID_TYPE(Commit, CommitId, std::size_t);
DECL_ID_TYPE(File, FileId, std::size_t);
DECL_ID_TYPE(Directory, DirectoryId, std::size_t);
DECL_ID_TYPE(String, StringId, std::size_t);

} // namespace ir


namespace dod {
/// Provide struct specialization for string to be able to get it's id
/// type.
template <>
struct id_type<Str> {
    using type = ir::StringId;
};
} // namespace dod

namespace ir {

DECL_ID_TYPE(Author, AuthorId, int);

/// single commit by author, taken at some point in time
struct Commit {
    using id_type = CommitId;
    AuthorId    author;   /// references unique author id
    i64         time;     /// posix time
    int         timezone; /// timezone where commit was taken
    Str         hash;     /// git hash of the commit
    Vec<FileId> files;
};

/// single version of the file that appeared in some commit
struct File {
    using id_type = FileId;
    CommitId commit_id; /// Id of the commit this version of the file was
                        /// recorded in
    DirectoryId parent; /// parent directory
    StringId    name;   /// file name
    Vec<LineId> lines;  /// List of all lines found in the file
};

/// Single directory path part, without specification at which point in
/// time it existed. Used for the representation of the repository
/// structure.
struct Directory {
    using id_type = DirectoryId;
    Opt<DirectoryId> parent; /// Parent directory ID
    StringId         name;   /// Id of the string

    bool operator==(CR<Directory> other) const {
        return name == other.name && parent == other.parent;
    }
};

/// Table of interned stirngs for different purposes
struct String {
    using id_type = StringId;
    Str  text; /// Textual content of the line
    bool operator==(CR<String> other) const { return text == other.text; }
};

/// Author - name and email found during the source code analysis.
struct Author {
    using id_type = AuthorId;
    Str name;
    Str email;

    bool operator==(CR<Author> other) const {
        return name == other.name && email == other.email;
    }
};


/// Single line in a file with all the information that can be relevang for
/// the further analysis. Provides information about the /content/ found at
/// some line. Interned in the main storage.
struct LineData {
    using id_type = LineId;
    AuthorId author;  /// Line author ID
    i64      time;    /// Time line was written
    StringId content; /// Content of the line

    bool operator==(CR<LineData> other) const {
        return author == other.author && time == other.time &&
               content == other.content;
    }
};
} // namespace ir


// Taken from the SO answer
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
inline void hash_combine(std::size_t& seed) {}

template <typename T, typename... Rest>
inline void hash_combine(std::size_t& seed, const T& v, Rest... rest) {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    hash_combine(seed, rest...);
}

#define MAKE_HASHABLE(__type, __varname, ...)                             \
    namespace std {                                                       \
        template <>                                                       \
        struct hash<__type> {                                             \
            std::size_t operator()(const __type& __varname) const {       \
                std::size_t ret = 0;                                      \
                hash_combine(ret, __VA_ARGS__);                           \
                return ret;                                               \
            }                                                             \
        };                                                                \
    }

// Add hashing declarations for the author and line data - they will be
// interned. `std::string` already has the hash structure.
MAKE_HASHABLE(ir::Author, it, it.name, it.email);
MAKE_HASHABLE(ir::LineData, it, it.author, it.time, it.content);
MAKE_HASHABLE(ir::Directory, it, it.name, it.parent);
MAKE_HASHABLE(ir::String, it, it.text);

using Path = std::filesystem::path;

namespace ir {
struct content_manager {
    dod::MultiStore<
        dod::InternStore<AuthorId, Author>, // Full list of authors
        dod::InternStore<LineId, LineData>, // found lines
        dod::Store<FileId, File>,           // files
        dod::Store<CommitId, Commit>,       // all commits
        dod::Store<DirectoryId, Directory>, // all directories
        dod::InternStore<StringId, String>  // all interned strings
        >
        multi;

    std::unordered_map<Str, DirectoryId> prefixes;

    Opt<DirectoryId> parentDirectory(CR<Path> dir) {
        if (dir.has_parent_path()) {
            auto parent = dir.parent_path();
            auto native = parent.native();
            if (prefixes.contains(native)) {
                return prefixes.at(native);
            } else {
                auto result = getDirectory(parent);
                prefixes.insert({parent, result});
                return result;
            }
        } else {
            return Opt<DirectoryId>{};
        }
    }

    DirectoryId getDirectory(CR<Path> dir) {
        return add(ir::Directory{
            .parent = parentDirectory(dir),
            .name   = add(String{dir.filename().native()})});
    }

    /// Get reference to value pointed to by the ID
    template <dod::IsIdType Id>
    typename dod::value_type_t<Id>& at(Id id) {
        return multi.at<Id>(id);
    }

    /// Push in a value, return newly generated ID
    template <typename T>
    [[nodiscard]] dod::id_type_t<T> add(CR<T> it) {
        return multi.add<T>(it);
    }
};


/// Intermediate types for the ORM storage - they are used in order to
/// provide interfacing - `id` field and default constructors (for the
/// `iterate<>()` method in the storage)

struct orm_file : File {
    FileId id;
    orm_file()
        : File{.commit_id = CommitId::Nil(), .parent = DirectoryId::Nil(), .name = StringId::Nil()}
        , id(FileId::Nil()) {}
    orm_file(FileId _id, CR<File> base) : File(base), id(_id) {}
};

struct orm_commit : Commit {
    CommitId id;

    orm_commit()
        : Commit{.author = AuthorId::Nil()}, id(CommitId::Nil()) {}
    orm_commit(CommitId _id, CR<Commit> base) : Commit(base), id(_id) {}
};

struct orm_dir : Directory {
    DirectoryId id;

    orm_dir()
        : Directory{.name = StringId::Nil()}, id(DirectoryId::Nil()) {}
    orm_dir(DirectoryId _id, CR<Directory> base)
        : Directory(base), id(_id) {}
};

struct orm_string : String {
    StringId id;

    orm_string() : String{}, id(StringId::Nil()) {}
    orm_string(StringId _id, CR<String> base) : String(base), id(_id) {}
};

struct orm_author : Author {
    AuthorId id;


    orm_author() : Author{}, id(AuthorId::Nil()) {}
    orm_author(AuthorId _id, CR<Author> base) : Author(base), id(_id) {}
};

struct orm_line : LineData {
    LineId id;
    orm_line()
        : LineData{.author = AuthorId::Nil(), .content = StringId::Nil()}
        , id(LineId::Nil()) {}

    orm_line(LineId _id, CR<LineData> base) : LineData(base), id(_id) {}
};

struct orm_lines_table {
    FileId file;
    int    index;
    LineId line;
};

auto create_db(CR<Str> storagePath) {
    auto storage = make_storage(
        storagePath,
        make_table<orm_commit>(
            "commits",
            make_column("id", &orm_commit::id, primary_key()),
            make_column("author", &orm_commit::author),
            make_column("time", &orm_commit::time),
            make_column("hash", &orm_commit::hash),
            make_column("timezone", &orm_commit::timezone)),
        make_table<orm_file>(
            "file",
            make_column("id", &orm_file::id, primary_key()),
            make_column("commit_id", &orm_file::commit_id),
            make_column("name", &orm_file::name),
            foreign_key(column<orm_file>(&orm_file::name))
                .references(column<orm_string>(&orm_string::id)),
            foreign_key(column<orm_file>(&orm_file::commit_id))
                .references(column<orm_commit>(&orm_commit::id))),
        make_table<orm_author>(
            "author",
            make_column("id", &orm_author::id, primary_key()),
            make_column("name", &orm_author::name),
            make_column("email", &orm_author::email)),
        make_table<orm_line>(
            "line",
            make_column("id", &orm_line::id, primary_key()),
            make_column("author", &orm_line::author),
            make_column("time", &orm_line::time),
            make_column("content", &orm_line::content),
            foreign_key(column<orm_line>(&orm_line::author))
                .references(column<orm_author>(&orm_author::id)),
            foreign_key(column<orm_line>(&orm_line::content))
                .references(column<orm_string>(&orm_string::id))),
        make_table<orm_lines_table>(
            "file_lines",
            make_column("file", &orm_lines_table::file),
            make_column("index", &orm_lines_table::index),
            make_column("line", &orm_lines_table::line)),
        make_table<orm_dir>(
            "dir",
            make_column("id", &orm_dir::id, primary_key()),
            make_column("parent", &orm_dir::parent),
            make_column("name", &orm_dir::name),
            foreign_key(column<orm_dir>(&orm_dir::parent))
                .references(column<orm_dir>(&orm_dir::id))),
        make_table<orm_string>(
            "strings",
            make_column("id", &orm_string::id, primary_key()),
            make_column("text", &orm_string::text)));

    return storage;
}

using DbConnection = decltype(create_db(""));

} // namespace ir
