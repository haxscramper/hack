#include <sqlite_orm/sqlite_orm.h>
#include <concepts>
#include <unordered_map>
#include <experimental/type_traits>

#include "../common.hpp"


/// Helper implementation to pass multiple types around in a 'pack'
template <typename... Args>
struct arg_pack {};

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

template <template <typename...> class Base, typename Derived>
inline constexpr bool
    is_base_of_template_v = is_base_of_template<Base, Derived>::value;

using namespace sqlite_orm;

namespace dod {
template <std::integral IdType>
struct Id {
    using value_type = IdType;
    IdType value;
    Id() {}
    Id(IdType in) : value(in) {}
    bool isNil() const { return value == IdType{}; }
};

template <typename D>
concept IsIdType = is_base_of_template_v<Id, D>;

template <IsIdType Id, typename T>
struct Store {
    Store(int startIdOffset = 0) : start_id_offset(startIdOffset) {}

    /// Add value to the storage and return newly created ID
    [[nodiscard]] Id add(const T& value) {
        int index = content.size();
        content.push_back(value);
        return Id(index);
    }

    [[nodiscard]] Id add(const T&& value) {
        int index = content.size();
        content.push_back(value);
        return Id(index);
    }

    T&    at(Id id) { return content.at(id.value); }
    CR<T> at(Id id) const { return content.at(id.value); }


  private:
    const int start_id_offset = 0;
    Vec<T>    content;
};

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

template <typename... Args>
struct MultiStore {

    template <typename T>
    requires is_in_pack<Store<typename T::id_type, T>, Args...>::value auto add(
        const T& t) -> typename T::id_type {
        return std::get<Store<typename T::id_type, T>>(stores).add(t);
    }

    template <typename T>
    requires is_in_pack<Store<typename T::id_type, T>, Args...>::value auto at(
        const T id) -> typename T::value_type& {
        return std::get<Store<T, typename T::value_type>>(stores).at(id);
    }


  private:
    std::tuple<Args...> stores;
};

}; // namespace dod

namespace sqlite_orm {
template <dod::IsIdType T>
struct type_printer<T> : public integer_printer {};

template <dod::IsIdType T>
struct statement_binder<T> {
    int bind(sqlite3_stmt* stmt, int index, T value) {
        return statement_binder<typename T::value_type>().bind(
            stmt, index, value.value);
    }
};

template <dod::IsIdType T>
struct field_printer<T> {
    std::string operator()(T t) const {
        return field_printer<typename T::value_type>()(t.value);
    }
};

template <dod::IsIdType T>
struct row_extractor<T> {
    T extract(const char* row_value) { return T{std::stoi(row_value)}; }

    T extract(sqlite3_stmt* stmt, int columnIndex) {
        return T{sqlite3_column_int(stmt, columnIndex)};
    }
};
}; // namespace sqlite_orm


namespace ir {
struct commit;
struct CommitId : dod::Id<int> {
    using value_type = commit;
};
struct file;
struct FileId : dod::Id<int> {
    using value_type = file;
};
struct dir;
struct DirectoryId : dod::Id<int> {
    using value_type = dir;
};
struct string;
struct StrId : dod::Id<int> {
    using value_type = string;
};
struct line;
struct LineId : dod::Id<int> {
    using value_type = line;
};
struct author;
struct AuthorId : dod::Id<int> {
    using value_type = author;
};

/// single commit by author, taken at some point in time
struct commit {
    using id_type = CommitId;
    int author;   /// references unique author id
    int time;     /// posix time
    int timezone; /// timezone where commit was taken
};

struct orm_commit : commit {
    CommitId    id;
    Vec<FileId> files;
};

/// single version of the file that appeared in some commit
struct file {
    using id_type = FileId;
    CommitId commit_id; /// Id of the commit this version of the file was
                        /// recorded in
    DirectoryId parent; /// parent directory
    StrId       name;   /// file name
    Vec<LineId> lines;
};

struct orm_file : file {
    FileId id;
};

/// Single directory path part, without specification at which point in
/// time it existed. Used for the representation of the repository
/// structure.
struct dir {
    using id_type = DirectoryId;
    DirectoryId parent; /// Parent directory ID
    StrId       name;   /// Id of the string
};

struct orm_dir : dir {
    DirectoryId id;
};

/// Table of interned stirngs for different purposes
struct string {
    using id_type = StrId;
    Str text; /// Textual content of the line
};

struct orm_string : string {
    StrId id;
};

struct author {
    using id_type = AuthorId;
    Str name;
    Str email;
};

struct orm_author : author {
    AuthorId id;
};

/// Single line in a file with all the information that can be relevang for
/// the further analysis
struct line {
    using id_type = LineId;
    AuthorId author;  /// Line author ID
    int      idx;     /// Index of the line in the file
    FileId   file;    /// Parent file ID
    i64      time;    /// Time line was written
    StrId    content; /// Content of the line
};


class intern_table {
    std::unordered_map<Str, StrId> id_map;
    dod::Store<StrId, Str>         content;

  public:
    [[nodiscard]] StrId add(CR<Str> in) {
        auto found = id_map.find(in);
        if (found != id_map.end()) {
            return found->second;
        } else {
            return content.add(in);
        }
    }
};

class content_manager {
    dod::MultiStore<
        dod::Store<AuthorId, author>, // Full list of authors
        dod::Store<LineId, line>,     // found lines
        dod::Store<FileId, file>      //
        >
        multi;

    intern_table strings;

  public:
    [[nodiscard]] StrId add(CR<Str> str) { return strings.add(str); }

    template <typename T>
    [[nodiscard]] auto add(CR<T> it) {
        return multi.add(it);
    }
};

auto create_db() {
    auto storage = make_storage(
        "/tmp/db.sqlite",
        make_table(
            "commits",
            make_column("id", &orm_commit::id, primary_key()),
            make_column("author", &orm_commit::author),
            make_column("time", &orm_commit::time),
            make_column("timezone", &orm_commit::timezone)),
        make_table(
            "file",
            make_column("id", &orm_file::id, primary_key()),
            make_column("commit_id", &orm_file::commit_id),
            make_column("name", &orm_file::name),
            foreign_key(&orm_file::name).references(&orm_string::id),
            foreign_key(&orm_file::commit_id).references(&orm_commit::id)),
        make_table(
            "dir",
            make_column("id", &orm_dir::id, primary_key()),
            make_column("parent", &orm_dir::parent),
            foreign_key(&orm_dir::parent).references(&orm_dir::parent)),
        make_table(
            "strings",
            make_column("id", &orm_string::id),
            make_column("text", &orm_string::text)));

    return storage;
}

using DbConnection = decltype(create_db());

} // namespace ir
