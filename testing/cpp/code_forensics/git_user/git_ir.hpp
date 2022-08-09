#include <sqlite_orm/sqlite_orm.h>
#include <concepts>
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
    Id(IdType in) : value(in) {}
};

template <typename D>
concept IsIdType = is_base_of_template_v<Id, D>;

template <IsIdType Id, typename T>
struct Store {
    Store(int startIdOffset = 0) : start_id_offset(startIdOffset) {}

    /// Add value to the storage and return newly created ID
    [[nodiscard]] Id add(const T&& value) {
        int index = content.size();
        content.push_back(value);
        return Id{index};
    }

  private:
    const int start_id_offset = 0;
    Vec<T>    content;
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
struct CommitId : dod::Id<int> {};
struct FileId : dod::Id<int> {};
struct DirectoryId : dod::Id<int> {};
struct StrId : dod::Id<int> {};
struct LineId : dod::Id<int> {};
struct AuthorId : dod::Id<int> {};

/// single commit by author, taken at some point in time
struct commit {
    CommitId id;
    int      author;   /// references unique author id
    int      time;     /// posix time
    int      timezone; /// timezone where commit was taken
};

/// single version of the file that appeared in some commit
struct file {
    FileId   id;
    CommitId commit_id; /// Id of the commit this version of the file was
                        /// recorded in
    DirectoryId parent; /// parent directory
    StrId       name;   /// file name
};

/// Single directory path part, without specification at which point in
/// time it existed. Used for the representation of the repository
/// structure.
struct dir {
    DirectoryId id;
    DirectoryId parent; /// Parent directory ID
    StrId       name;   /// Id of the string
};


/// Table of interned stirngs for different purposes
struct string {
    StrId id;
    Str   text; /// Textual content of the line
};

struct author {
    AuthorId id;
    Str      name;
    Str      email;
};

/// Single line in a file with all the information that can be relevang for
/// the further analysis
struct line {
    LineId   id;      /// unique id of the line
    AuthorId author;  /// Line author ID
    int      idx;     /// Index of the line in the file
    FileId   file;    /// Parent file ID
    int      time;    /// Time line was written
    StrId    content; /// Content of the line
};


class content_manager {
    dod::Store<AuthorId, author> authors;
};

auto create_db() {
    auto storage = make_storage(
        "/tmp/db.sqlite",
        make_table(
            "commits",
            make_column("id", &commit::id, primary_key()),
            make_column("author", &commit::author),
            make_column("time", &commit::time),
            make_column("timezone", &commit::timezone)),
        make_table(
            "file",
            make_column("id", &file::id, primary_key()),
            make_column("commit_id", &file::commit_id),
            make_column("name", &file::name),
            foreign_key(&file::name).references(&string::id),
            foreign_key(&file::commit_id).references(&commit::id)),
        make_table(
            "dir",
            make_column("id", &dir::id, primary_key()),
            make_column("parent", &dir::parent),
            foreign_key(&dir::parent).references(&dir::parent)),
        make_table(
            "strings",
            make_column("id", &string::id),
            make_column("text", &string::text)));

    return storage;
}

using DbConnection = decltype(create_db());

} // namespace ir
