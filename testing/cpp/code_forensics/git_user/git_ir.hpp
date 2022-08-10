#include <sqlite_orm/sqlite_orm.h>
#include <concepts>
#include <iostream>
#include <unordered_map>
#include <experimental/type_traits>

#include "../common.hpp"
#include "../generator.hpp"

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
struct [[nodiscard]] Id {
    using id_base_type = IdType;
    Id(IdType in) : value(in + 1) { // std::cout << " >> " << in << "\n";
    }
    static Id FromValue(IdType in) {
        Id res{IdType{}};
        res.value = in;
        return res;
    }
    bool   isNil() const { return value == IdType{}; }
    IdType getValue() const { return value; }
    void   setValue(IdType arg) { value = arg; }
    IdType getIndex() const { return value - 1; }

  protected:
    IdType value;
};

template <typename D>
concept IsIdType = is_base_of_template_v<Id, D>;

template <typename T>
struct value_type {
    using type = typename T::value_type;
};

template <typename T>
using value_type_t = typename value_type<T>::type;

template <typename T>
struct id_type {
    using type = typename T::id_type;
};

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

    [[nodiscard]] Id add(const T&& value) {
        int index = content.size();
        content.push_back(value);
        return Id(index);
    }

    T&    at(Id id) { return content.at(id.getIndex()); }
    CR<T> at(Id id) const { return content.at(id.getIndex()); }

    generator<std::pair<Id, CP<T>>> pairs() const {
        const int size = content.size();
        for (int i = 0; i < size; ++i) {
            co_yield {Id(i), &content.at(i)};
        }
    }

    generator<CP<T>> items() const {
        for (const auto& it : content) {
            co_yield &it;
        }
    }

  private:
    Vec<T> content;
};

template <IsIdType Id, typename Val>
struct InternStore {
    InternStore() = default;

    std::unordered_map<Val, Id> id_map;
    dod::Store<Id, Val>         content;

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

    Val&    at(Id id) { return content.at(id); }
    CR<Val> at(Id id) const { return content.at(id); }

    generator<std::pair<Id, CP<Val>>> pairs() const {
        return content.pairs();
    }

    generator<CP<Val>> items() const { return content.items(); }
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


template <typename T, typename... Pack>
inline constexpr bool is_in_pack_v = is_in_pack<T, Pack...>::value;

template <typename... Args>
struct MultiStore {
    inline MultiStore() {}

    template <typename Val>
    requires is_in_pack_v<Store<id_type_t<Val>, Val>, Args...>
    auto store() -> Store<id_type_t<Val>, Val>& {
        return std::get<Store<id_type_t<Val>, Val>>(stores);
    }

    template <typename Val>
    requires is_in_pack_v<InternStore<id_type_t<Val>, Val>, Args...>
    auto store() -> InternStore<id_type_t<Val>, Val>& {
        return std::get<InternStore<id_type_t<Val>, Val>>(stores);
    }

    template <typename Val>
    auto add(CR<Val> t) -> id_type_t<Val> {
        return store<Val>().add(t);
    }

    template <dod::IsIdType Id>
    auto at(Id id) -> value_type_t<Id>& {
        return store<value_type_t<Id>>().at(id);
    }

  private:
    std::tuple<Args...> stores;
};

}; // namespace dod

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
        return T{sqlite3_column_int(stmt, columnIndex)};
    }
};
}; // namespace sqlite_orm


namespace ir {

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
        __name(__type arg) : dod::Id<__type>(arg) {}                      \
    };


DECL_ID_TYPE(line, LineId, int);
DECL_ID_TYPE(commit, CommitId, int);
DECL_ID_TYPE(file, FileId, int);
DECL_ID_TYPE(dir, DirectoryId, int);
DECL_ID_TYPE(string, StrId, int);

} // namespace ir


namespace dod {
template <>
struct id_type<Str> {
    using type = ir::StrId;
};
} // namespace dod

namespace ir {

DECL_ID_TYPE(author, AuthorId, int);

/// single commit by author, taken at some point in time
struct commit {
    using id_type = CommitId;
    int         author;   /// references unique author id
    int         time;     /// posix time
    int         timezone; /// timezone where commit was taken
    Vec<FileId> files;
};

struct orm_commit : commit {
    CommitId id;
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

/// Single directory path part, without specification at which point in
/// time it existed. Used for the representation of the repository
/// structure.
struct dir {
    using id_type = DirectoryId;
    Opt<DirectoryId> parent; /// Parent directory ID
    StrId            name;   /// Id of the string
};

/// Table of interned stirngs for different purposes
struct string {
    using id_type = StrId;
    Str text; /// Textual content of the line
};


struct author {
    using id_type = AuthorId;
    Str name;
    Str email;

    bool operator==(CR<author> other) const {
        return name == other.name && email == other.email;
    }
};


/// Single line in a file with all the information that can be relevang for
/// the further analysis
struct line {
    using id_type = LineId;
    AuthorId author;  /// Line author ID
    FileId   file;    /// Parent file ID
    i64      time;    /// Time line was written
    StrId    content; /// Content of the line
};


// struct intern_table {
//     std::unordered_map<Str, StrId> id_map;
//     dod::Store<StrId, Str>         content;

//     [[nodiscard]] StrId add(CR<Str> in) {
//         auto found = id_map.find(in);
//         if (found != id_map.end()) {
//             return found->second;
//         } else {
//             auto result = content.add(in);
//             id_map.insert({in, result});
//             return result;
//         }
//     }
// };
} // namespace ir

namespace std {
template <>
struct hash<ir::author> {
    std::size_t operator()(CR<ir::author> it) const {
        return std::hash<Str>()(it.name) ^ std::hash<Str>()(it.email);
    }
};
}; // namespace std

namespace ir {
struct content_manager {
    dod::MultiStore<
        dod::InternStore<AuthorId, author>, // Full list of authors
        dod::Store<LineId, line>,           // found lines
        dod::Store<FileId, file>,           //
        dod::Store<CommitId, commit>,       //
        dod::Store<DirectoryId, dir>,       //
        dod::InternStore<StrId, Str>>
        multi;

    template <dod::IsIdType Id>
    typename dod::value_type_t<Id>& at(Id id) {
        return multi.at<Id>(id);
    }

    template <typename T>
    [[nodiscard]] dod::id_type_t<T> add(CR<T> it) {
        return multi.add<T>(it);
    }
};

struct orm_file : file {
    FileId id;
};


struct orm_dir : dir {
    DirectoryId id;
};


struct orm_string : string {
    StrId id;
};

struct orm_author : author {
    AuthorId id;
};

struct orm_line : line {
    LineId id;
};

auto create_db() {
    auto storage = make_storage(
        "/tmp/db.sqlite",
        make_table<orm_commit>(
            "commits",
            make_column("id", &orm_commit::id, primary_key()),
            make_column("author", &orm_commit::author),
            make_column("time", &orm_commit::time),
            make_column("timezone", &orm_commit::timezone)),
        make_table<orm_file>(
            "file",
            make_column("id", &orm_file::id, primary_key()),
            make_column("commit_id", &orm_file::commit_id),
            make_column("name", &orm_file::name),
            // make_column("lines", &)
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
            foreign_key(column<orm_line>(&orm_line::file))
                .references(column<orm_file>(&orm_file::id)),
            foreign_key(column<orm_line>(&orm_line::content))
                .references(column<orm_string>(&orm_string::id))),
        make_table<orm_dir>(
            "dir",
            make_column("id", &orm_dir::id, primary_key()),
            make_column("parent", &orm_dir::parent),
            foreign_key(column<orm_dir>(&orm_dir::parent))
                .references(column<orm_dir>(&orm_dir::id))),
        make_table<orm_string>(
            "strings",
            make_column("id", &orm_string::id, primary_key()),
            make_column("text", &orm_string::text)));

    return storage;
}

using DbConnection = decltype(create_db());

} // namespace ir
