#include "cppdecl/declarations/parse.h"
#include "cppdecl/declarations/to_string.h"
#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <cppdecl/declarations/parse_simple.h>
#include <rapidjson/error/en.h>
#include <rapidjson/filereadstream.h>
#include <rapidjson/reader.h>

#include <quill/Backend.h>
#include <quill/Frontend.h>
#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <quill/sinks/ConsoleSink.h>
#include <quill/std/Array.h>
#include <rapidjson/document.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

#include <cpptrace/cpptrace.hpp>

#include <duckdb.hpp>
#include <filesystem>

#include "to_json.hpp"
#include "to_json_normalized.hpp"
#include <name_tree.hpp>
#include <string_interner.hpp>

struct TraceEvent {
    std::int64_t               pid{};
    std::int64_t               tid{};
    std::int64_t               ts{};
    std::string                cat;
    char                       ph{};
    std::int64_t               id{};
    std::int64_t               dur{};
    std::string                name;
    std::optional<std::string> detail;
};

class TraceEventsSaxHandler
    : public rapidjson::BaseReaderHandler<rapidjson::UTF8<>, TraceEventsSaxHandler> {
  public:
    std::vector<TraceEvent> events;

    bool Null() {
        clearKeyAfterValue();
        return true;
    }

    bool Bool(bool) {
        clearKeyAfterValue();
        return true;
    }

    bool Int(int v) { return Int64(static_cast<std::int64_t>(v)); }

    bool Uint(unsigned v) { return Uint64(static_cast<std::uint64_t>(v)); }

    bool Int64(std::int64_t v) {
        if (eventActive) { assignInteger(v); }
        clearKeyAfterValue();
        return true;
    }

    bool Uint64(std::uint64_t v) {
        if (eventActive) { assignInteger(static_cast<std::int64_t>(v)); }
        clearKeyAfterValue();
        return true;
    }

    bool Double(double v) {
        if (eventActive) { assignInteger(static_cast<std::int64_t>(v)); }
        clearKeyAfterValue();
        return true;
    }

    bool String(const char* str, rapidjson::SizeType len, bool) {
        if (eventActive) {
            if (inArgsObject && currentKey == "detail") {
                currentEvent.detail = std::string(str, len);
            } else if (currentKey == "cat") {
                currentEvent.cat.assign(str, len);
            } else if (currentKey == "name") {
                currentEvent.name.assign(str, len);
            } else if (currentKey == "ph" && len > 0) {
                currentEvent.ph = str[0];
            }
        }
        clearKeyAfterValue();
        return true;
    }

    bool Key(const char* str, rapidjson::SizeType len, bool) {
        currentKey.assign(str, len);
        return true;
    }

    bool StartObject() {
        const bool isTraceEventsArrayAtTopLevel
            = (!scopeStack.empty() && scopeStack.back() == Scope::Array
               && traceEventsArrayActive && scopeStack.size() == traceEventsArrayDepth);

        scopeStack.push_back(Scope::Object);

        if (isTraceEventsArrayAtTopLevel && !eventActive) {
            eventActive      = true;
            eventObjectDepth = scopeStack.size();
            currentEvent     = TraceEvent{};
        } else if (
            eventActive && currentKey == "args"
            && scopeStack.size() == eventObjectDepth + 1) {
            inArgsObject    = true;
            argsObjectDepth = scopeStack.size();
        }

        currentKey.clear();
        return true;
    }

    bool EndObject(rapidjson::SizeType) {
        if (inArgsObject && scopeStack.size() == argsObjectDepth) {
            inArgsObject = false;
        }

        if (eventActive && scopeStack.size() == eventObjectDepth) {
            events.push_back(std::move(currentEvent));
            eventActive = false;
        }

        if (!scopeStack.empty()) { scopeStack.pop_back(); }
        currentKey.clear();
        return true;
    }

    bool StartArray() {
        const bool isTopLevelTraceEventsArray
            = (!scopeStack.empty() && scopeStack.back() == Scope::Object
               && scopeStack.size() == 1 && currentKey == "traceEvents");

        scopeStack.push_back(Scope::Array);

        if (isTopLevelTraceEventsArray) {
            traceEventsArrayActive = true;
            traceEventsArrayDepth  = scopeStack.size();
        }

        currentKey.clear();
        return true;
    }

    bool EndArray(rapidjson::SizeType) {
        if (traceEventsArrayActive && scopeStack.size() == traceEventsArrayDepth) {
            traceEventsArrayActive = false;
        }

        if (!scopeStack.empty()) { scopeStack.pop_back(); }
        currentKey.clear();
        return true;
    }

  private:
    enum class Scope
    {
        Object,
        Array
    };

    std::vector<Scope> scopeStack;
    std::string        currentKey;

    bool        traceEventsArrayActive = false;
    std::size_t traceEventsArrayDepth  = 0;

    bool        eventActive      = false;
    std::size_t eventObjectDepth = 0;
    TraceEvent  currentEvent{};

    bool        inArgsObject    = false;
    std::size_t argsObjectDepth = 0;

    void assignInteger(std::int64_t v) {
        if (currentKey == "pid") {
            currentEvent.pid = v;
        } else if (currentKey == "tid") {
            currentEvent.tid = v;
        } else if (currentKey == "ts") {
            currentEvent.ts = v;
        } else if (currentKey == "id") {
            currentEvent.id = v;
        } else if (currentKey == "dur") {
            currentEvent.dur = v;
        }
    }

    void clearKeyAfterValue() { currentKey.clear(); }
};

std::string toString(const rapidjson::Value& value) {
    rapidjson::StringBuffer                    buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    value.Accept(writer);
    return buffer.GetString();
}

struct EventInfo {
    TraceEvent const* ev;
    int64_t           eventId;
    int64_t           endTs;
    bool              hasScope;
};

class EventDatabaseWriter {
  public:
    explicit EventDatabaseWriter(std::string path)
        : dbPath(std::move(path)), db(openDatabase(dbPath)), con(db) {
        initializeSchemaAndStatements();
    }

    void writeEvents(std::vector<TraceEvent> const& traceEvents) {
        infos.clear();
        infos.reserve(traceEvents.size());
        nextEventId = 0;

        for (auto const& ev : traceEvents) { insertEvent(ev); }

        fillEventNesting();
        flushStringIds();
    }


  private:
    static duckdb::Value makePrefixValue(const std::vector<StrId>& ids) {
        duckdb::vector<duckdb::Value> values;
        values.reserve(ids.size());
        for (StrId id : ids) { values.emplace_back(static_cast<int64_t>(id.raw())); }
        return duckdb::Value::LIST(values);
    }

    void flushStringIds() {
        const auto n = nameTree.interner().size();
        for (std::size_t i = 0; i < n; ++i) {
            const StrId sid = static_cast<StrId>(static_cast<uint32_t>(i));
            auto        r   = insertStringIdStmt->Execute(
                static_cast<int64_t>(sid.raw()), nameTree.interner().get(sid));
            if (r->HasError()) {
                throw cpptrace::runtime_error(
                    "duckdb insert string_ids failed: " + r->GetError());
            }
        }
    }


    std::string        dbPath;
    duckdb::DuckDB     db;
    duckdb::Connection con;

    std::unique_ptr<duckdb::PreparedStatement> insertEventStmt;
    std::unique_ptr<duckdb::PreparedStatement> insertNestedStmt;
    std::unique_ptr<duckdb::PreparedStatement> insertParentStmt;
    std::unique_ptr<duckdb::PreparedStatement> insertStringIdStmt;

    NameTreeStore nameTree;


    std::vector<EventInfo> infos;
    int64_t                nextEventId = 0;

    static duckdb::DuckDB openDatabase(std::string const& path) {
        std::filesystem::remove(path);
        return duckdb::DuckDB(path);
    }

    void initializeSchemaAndStatements() {
        auto createResult = con.Query(
            R"SQL(
INSTALL json;
LOAD json;

CREATE TABLE events (
    event_id BIGINT PRIMARY KEY,
    pid BIGINT,
    tid BIGINT,
    ts BIGINT,
    cat VARCHAR,
    ph VARCHAR,
    id BIGINT,
    dur BIGINT,
    name VARCHAR,
    detail VARCHAR,
    grouping_prefix BIGINT[],
    parsed_json JSON,
    normal_json JSON
);

CREATE TABLE event_nested (
    parent_id BIGINT NOT NULL,
    nested_id BIGINT NOT NULL,
    PRIMARY KEY (parent_id, nested_id)
);

CREATE TABLE event_parent (
    event_id BIGINT PRIMARY KEY,
    parent_id BIGINT
);

CREATE TABLE string_ids (
    str_id BIGINT PRIMARY KEY,
    value VARCHAR NOT NULL
);

)SQL");

        if (createResult->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb create table failed: " + createResult->GetError());
        }

        insertStringIdStmt = con.Prepare(
            R"SQL(
INSERT INTO string_ids (str_id, value) VALUES (?, ?);
)SQL");
        if (insertStringIdStmt->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb prepare string_ids failed: " + insertStringIdStmt->GetError());
        }

        insertEventStmt = con.Prepare(
            R"SQL(
INSERT INTO events (
    event_id, pid, tid, ts, cat, ph, id, dur, name, detail, grouping_prefix, parsed_json, normal_json
)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
)SQL");
        if (insertEventStmt->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb prepare events failed: " + insertEventStmt->GetError());
        }

        insertNestedStmt = con.Prepare(
            R"SQL(
INSERT INTO event_nested (parent_id, nested_id) VALUES (?, ?);
)SQL");
        if (insertNestedStmt->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb prepare nested failed: " + insertNestedStmt->GetError());
        }

        insertParentStmt = con.Prepare(
            R"SQL(
INSERT INTO event_parent (event_id, parent_id) VALUES (?, ?);
)SQL");
        if (insertParentStmt->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb prepare parent failed: " + insertParentStmt->GetError());
        }
    }

    void insertEvent(TraceEvent const& ev) {
        int64_t const eventId = ++nextEventId;

        std::optional<std::string> parsedJson;
        std::optional<std::string> normalJson;
        std::vector<StrId>         groupingPrefix;

        if (ev.name == "InstantiateFunction" && ev.detail.has_value()) {
            cppdecl::ParseQualifiedNameFlags flags            = {};
            std::string_view                 input            = ev.detail.value();
            std::string_view                 inputBeforeParse = input;
            auto result = cppdecl::ParseQualifiedName(input, flags);

            if (auto error = std::get_if<cppdecl::ParseError>(&result)) {
                throw cpptrace::runtime_error(
                    "cppdecl: Parse error in qualified name `"
                    + std::string(inputBeforeParse) + "` at position "
                    + cppdecl::NumberToString(input.data() - inputBeforeParse.data())
                    + ": " + error->message);
            }

            if (result.index() == 0) {
                {
                    rapidjson::Document parsedDoc;
                    parsedDoc.SetObject();
                    auto js = cppdecl::ToJson(
                        std::get<0>(result), parsedDoc.GetAllocator());
                    parsedDoc.CopyFrom(js, parsedDoc.GetAllocator());
                    parsedJson = toString(parsedDoc);
                }

                rapidjson::Document normalizedDoc;
                normalizedDoc.SetObject();
                auto normal = cppdecl::ToJsonNormalized(
                    std::get<0>(result), normalizedDoc.GetAllocator());
                normalizedDoc.CopyFrom(normal, normalizedDoc.GetAllocator());
                normalJson = toString(normalizedDoc);

                EventId treeEventId = static_cast<EventId>(
                    static_cast<uint32_t>(eventId));
                groupingPrefix = nameTree.insertEvent(
                    treeEventId,
                    normalizedDoc,
                    NameTreeStore::VisitMode::PrefixSequenceOnly);

                nameTree.insertEvent(
                    treeEventId, normalizedDoc, NameTreeStore::VisitMode::Insert);
            }
        }

        duckdb::Value detailValue = ev.detail.has_value()
                                      ? duckdb::Value(ev.detail.value())
                                      : duckdb::Value();

        duckdb::Value parsedValue = parsedJson.has_value()
                                      ? duckdb::Value(parsedJson.value())
                                      : duckdb::Value();

        duckdb::Value normalValue = normalJson.has_value()
                                      ? duckdb::Value(normalJson.value())
                                      : duckdb::Value();

        duckdb::Value groupingPrefixValue = groupingPrefix.empty()
                                              ? duckdb::Value()
                                              : makePrefixValue(groupingPrefix);

        auto insertResult = insertEventStmt->Execute(
            eventId,
            ev.pid,
            ev.tid,
            ev.ts,
            ev.cat,
            std::string(1, ev.ph),
            ev.id,
            ev.dur,
            ev.name,
            detailValue,
            groupingPrefixValue,
            parsedValue,
            normalValue);

        if (insertResult->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb insert event failed: " + insertResult->GetError());
        }

        infos.push_back({&ev, eventId, static_cast<int64_t>(ev.ts), false});
    }


    void fillEventNesting() {
        std::map<std::pair<int64_t, int64_t>, std::vector<EventInfo*>> byThread;

        for (auto& e : infos) {
            byThread[{static_cast<int64_t>(e.ev->pid), static_cast<int64_t>(e.ev->tid)}]
                .push_back(&e);
        }

        for (auto& [key, vec] : byThread) {
            std::sort(vec.begin(), vec.end(), [](EventInfo const* a, EventInfo const* b) {
                if (a->ev->ts != b->ev->ts) { return a->ev->ts < b->ev->ts; }
                return a->eventId < b->eventId;
            });

            std::vector<EventInfo*> bstack;
            for (auto* e : vec) {
                if (e->ev->ph == 'B') {
                    bstack.push_back(e);
                } else if (e->ev->ph == 'E') {
                    if (!bstack.empty()) {
                        auto* b     = bstack.back();
                        b->endTs    = e->ev->ts;
                        b->hasScope = true;
                        bstack.pop_back();
                    }
                } else if (e->ev->ph == 'X') {
                    if (e->ev->dur > 0) {
                        e->endTs    = e->ev->ts + e->ev->dur;
                        e->hasScope = true;
                    }
                }
            }
        }

        std::vector<std::pair<int64_t, int64_t>> nestedRows;
        std::unordered_map<int64_t, int64_t>     parentOf;
        nestedRows.reserve(infos.size());

        for (auto& [key, vec] : byThread) {
            std::vector<EventInfo*> stack;
            for (auto* e : vec) {
                while (!stack.empty() && stack.back()->endTs <= e->ev->ts) {
                    stack.pop_back();
                }

                if (e->ev->ph != 'E' && !stack.empty()) {
                    nestedRows.emplace_back(stack.back()->eventId, e->eventId);
                    parentOf[e->eventId] = stack.back()->eventId;
                }

                if (e->hasScope) { stack.push_back(e); }
            }
        }

        for (auto const& [parentId, nestedId] : nestedRows) {
            auto r = insertNestedStmt->Execute(parentId, nestedId);
            if (r->HasError()) {
                throw cpptrace::runtime_error(
                    "duckdb insert nested failed: " + r->GetError());
            }
        }

        for (auto const& e : infos) {
            duckdb::Value parentValue = [&]() -> duckdb::Value {
                auto it = parentOf.find(e.eventId);
                if (it != parentOf.end()) { return duckdb::Value(it->second); }
                return duckdb::Value();
            }();

            auto r = insertParentStmt->Execute(e.eventId, parentValue);
            if (r->HasError()) {
                throw cpptrace::runtime_error(
                    "duckdb insert parent failed: " + r->GetError());
            }
        }
    }
};

int main(int argc, char** argv) {
    quill::BackendOptions backendOptions;
    quill::Backend::start(backendOptions);

    auto consoleSink = quill::Frontend::create_or_get_sink<quill::ConsoleSink>(
        "sink_id_1");
    quill::Logger* logger = quill::Frontend::create_or_get_logger(
        "root", std::move(consoleSink));
    logger->set_log_level(quill::LogLevel::TraceL3);

    const char* inputPath = (argc > 1) ? argv[1] : "input.json";

    std::FILE* file = std::fopen(inputPath, "rb");
    if (file == nullptr) {
        LOG_ERROR(logger, "Failed to open file '{}'", inputPath);
        return EXIT_FAILURE;
    }

    char                      readBuffer[1 << 16];
    rapidjson::FileReadStream stream(file, readBuffer, sizeof(readBuffer));

    rapidjson::Reader     reader;
    TraceEventsSaxHandler handler;

    const bool ok = reader.Parse(stream, handler);
    std::fclose(file);

    if (!ok) {
        LOG_ERROR(
            logger,
            "JSON parse error: '{}' at offset {}",
            rapidjson::GetParseError_En(reader.GetParseErrorCode()),
            reader.GetErrorOffset());
        return EXIT_FAILURE;
    }

    std::vector<TraceEvent> traceEvents = std::move(handler.events);
    LOG_INFO(logger, "Parsed trace events: {}", traceEvents.size());

    EventDatabaseWriter writer("/tmp/result.duckdb");
    writer.writeEvents(traceEvents);

    return EXIT_SUCCESS;
}
