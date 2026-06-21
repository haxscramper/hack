#include "cppdecl/declarations/parse.h"
#include "cppdecl/declarations/to_string.h"
#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <filesystem>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <CLI/CLI.hpp>

#include <cppdecl/declarations/parse_simple.h>
#include <ranges>
#include <rapidjson/document.h>
#include <rapidjson/error/en.h>
#include <rapidjson/filereadstream.h>
#include <rapidjson/reader.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

#include <quill/Backend.h>
#include <quill/Frontend.h>
#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <quill/sinks/ConsoleSink.h>
#include <quill/sinks/FileSink.h>
#include <quill/std/Array.h>

#include <cpptrace/cpptrace.hpp>
#include <duckdb.hpp>

#include "to_json.hpp"
#include "to_json_normalized.hpp"
#include <name_tree.hpp>
#include <string_interner.hpp>

#include <perfetto_aux.hpp>
#include <perfetto_aux_impl_template.hpp>


struct InputFile {
    std::int64_t fileId{};
    std::string  absPath;
};

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
    std::int64_t               fileId{};
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
    bool set_parsed_json = false;

    explicit EventDatabaseWriter(std::string path, quill::Logger* logger)
        : logger{logger}, dbPath(std::move(path)), db(openDatabase(dbPath)), con(db) {
        initializeSchema();
    }

    void writeEvents(
        std::vector<TraceEvent> const& traceEvents,
        std::vector<InputFile> const&  files) {
        TRACE_EVENT("main", "write all events");
        infos.clear();
        infos.reserve(traceEvents.size());
        nextEventId = 0;

        auto tx_begin = con.Query("BEGIN TRANSACTION");
        if (tx_begin->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb begin transaction failed: " + tx_begin->GetError());
        }

        try {
            duckdb::Appender filesAppender(con, "files");
            for (auto const& f : files) {
                filesAppender.BeginRow();
                filesAppender.Append(f.fileId);
                filesAppender.Append(
                    f.absPath.c_str(), static_cast<uint32_t>(f.absPath.size()));
                filesAppender.EndRow();
            }
            filesAppender.Close();

            duckdb::Appender  eventsAppender(con, "events");
            const std::size_t total     = traceEvents.size();
            const std::size_t logStep   = std::max<std::size_t>(1, total / 100);
            const auto        startedAt = std::chrono::steady_clock::now();

            for (std::size_t idx = 0; idx < total; ++idx) {
                auto const& ev = traceEvents[idx];
                insertEvent(ev, eventsAppender);

                const std::size_t processed = idx + 1;
                if (processed % logStep == 0 || processed == total) {
                    const auto now = std::chrono::steady_clock::now();
                    const std::chrono::duration<double> elapsed = now - startedAt;

                    const double perEventSec  = elapsed.count()
                                              / static_cast<double>(processed);
                    const double remainingSec = perEventSec
                                              * static_cast<double>(total - processed);

                    const int    etaMinutes = static_cast<int>(remainingSec) / 60;
                    const int    etaSeconds = static_cast<int>(remainingSec) % 60;
                    const double percent    = (static_cast<double>(processed) * 100.0)
                                            / static_cast<double>(total);

                    auto width = std::to_string(total).size();

                    LOG_TRACE_L1(
                        logger,
                        "{:.2f}%, ([{:>{}}/{}]) ETA: {:02d}m{:02d}s",
                        percent,
                        processed,
                        width,
                        total,
                        etaMinutes,
                        etaSeconds);
                }
            }

            eventsAppender.Close();

            duckdb::Appender nestedAppender(con, "event_nested");
            duckdb::Appender parentAppender(con, "event_parent");
            fillEventNesting(nestedAppender, parentAppender);
            nestedAppender.Close();
            parentAppender.Close();

            duckdb::Appender stringIdsAppender(con, "string_ids");
            flushStringIds(stringIdsAppender);
            stringIdsAppender.Close();

            auto tx_commit = con.Query("COMMIT");
            if (tx_commit->HasError()) {
                throw cpptrace::runtime_error(
                    "duckdb commit transaction failed: " + tx_commit->GetError());
            }
        } catch (...) {
            auto tx_rollback = con.Query("ROLLBACK");
            if (tx_rollback->HasError()) {
                throw cpptrace::runtime_error(
                    "duckdb rollback failed: " + tx_rollback->GetError());
            }
            throw;
        }
    }


  private:
    static duckdb::Value makePrefixValue(const std::vector<StrId>& ids) {
        duckdb::vector<duckdb::Value> values;
        values.reserve(ids.size());
        for (StrId id : ids) { values.emplace_back(static_cast<int64_t>(id.raw())); }
        return duckdb::Value::LIST(values);
    }

    static duckdb::DuckDB openDatabase(std::string const& path) {
        std::filesystem::remove(path);
        return duckdb::DuckDB(path);
    }

    void initializeSchema() {
        auto createResult = con.Query(
            R"SQL(
INSTALL json;
LOAD json;

CREATE TABLE files (
    file_id BIGINT PRIMARY KEY,
    abs_path VARCHAR NOT NULL
);

CREATE TABLE events (
    event_id BIGINT PRIMARY KEY,
    file_id BIGINT NOT NULL,
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
    }


    void flushStringIds(duckdb::Appender& appender) {
        const auto n = nameTree.interner().size();
        for (std::size_t i = 0; i < n; ++i) {
            const StrId sid = static_cast<StrId>(static_cast<uint32_t>(i));

            appender.BeginRow();
            appender.Append(static_cast<int64_t>(sid.raw()));
            auto const& s = nameTree.interner().get(sid);
            appender.Append(s.c_str(), static_cast<uint32_t>(s.size()));
            appender.EndRow();
        }
    }

    void insertEvent(TraceEvent const& ev, duckdb::Appender& appender) {
        int64_t const eventId = ++nextEventId;

        std::optional<std::string> parsedJson;
        std::optional<std::string> normalJson;
        std::vector<StrId>         groupingPrefix;

        std::string_view input = ev.detail.value();
        std::string      maybe_demangled_content;

        if (ev.detail.has_value()) {
            if (ev.name == "InstantiateFunction" || ev.name == "InstantiateClass"
                || ev.name == "ParseClass") {
                // ev.detail is already demangled
            } else {
                if (input.starts_with('(') && input.ends_with(')')) {
                    input.remove_prefix(1);
                    input.remove_suffix(1);
                }

                int   status    = 0;
                char* demangled = abi::__cxa_demangle(
                    input.data(), nullptr, nullptr, &status);
                if (status == 0) { maybe_demangled_content = std::string{demangled}; }
                std::free(demangled);
            }

            if (!maybe_demangled_content.empty()) {
                cppdecl::ParseQualifiedNameFlags flags = {};
                std::string_view                 input = maybe_demangled_content.empty()
                                                           ? ev.detail.value()
                                                           : maybe_demangled_content;
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
                    if (set_parsed_json) {
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
        }


        appender.BeginRow();
        appender.Append(eventId);
        appender.Append(ev.fileId);
        appender.Append(ev.pid);
        appender.Append(ev.tid);
        appender.Append(ev.ts);
        appender.Append(ev.cat.c_str(), static_cast<uint32_t>(ev.cat.size()));
        char ph_buf[1] = {ev.ph};
        appender.Append(ph_buf, 1);
        appender.Append(ev.id);
        appender.Append(ev.dur);
        appender.Append(ev.name.c_str(), static_cast<uint32_t>(ev.name.size()));

        if (ev.detail.has_value()) {
            auto const& s = ev.detail.value();
            appender.Append(s.c_str(), static_cast<uint32_t>(s.size()));
        } else {
            appender.Append(duckdb::Value());
        }

        if (groupingPrefix.empty()) {
            appender.Append(duckdb::Value());
        } else {
            appender.Append(makePrefixValue(groupingPrefix));
        }

        if (parsedJson.has_value()) {
            auto const& s = parsedJson.value();
            appender.Append(s.c_str(), static_cast<uint32_t>(s.size()));
        } else {
            appender.Append(duckdb::Value());
        }

        if (normalJson.has_value()) {
            auto const& s = normalJson.value();
            appender.Append(s.c_str(), static_cast<uint32_t>(s.size()));
        } else {
            appender.Append(duckdb::Value());
        }

        appender.EndRow();

        infos.push_back({&ev, eventId, static_cast<int64_t>(ev.ts), false});
    }


    void fillEventNesting(
        duckdb::Appender& nestedAppender,
        duckdb::Appender& parentAppender) {
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
            nestedAppender.BeginRow();
            nestedAppender.Append(parentId);
            nestedAppender.Append(nestedId);
            nestedAppender.EndRow();
        }

        for (auto const& e : infos) {
            parentAppender.BeginRow();
            parentAppender.Append(e.eventId);

            auto it = parentOf.find(e.eventId);
            if (it != parentOf.end()) {
                parentAppender.Append(it->second);
            } else {
                parentAppender.Append(duckdb::Value());
            }

            parentAppender.EndRow();
        }
    }

  private:
    quill::Logger*     logger;
    std::string        dbPath;
    duckdb::DuckDB     db;
    duckdb::Connection con;

    NameTreeStore nameTree;

    std::vector<EventInfo> infos;
    int64_t                nextEventId = 0;
};


static bool hasTraceEventsArray(std::filesystem::path const& filePath) {
    std::FILE* file = std::fopen(filePath.string().c_str(), "rb");
    if (file == nullptr) { return false; }

    char                      readBuffer[1 << 16];
    rapidjson::FileReadStream stream(file, readBuffer, sizeof(readBuffer));

    rapidjson::Document doc;
    doc.ParseStream(stream);
    std::fclose(file);

    if (doc.HasParseError() || !doc.IsObject()) { return false; }

    auto it = doc.FindMember("traceEvents");
    return it != doc.MemberEnd() && it->value.IsArray();
}

static std::vector<std::filesystem::path> collectTraceJsonFiles(
    std::filesystem::path const& inputDir) {
    std::vector<std::filesystem::path> files;

    std::filesystem::recursive_directory_iterator it(
        inputDir, std::filesystem::directory_options::skip_permission_denied);
    std::filesystem::recursive_directory_iterator end;

    for (; it != end; ++it) {
        if (!it->is_regular_file()) { continue; }
        if (it->path().extension() != ".json") { continue; }
        if (!hasTraceEventsArray(it->path())) { continue; }
        files.push_back(it->path());
    }

    std::sort(files.begin(), files.end());
    return files;
}

static std::vector<TraceEvent> parseTraceEventsFile(
    std::filesystem::path const& filePath) {
    std::FILE* file = std::fopen(filePath.string().c_str(), "rb");
    if (file == nullptr) {
        throw cpptrace::runtime_error("Failed to open file: " + filePath.string());
    }

    char                      readBuffer[1 << 16];
    rapidjson::FileReadStream stream(file, readBuffer, sizeof(readBuffer));

    rapidjson::Reader     reader;
    TraceEventsSaxHandler handler;

    const bool ok = reader.Parse(stream, handler);
    std::fclose(file);

    if (!ok) {
        throw cpptrace::runtime_error(
            "JSON parse error in '" + filePath.string()
            + "': " + rapidjson::GetParseError_En(reader.GetParseErrorCode())
            + " at offset " + std::to_string(reader.GetErrorOffset()));
    }

    return std::move(handler.events);
}

int main(int argc, char** argv) {
    TRACE_EVENT("main", "main");
    CLI::App app{"trace-events to duckdb"};

    std::unique_ptr<perfetto::TracingSession> tracing_session = StartProcessTracing(
        "Perfetto track example");

    std::string        inputDir;
    std::string        outputDbPath    = "/tmp/result.duckdb";
    std::string        logFilePath     = "/tmp/trace_events.log";
    std::optional<int> max_trace_file  = std::nullopt;
    bool               set_parsed_json = false;

    app.add_option("--input-dir", inputDir, "Input directory with trace JSON files")
        ->required()
        ->check(CLI::ExistingDirectory);

    app.add_option(
        "--set-parsed-json",
        set_parsed_json,
        "Write full parsed-demangled signature for every entry");
    app.add_option("--output-db", outputDbPath, "Output DuckDB file path");
    app.add_option(
        "--max-trace-file", max_trace_file, "Max number of trace files for processing");

    CLI11_PARSE(app, argc, argv);

    quill::BackendOptions backendOptions;
    quill::Backend::start(backendOptions);

    auto consoleSink = quill::Frontend::create_or_get_sink<quill::ConsoleSink>(
        "sink_id_1");
    quill::Logger* logger = quill::Frontend::create_or_get_logger(
        "root", std::move(consoleSink));
    logger->set_log_level(quill::LogLevel::TraceL3);

    auto const traceFiles = collectTraceJsonFiles(inputDir);

    if (traceFiles.empty()) {
        LOG_ERROR(logger, "No matching trace JSON files found in '{}'", inputDir);
        return EXIT_FAILURE;
    }

    std::vector<TraceEvent> allEvents;
    std::vector<InputFile>  inputFiles;
    std::int64_t            nextFileId = 1;

    for (auto const& [idx, filePath] : std::views::enumerate(traceFiles)) {
        TRACE_EVENT("main", "path read loop");
        if (max_trace_file && max_trace_file.value() < static_cast<int>(idx)) {
            LOG_INFO(logger, "Max number of trace files read, moving on to processing");
            break;
        }

        const std::int64_t fileId = nextFileId++;
        const auto
            absPath = std::filesystem::absolute(filePath).lexically_normal().string();
        inputFiles.push_back({fileId, absPath});

        auto fileEvents = parseTraceEventsFile(filePath);
        for (auto& ev : fileEvents) { ev.fileId = fileId; }

        LOG_INFO(
            logger,
            "Parsed file '{}' with {} trace events",
            filePath.string(),
            fileEvents.size());

        allEvents.insert(
            allEvents.end(),
            std::make_move_iterator(fileEvents.begin()),
            std::make_move_iterator(fileEvents.end()));
    }

    LOG_INFO(
        logger,
        "Total files: {}, total trace events: {}",
        traceFiles.size(),
        allEvents.size());


    EventDatabaseWriter writer(outputDbPath, logger);
    writer.writeEvents(allEvents, inputFiles);

    LOG_INFO(logger, "Wrote output database '{}'", outputDbPath);
    StopTracing(std::move(tracing_session), "/tmp/t_common_main_perfetto_trace.pftrace");


    return EXIT_SUCCESS;
}
