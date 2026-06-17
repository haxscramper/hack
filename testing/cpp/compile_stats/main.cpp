#include "cppdecl/declarations/parse.h"
#include "cppdecl/declarations/to_string.h"
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <rapidjson/error/en.h>
#include <rapidjson/filereadstream.h>
#include <rapidjson/reader.h>
#include <cppdecl/declarations/parse_simple.h>

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
    : public rapidjson::
          BaseReaderHandler<rapidjson::UTF8<>, TraceEventsSaxHandler> {
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
               && traceEventsArrayActive
               && scopeStack.size() == traceEventsArrayDepth);

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
        if (traceEventsArrayActive
            && scopeStack.size() == traceEventsArrayDepth) {
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

void fill_events(std::vector<TraceEvent> const& traceEvents) {
    const std::string db_path = "/tmp/result.duckdb";
    std::filesystem::remove(db_path);

    duckdb::DuckDB     db(db_path);
    duckdb::Connection con(db);

    con.Query("INSTALL json;");
    con.Query("LOAD json;");

    auto create_result = con.Query(
        "CREATE TABLE events ("
        "  pid BIGINT,"
        "  tid BIGINT,"
        "  ts BIGINT,"
        "  cat VARCHAR,"
        "  ph VARCHAR,"
        "  id BIGINT,"
        "  dur BIGINT,"
        "  name VARCHAR,"
        "  detail VARCHAR,"
        "  parsed_json JSON"
        ");");


    if (create_result->HasError()) {
        throw cpptrace::runtime_error(
            "duckdb create table failed: " + create_result->GetError());
    }

    auto insert_stmt = con.Prepare(
        "INSERT INTO events (pid, tid, ts, cat, ph, id, dur, name, "
        "detail, parsed_json) "
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);");

    if (insert_stmt->HasError()) {
        throw cpptrace::runtime_error(
            "duckdb prepare failed: " + insert_stmt->GetError());
    }

    for (auto const& ev : traceEvents) {
        std::optional<std::string> parsed_json;

        if (ev.name == "InstantiateFunction" && ev.detail.has_value()) {
            cppdecl::ParseQualifiedNameFlags flags = {};
            std::string_view                 input = ev.detail.value();
            std::string_view                 input_before_parse = input;
            auto result = cppdecl::ParseQualifiedName(input, flags);

            if (auto error = std::get_if<cppdecl::ParseError>(&result)) {
                throw cpptrace::runtime_error(
                    "cppdecl: Parse error in qualified name `"
                    + std::string(input_before_parse) + "` at position "
                    + cppdecl::NumberToString(
                        input.data() - input_before_parse.data())
                    + ": " + error->message);
            }

            if (result.index() == 0) {
                rapidjson::Document doc;
                auto                js = cppdecl::ToJson(
                    std::get<0>(result), doc.GetAllocator());
                parsed_json = toString(js);
            }
        }

        duckdb::Value detail_value //
            = ev.detail.has_value() ? duckdb::Value(ev.detail.value())
                                    : duckdb::Value();

        duckdb::Value parsed_value //
            = parsed_json.has_value() ? duckdb::Value(parsed_json.value())
                                      : duckdb::Value();

        auto insert_result = insert_stmt->Execute(
            ev.pid,
            ev.tid,
            ev.ts,
            ev.cat,
            std::string(1, ev.ph),
            ev.id,
            ev.dur,
            ev.name,
            detail_value,
            parsed_value);

        if (insert_result->HasError()) {
            throw cpptrace::runtime_error(
                "duckdb insert failed: " + insert_result->GetError());
        }
    }
}

int main(int argc, char** argv) {
    quill::BackendOptions backend_options;
    quill::Backend::start(backend_options);

    // Frontend
    auto console_sink = quill::Frontend::create_or_get_sink<
        quill::ConsoleSink>("sink_id_1");
    quill::Logger* logger = quill::Frontend::create_or_get_logger(
        "root", std::move(console_sink));

    // Change the LogLevel to print everything
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

    fill_events(traceEvents);

    return EXIT_SUCCESS;
}
