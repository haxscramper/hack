#include "cppdecl/declarations/parse.h"
#include "cppdecl/declarations/to_string.h"
#include <algorithm>
#include <cstdint>

#include <filesystem>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "event_writer.hpp"
#include <CLI/CLI.hpp>
#include <perfetto_aux_impl_template.hpp>
#include <ranges>

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
