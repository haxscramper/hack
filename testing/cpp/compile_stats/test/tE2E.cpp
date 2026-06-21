#include <event_writer.hpp>
#include <gtest/gtest.h>

#include <chrono>
#include <filesystem>
#include <stdexcept>
#include <string>
#include <vector>


#include <duckdb.hpp>
#include <quill/Backend.h>
#include <quill/Frontend.h>
#include <quill/Logger.h>
#include <rapidjson/reader.h>
#include <rapidjson/stream.h>
#include <rapidjson/stringbuffer.h>

namespace {

std::string make_temp_db_path() {
    auto const now_ns = std::chrono::steady_clock::now().time_since_epoch().count();
    auto const pid    = static_cast<long long>(::getpid());
    return (std::filesystem::temp_directory_path()
            / ("event_db_writer_test_" + std::to_string(pid) + "_"
               + std::to_string(now_ns) + ".duckdb"))
        .string();
}

quill::Logger* get_test_logger() {
    if (auto* root = quill::Frontend::get_logger("root"); root) { return root; }
    auto sink = quill::Frontend::create_or_get_sink<quill::ConsoleSink>(
        "test_console_sink");
    return quill::Frontend::create_or_get_logger("test_logger", std::move(sink));
}

std::vector<TraceEvent> parse_events_from_json(std::string const& json) {
    rapidjson::Reader       reader;
    rapidjson::StringStream stream(json.c_str());
    TraceEventsSaxHandler   handler;

    if (!reader.Parse(stream, handler)) {
        throw std::runtime_error("RapidJSON parse failed");
    }
    return handler.events;
}

int64_t scalar_i64(duckdb::Connection& con, std::string const& sql) {
    auto res = con.Query(sql);
    EXPECT_FALSE(res->HasError()) << res->GetError();
    if (res->HasError()) { return -1; }
    return res->GetValue(0, 0).GetValue<int64_t>();
}

std::string scalar_str(duckdb::Connection& con, std::string const& sql) {
    auto res = con.Query(sql);
    EXPECT_FALSE(res->HasError()) << res->GetError();
    if (res->HasError()) { return {}; }
    return res->GetValue(0, 0).ToString();
}

} // namespace

class EventDatabaseWriterFixture : public ::testing::Test {
  protected:
    static void SetUpTestSuite() { quill::Backend::start(); }

    void SetUp() override {
        db_path = make_temp_db_path();
        logger  = get_test_logger();
    }

    void TearDown() override {
        std::error_code ec;
        std::filesystem::remove(db_path, ec);
    }

    duckdb::Connection open_read_connection() {
        read_db  = std::make_unique<duckdb::DuckDB>(db_path);
        read_con = std::make_unique<duckdb::Connection>(*read_db);
        return duckdb::Connection(*read_db);
    }

    std::string    db_path;
    quill::Logger* logger{nullptr};

  private:
    std::unique_ptr<duckdb::DuckDB>     read_db;
    std::unique_ptr<duckdb::Connection> read_con;
};

TEST_F(EventDatabaseWriterFixture, WritesFilesAndEventsFromTraceJson) {
    std::string const json = R"json(
{
  "traceEvents": [
    { "pid": 11, "tid": 22, "ts": 100, "cat": "Source", "ph": "b", "id": 0, "name": "Source", "args": {"detail": "/tmp/a.hpp"} },
    { "pid": 11, "tid": 22, "ts": 130, "cat": "Source", "ph": "e", "id": 0, "name": "Source", "args": {"detail": "/tmp/a.hpp"} },
    { "pid": 11, "tid": 22, "ts": 150, "ph": "X", "dur": 20, "name": "ParseClass", "args": {"detail": "std::numeric_limits"} }
  ]
}
)json";

    auto events = parse_events_from_json(json);
    ASSERT_EQ(events.size(), 3u);

    std::vector<InputFile> files{InputFile{
        1, std::filesystem::absolute("/tmp/trace_1.json").lexically_normal().string()}};

    for (auto& ev : events) { ev.fileId = 1; }

    EventDatabaseWriter writer(db_path, logger);
    writer.writeEvents(events, files);

    duckdb::DuckDB     db(db_path);
    duckdb::Connection con(db);

    EXPECT_EQ(scalar_i64(con, "SELECT COUNT(*) FROM files"), 1);
    EXPECT_EQ(scalar_i64(con, "SELECT COUNT(*) FROM events"), 3);

    EXPECT_EQ(
        scalar_str(con, "SELECT abs_path FROM files WHERE file_id = 1"),
        files.front().absPath);

    EXPECT_EQ(scalar_str(con, "SELECT ph FROM events WHERE event_id = 1"), "b");
    EXPECT_EQ(scalar_str(con, "SELECT ph FROM events WHERE event_id = 2"), "e");
    EXPECT_EQ(scalar_str(con, "SELECT ph FROM events WHERE event_id = 3"), "X");
}

TEST_F(EventDatabaseWriterFixture, BuildsEventParentAndNestedTables) {
    std::vector<InputFile> files{InputFile{
        1, std::filesystem::absolute("/tmp/trace_2.json").lexically_normal().string()}};

    std::vector<TraceEvent> events{
        TraceEvent{1, 1, 100, "cat", 'B', 0, 0, "Outer", std::string("outer"), 1},
        TraceEvent{1, 1, 110, "cat", 'X', 0, 10, "InnerX", std::string("inner_x"), 1},
        TraceEvent{1, 1, 120, "cat", 'B', 0, 0, "InnerB", std::string("inner_b"), 1},
        TraceEvent{1, 1, 130, "cat", 'E', 0, 0, "InnerB", std::string("inner_b"), 1},
        TraceEvent{1, 1, 150, "cat", 'E', 0, 0, "Outer", std::string("outer"), 1},
        TraceEvent{1, 1, 200, "cat", 'X', 0, 5, "TopX", std::string("top_x"), 1},
    };

    EventDatabaseWriter writer(db_path, logger);
    writer.writeEvents(events, files);

    duckdb::DuckDB     db(db_path);
    duckdb::Connection con(db);

    EXPECT_EQ(scalar_i64(con, "SELECT COUNT(*) FROM event_nested"), 2);
    EXPECT_EQ(
        scalar_i64(
            con,
            "SELECT COUNT(*) FROM event_nested WHERE parent_id = 1 AND nested_id = 2"),
        1);
    EXPECT_EQ(
        scalar_i64(
            con,
            "SELECT COUNT(*) FROM event_nested WHERE parent_id = 1 AND nested_id = 3"),
        1);

    auto parent_res = con.Query(
        "SELECT event_id, parent_id FROM event_parent ORDER BY event_id");
    ASSERT_FALSE(parent_res->HasError()) << parent_res->GetError();
    ASSERT_EQ(parent_res->RowCount(), 6);

    EXPECT_TRUE(parent_res->GetValue(1, 0).IsNull());             // event 1
    EXPECT_EQ(parent_res->GetValue(1, 1).GetValue<int64_t>(), 1); // event 2 -> 1
    EXPECT_EQ(parent_res->GetValue(1, 2).GetValue<int64_t>(), 1); // event 3 -> 1
    EXPECT_TRUE(parent_res->GetValue(1, 3).IsNull());             // event 4 (E)
    EXPECT_TRUE(parent_res->GetValue(1, 4).IsNull());             // event 5 (E)
    EXPECT_TRUE(parent_res->GetValue(1, 5).IsNull());             // event 6 top-level X
}

TEST_F(EventDatabaseWriterFixture, DemanglesMangledDetailAndStoresJsonColumns) {
    std::vector<InputFile> files{InputFile{
        1, std::filesystem::absolute("/tmp/trace_3.json").lexically_normal().string()}};

    std::vector<TraceEvent> events{
        TraceEvent{
            1376898,
            1376898,
            116553114,
            "",
            'X',
            0,
            2030,
            "CoroSplitPass",
            std::string("(_ZNK3org3imm7ImmPath9pathSpansEb)"),
            1},
        TraceEvent{
            1376898,
            1376898,
            116553114,
            "",
            'X',
            0,
            2032,
            "PassManager<LazyCallGraph::SCC, AnalysisManager<LazyCallGraph::SCC, "
            "LazyCallGraph&>, LazyCallGraph&, CGSCCUpdateResult&>",
            std::string("(_ZNK3org3imm7ImmPath9pathSpansEb)"),
            1},
        TraceEvent{
            1376898,
            1376898,
            116591158,
            "",
            'X',
            0,
            1468,
            "CoroSplitPass",
            std::string("(_ZNK3org3imm11ImmAstStore7all_idsEv)"),
            1},
        TraceEvent{
            1376898,
            1376898,
            116591158,
            "",
            'X',
            0,
            1470,
            "PassManager<LazyCallGraph::SCC, AnalysisManager<LazyCallGraph::SCC, "
            "LazyCallGraph&>, LazyCallGraph&, CGSCCUpdateResult&>",
            std::string("(_ZNK3org3imm11ImmAstStore7all_idsEv)"),
            1},
    };

    EventDatabaseWriter writer(db_path, logger);
    writer.set_parsed_json = true;
    writer.writeEvents(events, files);

    duckdb::DuckDB     db(db_path);
    duckdb::Connection con(db);

    EXPECT_EQ(scalar_i64(con, "SELECT COUNT(*) FROM events"), 4);
    EXPECT_EQ(
        scalar_i64(con, "SELECT COUNT(*) FROM events WHERE normal_json IS NOT NULL"), 4);
    EXPECT_EQ(
        scalar_i64(con, "SELECT COUNT(*) FROM events WHERE parsed_json IS NOT NULL"), 4);

    EXPECT_EQ(
        scalar_str(
            con,
            "SELECT detail FROM events WHERE name = 'CoroSplitPass' ORDER BY ts LIMIT 1"),
        "(_ZNK3org3imm7ImmPath9pathSpansEb)");

    std::string normal_json = scalar_str(
        con,
        "SELECT normal_json FROM events WHERE name = 'CoroSplitPass' ORDER BY ts LIMIT "
        "1");
    EXPECT_NE(normal_json.find("ImmPath"), std::string::npos);
    EXPECT_NE(normal_json.find("pathSpans"), std::string::npos);

    EXPECT_GT(scalar_i64(con, "SELECT COUNT(*) FROM string_ids"), 0);
}


TEST_F(EventDatabaseWriterFixture, InstantiateFunctionWorksProperly) {
    std::vector<InputFile> files{InputFile{
        1, std::filesystem::absolute("/tmp/trace_4.json").lexically_normal().string()}};

    std::vector<TraceEvent> events{
        TraceEvent{
            1376898,
            1376898,
            116553114,
            "",
            'X',
            0,
            2030,
            "InstantiateFunction",
            std::string(
                "llvm::trailing_objects_internal::TrailingObjectsImpl<8, "
                "clang::FunctionProtoType, "
                "llvm::TrailingObjects<clang::FunctionProtoType, clang::QualType, "
                "clang::SourceLocation, clang::FunctionType::FunctionTypeExtraBitfields, "
                "clang::FunctionType::FunctionTypeExtraAttributeInfo, "
                "clang::FunctionType::FunctionTypeArmAttributes, "
                "clang::FunctionType::ExceptionType, clang::Expr *, clang::FunctionDecl "
                "*, clang::FunctionType::ExtParameterInfo, clang::Qualifiers, "
                "clang::FunctionEffect, clang::EffectConditionExpr>, clang::Qualifiers, "
                "clang::FunctionEffect, "
                "clang::EffectConditionExpr>::getTrailingObjectsImpl"),
            1},
    };

    EventDatabaseWriter writer(db_path, logger);
    writer.set_parsed_json = true;
    writer.writeEvents(events, files);

    duckdb::DuckDB     db(db_path);
    duckdb::Connection con(db);

    EXPECT_EQ(scalar_i64(con, "SELECT COUNT(*) FROM events"), 1);
    EXPECT_EQ(
        scalar_i64(con, "SELECT COUNT(*) FROM events WHERE normal_json IS NOT NULL"), 1);
    EXPECT_EQ(
        scalar_i64(con, "SELECT COUNT(*) FROM events WHERE parsed_json IS NOT NULL"), 1);
}
