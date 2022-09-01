#include <QGuiApplication>
#include <QQmlApplicationEngine>
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QSqlError>
#include <QQmlContext>

#include "../../../cpp_common.hpp"

#include "qmllogger.hpp"
#include "list_model.hpp"

int main(int argc, char* argv[]) {
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
#endif
    QGuiApplication app(argc, argv);

    QQmlApplicationEngine engine;
    const QUrl            url(QStringLiteral("qrc:/main.qml"));

    QObject::connect(
        &engine,
        &QQmlApplicationEngine::objectCreated,
        &app,
        [url](QObject* obj, const QUrl& objUrl) {
            if (!obj && url == objUrl) { QCoreApplication::exit(-1); }
        },
        Qt::QueuedConnection);

    auto db     = QSqlDatabase::addDatabase("QSQLITE");
    auto dbpath = "/tmp/db.sqlite"; // PROJECT_PATH "database.tmp.db";
    qDebug() << "Reading db from" << dbpath;
    db.setDatabaseName(dbpath);

    if (!db.open()) {
        qWarning() << "Open failed" + db.lastError().text();
        exit(1);
    }

    QSqlQuery query;

    qDebug() << "Reading procnames from procs";
    if (!query.exec(
            "SELECT procid, procname, docstring, rettype FROM procs")) {
        qWarning() << "Query failed" << query.lastError().text();
        exit(1);
    }

    int                                          nodocCnt = 0;
    std::vector<std::pair<NimProc, std::string>> procs;
    while (query.next()) {
        NimProc res;
        res.id        = query.value(0).toInt();
        res.name      = query.value(1).toString();
        res.docstring = query.value(2).toString();
        res.rettype   = query.value(3).toString();

        if (res.docstring.length() < 2) { ++nodocCnt; }

        QSqlQuery argquery(
            QString("SELECT arg, type FROM arguments WHERE procid == %1")
                .arg(res.id));

        while (argquery.next()) {
            NimProcArg arg;
            arg.name = argquery.value(0).toString();
            arg.type = argquery.value(1).toString();
            res.args.push_back(arg);
        }

        procs.push_back({res, res.getSearchName().toStdString()});
    }

    Q_ASSERT(qmlRegisterType<QmlLogger>("QmlLogger", 1, 0, "Logger"));
    Q_ASSERT(qmlRegisterType<FuzzySearchProxyModel>(
        "Model", 1, 0, "ProxyModel"));

    ListItemModel         model;
    FuzzySearchProxyModel proxy;
    proxy.setSourceModel(&model);
    proxy.setDynamicSortFilter(false);
    engine.rootContext()->setContextProperty(
        "model", QVariant::fromValue(&proxy));

    engine.load(url);


    return app.exec();
}
