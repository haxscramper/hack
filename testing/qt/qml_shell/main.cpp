#include <QGuiApplication>
#include <QQmlApplicationEngine>
#include "qmllogger.h"

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

    int typeId = qmlRegisterType<QmlLogger>("QmlLogger", 1, 0, "Logger");
    // if typeId is 0 => Error
    Q_ASSERT(typeId);

    engine.load(url);


    return app.exec();
}
