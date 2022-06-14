#include "delayed_predicate_interpreter.hpp"
#include <QCoreApplication>

int main(int argc, char* argv[]) {
    // Main application is necessary to create event processing loop at the
    // widget recieving side
    QCoreApplication a(argc, argv);
    // qDebug fixes, IDK it does not work by default
    qputenv("QT_ASSUME_STDERR_HAS_CONSOLE", "1");
    // Register metatype for interthread passing. Another IDK -
    // `Q_DECLARE_METATYPE` does not work properly.
    qRegisterMetaType<EvalRequest>();
    qRegisterMetaType<EvalResponse>();
    // Main wizard object
    Wizard app;
    // Interpreter instance
    AbstractInterpreter inter;
    // Send results to interpreter for evaluation
    QObject::connect(
        &app,
        &Wizard::requestEvaluation,
        &inter,
        &AbstractInterpreter::enqueue);

    // And process optional responses back again
    QObject::connect(
        &inter,
        &AbstractInterpreter::evaluationDone,
        &app,
        &Wizard::evaluationDone);

    // Kickstart evaluation thread
    inter.start();

    // This was necessary until I added the core application - current code
    // works without it, but I'm leaving it here for the sake of
    // completeness
    //
    // inter.enqueue(EvalRequest{});

    qDebug() << "[-] ----";

    // First piece of the user code to be processed, this one should be
    // accepted
    qDebug() << "[U] Changing user data first time";
    app.widget.userChange(QVariant("New user data"));
    qDebug() << "[-] ----";

    // Second entry, it should be rejected
    qDebug() << "[U] Second user data change";
    app.widget.userChange(QVariant("Other user data"));
    qDebug() << "[-] ----";

    QThread::sleep(2);
    inter.setFinishEvaluation(true);

    return a.exec();
}
