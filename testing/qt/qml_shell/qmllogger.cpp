#include "qmllogger.hpp"
#include <QDebug>
#include <iostream>

QmlLogger::QmlLogger(QQuickItem* iParent) : QQuickItem{iParent} {}

// Implementation of log method callable from Qml source
void QmlLogger::log(unsigned int iLogLevel, const QString& iDataToLog)
    const {
    switch (iLogLevel) {
        case Error: // ERROR
            std::cerr << "ERR: " << iDataToLog.toStdString() << std::endl;
            break;
        case Warning: // WARNING
            std::cout << "WARN: " << iDataToLog.toStdString() << std::endl;
            break;
        case Info: // INFO
            std::cout << "INFO: " << iDataToLog.toStdString() << std::endl;
            break;
        case Debug: // DEBUG
            std::cout << "DBG: " << iDataToLog.toStdString() << std::endl;
            break;
        case Trace: // TRACE
            std::cout << "TRACE: " << iDataToLog.toStdString()
                      << std::endl;
            break;
    }
}
