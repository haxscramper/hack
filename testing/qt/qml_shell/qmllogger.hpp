#ifndef QMLLOGGER_H
#define QMLLOGGER_H

#include <QQuickItem>

class QmlLogger : public QQuickItem {
    Q_OBJECT
  public:
    explicit QmlLogger(QQuickItem* iParent = 0);

    // Q_INVOKABLE log method will be called by Qml source.
    Q_INVOKABLE void log(unsigned int iLogLevel, const QString& iDataToLog)
        const;


    enum Level { Error = 0, Warning, Info, Debug, Trace };
    Q_ENUM(Level)


    Q_INVOKABLE inline void debug(const QString& msg) { log(Debug, msg); }
    Q_INVOKABLE inline void warning(const QString& msg) {
        log(Warning, msg);
    }
    Q_INVOKABLE inline void info(const QString& msg) { log(Info, msg); }
    Q_INVOKABLE inline void trace(const QString& msg) { log(Trace, msg); }
    Q_INVOKABLE inline void error(const QString& msg) { log(Error, msg); }

  private:
};

#endif // QMLLOGGER_H
