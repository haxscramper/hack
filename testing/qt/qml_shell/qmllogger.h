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

  private:
};

#endif // QMLLOGGER_H
