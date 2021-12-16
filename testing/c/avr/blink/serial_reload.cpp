#include <QtCore/QDebug>
#include <QtCore/QElapsedTimer>
#include <QtCore/QString>
#include <QtCore/QThread>
#include <QtSerialPort/QSerialPort>
#include <QtSerialPort/QSerialPortInfo>

bool forceResetComPort(const QString& portName) {
    QSerialPort serial;
    serial.setPortName(portName);
    serial.setBaudRate(1200);

    qDebug() << "Forcing reset using 1200bps open/close on port "
             << portName;
    if (!serial.open(QIODevice::ReadWrite))
        return false;

    // This seems optional
    serial.setDataTerminalReady(false);

    qDebug() << "Waiting for the new upload port...";

    QElapsedTimer timeoutWatcher;
    qint64        timeoutMs     = 10000;
    bool          isPortPresent = true;

    const auto fetchIsPortPresent = [&]() -> bool {
        const auto ports = QSerialPortInfo::availablePorts();
        for (const auto& info : ports) {
            if (info.portName() == serial.portName()) {
                return true;
            }
        }
        return false;
    };

    timeoutWatcher.start();

    // Wait for port to disconnect
    while (isPortPresent && !timeoutWatcher.hasExpired(timeoutMs)) {
        isPortPresent = fetchIsPortPresent();

        if (isPortPresent) {
            QThread::msleep(1);
        }
    }

    serial.close();

    // Wait for port to reconnect
    while (!isPortPresent && !timeoutWatcher.hasExpired(timeoutMs)) {
        isPortPresent = fetchIsPortPresent();

        if (!isPortPresent)
            QThread::msleep(1);
    }

    return !timeoutWatcher.hasExpired(timeoutMs);
}
