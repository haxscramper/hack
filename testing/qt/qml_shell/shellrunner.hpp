#ifndef SHELLRUNNER_H
#define SHELLRUNNER_H

#include <QObject>

class ShellRunner : public QObject
{
    Q_OBJECT
public:
    explicit ShellRunner(QObject *parent = nullptr);

signals:

};

#endif // SHELLRUNNER_H
