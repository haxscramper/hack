#ifndef TEXTINPUT_H
#define TEXTINPUT_H

#include <QWidget>
#include <QPlainTextEdit>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE


class TextInput : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit TextInput(QWidget *parent = nullptr);
    void setUI(Ui::MainWindow * _ui);

private:
    Ui::MainWindow *ui;

signals:

public slots:

    // QWidget interface
protected:
    void keyPressEvent(QKeyEvent *event);
};

#endif // TEXTINPUT_H
