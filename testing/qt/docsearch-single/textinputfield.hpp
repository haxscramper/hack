#ifndef TEXTINPUTFIELD_HPP
#define TEXTINPUTFIELD_HPP

#include <QKeyEvent>
#include <QLineEdit>

class TextInputField : public QLineEdit
{
    Q_OBJECT
  signals:
    void ctrlEnterPressed(QString text);

  public:
    TextInputField(QWidget* parent = nullptr) : QLineEdit(parent) {
    }

    // QWidget interface
  protected:
    void keyPressEvent(QKeyEvent* event) override {
        if ((event->modifiers() == Qt::ControlModifier)
            && (event->key() == Qt::Key::Key_Return)) {
            emit ctrlEnterPressed(this->text());
        } else {
            QLineEdit::keyPressEvent(event);
        }
    }
};

#endif // TEXTINPUTFIELD_HPP
