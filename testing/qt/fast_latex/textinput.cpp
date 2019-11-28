#include "textinput.h"
#include "ui_mainwindow.h"

TextInput::TextInput(QWidget *parent) : QPlainTextEdit(parent)
{

}

void TextInput::setUI(Ui::MainWindow *_ui){
    ui = _ui;
}

void TextInput::keyPressEvent(QKeyEvent *event)
{
    QPlainTextEdit::keyPressEvent(event);

    auto templateText = ui->templateInput->toPlainText();
    templateText.replace("<++++>", ui->textInput->toPlainText());

    ui->webPreview->setHtml(templateText);

    const QString qPath("result.html");
    QFile qFile(qPath);
    if (qFile.open(QIODevice::WriteOnly)) {
      QTextStream out(&qFile); out << templateText;
      qFile.close();
    }
}
