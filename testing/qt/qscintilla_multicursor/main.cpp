#include <QApplication>
#include <QMainWindow>

#include <Qsci/qscilexercpp.h>
#include <Qsci/qsciscintilla.h>

int main(int argc, char* argv[]) {
    QApplication a(argc, argv);
    QMainWindow  w;

    auto edit = new QsciScintilla;

    // QScintilla
    {

        edit->setText(
            "test string\n with some newlines\ngood for multiple cursors");

        edit->SendScintilla(
            QsciScintilla::SCI_SETADDITIONALSELECTIONTYPING, 1);

        for (int line = 0; line < 3; ++line) {
            auto offset = edit->positionFromLineIndex(line, 2);
            edit->SendScintilla(
                QsciScintilla::SCI_ADDSELECTION, offset, offset);
        }
    }


    w.setCentralWidget(edit);

    w.show();
    return a.exec();
}
