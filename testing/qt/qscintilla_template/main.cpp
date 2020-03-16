#include <QApplication>
#include <QMainWindow>

#include <Qsci/qscilexercpp.h>
#include <Qsci/qsciscintilla.h>

int main(int argc, char* argv[]) {
    QApplication a(argc, argv);
    QMainWindow  w;

    auto edit = new QsciScintilla;

    // QScintilla Configuration
    {


    }


    w.setCentralWidget(edit);

    w.show();
    return a.exec();
}
