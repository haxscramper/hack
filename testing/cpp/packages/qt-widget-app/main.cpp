#include <QApplication>
#include <QMainWindow>

#include <libwidget/libwidget.hpp>

int main(int argc, char* argv[]) {
    QApplication app(argc, argv);
    QMainWindow  window;

    auto widget = new LibWidget();
    window.setCentralWidget(widget);

    window.show();
    return app.exec();
}
