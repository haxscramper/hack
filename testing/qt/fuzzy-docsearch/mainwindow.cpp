#include "mainwindow.hpp"
#include <fuzzywidget/fuzzysearchwidget.hpp>
#include <libtest-n12/header.hpp>

MainWindow::MainWindow(QWidget* parent) : QMainWindow(parent) {
    setCentralWidget(new FuzzySearchWidget());
    printNumber(12);
}

MainWindow::~MainWindow() {
}
