#include "mainwindow.hpp"
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include "lib.h"
#include <iostream>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    this->setCentralWidget(new QWidget);
    auto lyt = new QVBoxLayout(this);
    this->centralWidget()->setLayout(lyt);

    auto button = new QPushButton(this);
    auto label = new QLabel(this);

    lyt->addWidget(label);
    lyt->addWidget(button);

    label->setText("Default text");

    connect(button, &QPushButton::clicked, [label](){
       const char* result = exportedProc();
       label->setText(result);
    });

}

MainWindow::~MainWindow()
{
}

