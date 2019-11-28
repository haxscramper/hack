#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <iostream>

#define let const auto
#define var auto

void setMainFont(QPlainTextEdit* text,const QString& family = "Source Code Pro") {
    QFont font = text->document()->defaultFont();
    font.setFamily(family);
    font.setPointSize(12);
    text->document()->setDefaultFont(font);
}

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->textInput->setUI(ui);

    setMainFont(ui->textInput);
    setMainFont(ui->templateInput);
}

MainWindow::~MainWindow()
{
    delete ui;
}


void MainWindow::on_updatePreview_clicked()
{

}
