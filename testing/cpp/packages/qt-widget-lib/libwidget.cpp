#include "libwidget.hpp"
#include <QPushButton>
#include <QVBoxLayout>

LibWidget::LibWidget(QWidget* parent) : QWidget(parent) {
    setLayout(new QVBoxLayout);
    layout()->addWidget(new QPushButton("hekksdfk"));
}
