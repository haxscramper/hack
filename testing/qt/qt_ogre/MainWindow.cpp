#include <QLabel>
#include <QDebug>
#include <QStatusBar>

#include "MainWindow.hpp"
#include "QTOgreWindow.hpp"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent)
    , statusWidgets{
          .status = new QStatusBar(),
          .xPos   = new QLabel(),
          .yPos   = new QLabel(),
          .zPos   = new QLabel(),
      } {
    setStatusBar(statusWidgets.status);

    statusWidgets.status->addWidget(new QLabel("x: "));
    statusWidgets.status->addWidget(statusWidgets.xPos);
    statusWidgets.status->addWidget(new QLabel("y: "));
    statusWidgets.status->addWidget(statusWidgets.yPos);
    statusWidgets.status->addWidget(new QLabel("z: "));
    statusWidgets.status->addWidget(statusWidgets.zPos);

    ogreWindow                  = new QTOgreWindow();
    QWidget* renderingContainer = QWidget::createWindowContainer(
        ogreWindow);

    Q_ASSERT(connect(
        ogreWindow, &QTOgreWindow::initializationComplete, [this]() {
            auto manager = ogreWindow->getCameraManager();

            Q_CHECK_PTR(manager);
            Q_ASSERT(QObject::connect(
                manager,
                &OgreQtBites::QtCameraController::cameraPositionChanged,
                [this](float x, float y, float z) {
                    statusWidgets.xPos->setText(QString::number(x));
                    statusWidgets.yPos->setText(QString::number(y));
                    statusWidgets.zPos->setText(QString::number(z));
                }));
        }));


    resize(600, 600);

    this->setCentralWidget(renderingContainer);
}

MainWindow::~MainWindow() { delete ogreWindow; }
