#ifndef MAINWINDOW_HPP
#define MAINWINDOW_HPP

#include <QMainWindow>

#include "QTOgreWindow.hpp"

class QStatusBar;
class QLabel;

class MainWindow : public QMainWindow
{
    Q_OBJECT

  public:
    explicit MainWindow(QWidget* parent = 0);
    ~MainWindow();

  private:
    QTOgreWindow* ogreWindow;
    struct StatusWidgets
    {
        QStatusBar* status;
        QLabel*     xPos;
        QLabel*     yPos;
        QLabel*     zPos;
    } statusWidgets;
};

#endif // MAINWINDOW_HPP
