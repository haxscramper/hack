#pragma once

#include "assemblyview.hpp"
#include <QComboBox>
#include <QGraphicsView>
#include <QGraphicsWidget>
#include <QGridLayout>
#include <QMainWindow>
#include <QPushButton>
#include <QSplitter>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QWidget>
#include <algorithm/qwidget_cptr.hpp>


class MainWindow : public QMainWindow
{
    Q_OBJECT

  public:
    MainWindow(QWidget* parent = nullptr);
    void run_computations();

    void populateTest();

    void newDataRow();


  private:
    spt::qwidget_cptr<QVBoxLayout>  input_lyt;
    spt::qwidget_cptr<QWidget>      input_wgt;
    spt::qwidget_cptr<QTableWidget> input_tbl;

    spt::qwidget_cptr<AssemblyView> view;
    spt::qwidget_cptr<QTableWidget> output_tbl;
    spt::qwidget_cptr<QPushButton>  addRow_pbtn;
    spt::qwidget_cptr<QPushButton>  compute_pbtn;
    spt::qwidget_cptr<QSplitter>    splitter;
};
