#include "mainwindow.hpp"
#include <debugmacro/all.hpp>
#include <tuple>

const size_t name_col = 0;
const size_t deps_col = 1;
const size_t rest_col = 2;
const size_t type_col = 3;


void MainWindow::run_computations() {
    LOG << "Running computations";
    view->clear();

    for_i(row, input_tbl->rowCount()) {
        if ((qobject_cast<QComboBox*>(
                 input_tbl->cellWidget(row, type_col)))
                ->currentIndex()
            == 0) {
            LOG << "Adding assembly item";
            QString itemName = input_tbl->item(row, name_col)->text();
            view->addItem(itemName);
        } else {
            LOG << "Adding assembly step";
            QString stepName     = input_tbl->item(row, name_col)->text();
            QString targetName   = input_tbl->item(row, rest_col)->text();
            QStringList stepDeps = input_tbl->item(row, deps_col)
                                       ->text()
                                       .split(",");

            view->addStep(stepName, stepDeps, targetName);
        }
    }

try {
    view->update();
    } catch (std::exception e) {
LOG << e.what();
    } catch (...) {

    }
}

void MainWindow::populateTest() {
    // List of inputs, name of output, time to assemble and additional
    // dependecies
    std::vector<std::tuple<QStringList, QString, qreal, QStringList>>
        assembly = {
            // First  assembly
            {{"input1_1", "input2_1", "input3_1"}, "result1", 10, {}},
            // First  assembly
            {{"input1_2", "input2_2", "input3_2"}, "result2", 10, {}},
            // First assembly
            {{"input1_3", "input2_3", "input3_3"},
             "result3",
             10,
             {"result1", "result2"}}};

    size_t offset = 0;

    for (auto& step : assembly) {
        QStringList& input      = std::get<0>(step);
        int          processRow = offset + input.size();

        for_i(row, input.size()) {
            newDataRow();
            LOG << input[row];
            input_tbl->item(row + offset, name_col)->setText(input[row]);
        }

        newDataRow();

        QComboBox* typeSelector = qobject_cast<QComboBox*>(
            input_tbl->cellWidget(processRow, type_col));

        input_tbl->item(processRow, name_col)
            ->setText(QString("##") + std::get<1>(step));
        typeSelector->setCurrentIndex(processRow);
        typeSelector->activated(processRow);

        QStringList& additionalDeps = std::get<3>(step);

        input_tbl->item(processRow, rest_col)->setText(std::get<1>(step));
        input_tbl->item(processRow, deps_col)
            ->setText(
                input.join(",")
                + (additionalDeps.size() == 0
                       ? ""
                       : "," + additionalDeps.join(",")));

        newDataRow();

        input_tbl->item(processRow + 1, name_col)
            ->setText(std::get<1>(step));

        offset = processRow + 2;
    }
}

void MainWindow::newDataRow() {
    input_tbl->insertRow(input_tbl->rowCount());


    int lastRow = input_tbl->rowCount() - 1;

    for_i(col, 3) {
        input_tbl->setItem(lastRow, col, new QTableWidgetItem);
    }

    input_tbl->item(lastRow, rest_col)->setBackground(Qt::gray);
    input_tbl->item(lastRow, deps_col)->setBackground(Qt::gray);

    QComboBox* select = new QComboBox(input_tbl);
    select->addItem("Assembly item");
    select->addItem("Assembly step");

    select->setStyleSheet("QComboBox { background-color: green; }");
    select->setAutoFillBackground(true);


    input_tbl->resizeColumnToContents(type_col);
    connect(
        select,
        QOverload<int>::of(&QComboBox::activated),
        [lastRow, select, table = this->input_tbl.get()](int index) {
            if (index == 0) {
                select->setStyleSheet(
                    "QComboBox { background-color: green; }");
                table->item(lastRow, rest_col)->setBackground(Qt::gray);

                table->item(lastRow, deps_col)->setBackground(Qt::gray);
            } else {
                select->setStyleSheet(
                    "QComboBox { background-color: orange; }");
                table->item(lastRow, rest_col)->setBackground(Qt::white);

                table->item(lastRow, deps_col)->setBackground(Qt::white);
            }
        });

    input_tbl->setCellWidget(lastRow, type_col, select);
}

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent)
    , input_lyt(false)
    , input_wgt(true, this)
    , compute_pbtn(true, this)
    , view(true, this)
    , input_tbl(true, this)
    , output_tbl(true, this)
    , addRow_pbtn(true, this)
    , splitter(true, this)
//  ,
{

    setCentralWidget(splitter);
    splitter->addWidget(view);

    { // Input fields layout
        splitter->addWidget(input_wgt);

        input_wgt->setLayout(input_lyt);

        input_lyt->addWidget(input_tbl);
        input_lyt->addWidget(addRow_pbtn);
        // input_lyt->addWidget(output_tbl);
        input_lyt->addWidget(compute_pbtn);
        { // Input table layout
            input_tbl->setColumnCount(4);

            input_tbl->setColumnWidth(name_col, 100);
            input_tbl->setColumnWidth(deps_col, 100);
            input_tbl->setColumnWidth(rest_col, 100);


            input_tbl->setHorizontalHeaderLabels(
                {"Name", "Dependency", "Result", "Type"});

            addRow_pbtn->setText("Add row");
            compute_pbtn->setText("Compute");
        }
    }
    splitter->setSizes({600, 400});

    connect(addRow_pbtn, &QPushButton::clicked, [&]() {
        MainWindow::newDataRow();
    });

    connect(compute_pbtn, &QPushButton::clicked, [&]() {
        run_computations();
    });
}
