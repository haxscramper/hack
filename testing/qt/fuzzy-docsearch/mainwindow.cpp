#include "mainwindow.hpp"
#include <QDebug>
#include <QStyledItemDelegate>
#include <QtSql>
#include <fuzzywidget/fuzzysearchwidget.hpp>

#include <vector>

#define let const auto

struct NimProcArg {
    QString name;
    QString type;
};

struct NimProc {
    int                     id;
    QString                 name;
    QString                 rettype;
    std::vector<NimProcArg> args;
};

std::vector<std::pair<NimProc, std::string>> procs;


class ProcDraw : public QStyledItemDelegate
{

    // QAbstractItemDelegate interface
  public:
    void paint(
        QPainter*                   painter,
        const QStyleOptionViewItem& option,
        const QModelIndex&          index) const override {
        if (index.data().canConvert<ModelData>()) {
            qDebug() << "Recieved model data to draw";
            let  data = qvariant_cast<ModelData>(index.data());
            let& proc = procs.at(data.dataIndex);
            painter->fillRect(option.rect, QBrush("green"));
            painter->setPen(QPen("blue"));
            painter->drawText(option.rect, proc.first.name);
        }
    }
};


MainWindow::MainWindow(QWidget* parent) : QMainWindow(parent) {
    let fuzzy = new FuzzySearchWidget();
    setCentralWidget(fuzzy);

    auto db     = QSqlDatabase::addDatabase("QSQLITE");
    auto dbpath = PROJECT_PATH "database.tmp.db";
    qDebug() << "Reading db from" << dbpath;
    db.setDatabaseName(dbpath);

    if (!db.open()) {
        qDebug() << "Open failed" << db.lastError().text();
        return;
    }

    QSqlQuery query;

    qDebug() << "Reading procnames from procs";
    if (!query.exec("SELECT procid, procname FROM procs")) {
        qDebug() << "Query failed" << query.lastError().text();
        return;
    }

    while (query.next()) {
        NimProc res;
        res.id   = query.value(0).toInt();
        res.name = query.value(1).toString();

        QSqlQuery argquery(
            QString("SELECT arg, type FROM arguments WHERE procid == %1")
                .arg(res.id));

        std::vector<NimProcArg> args;
        while (argquery.next()) {
            NimProcArg res;
            res.name = argquery.value(0).toString();
            res.type = argquery.value(1).toString();
            args.push_back(res);
        }

        res.args            = args;
        std::string display = res.name.toStdString() + "(";
        for (let& arg : args) {
            display += arg.name.toStdString() + ": "
                       + arg.type.toStdString();
        }

        display += ")";

        procs.push_back({res, display});
    }

    std::vector<std::string> dict;
    dict.reserve(procs.size());
    for (let& proc : procs) {
        qDebug() << "Adding proc " << QString::fromStdString(proc.second);
        dict.push_back(proc.second);
    }

    fuzzy->setDictionary(dict);
    fuzzy->setItemDelegate(new ProcDraw());
}
