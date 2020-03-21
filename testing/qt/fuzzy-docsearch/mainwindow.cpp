#include "mainwindow.hpp"
#include <QDebug>
#include <QStyledItemDelegate>
#include <QtSql>
#include <fuzzywidget/fuzzysearchwidget.hpp>

#include "base_16_colors.hpp"
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
    QString                 docstring;
};

using ColoredStrings = std::vector<std::pair<QString, QColor>>;

void drawColored(
    const ColoredStrings& colored,
    QPainter*             painter,
    const QRectF&         textArea,
    QPointF               startOffset = QPointF()) {
    QPointF      lastStop = textArea.topLeft() + startOffset;
    QFontMetrics metric   = QFontMetrics(painter->font());
    for (auto& item : colored) {
        painter->setPen(item.second);
        painter->drawText(
            QRectF(lastStop, textArea.bottomRight()),
            Qt::AlignTop,
            item.first);

        lastStop = QPointF(
            lastStop.x() + metric.horizontalAdvance(item.first),
            lastStop.y());
    }
}

std::string getSearchName(NimProc proc) {
    std::string res;
    res += proc.name.toStdString();
    res += "(";
    for (auto& arg : proc.args) {
        res += arg.name.toStdString() + ": " + arg.type.toStdString()
               + ", ";
    }
    res += "):";
    res += proc.rettype.toStdString();

    return res;
}

ColoredStrings getDisplayName(NimProc data) {
    ColoredStrings proc;
    proc.push_back({data.name, Colors::getRed()});
    proc.push_back({"(", Colors::getGreen()});
    for (auto& arg : data.args) {
        proc.push_back({arg.name + ": ", Colors::getYellow()});
        proc.push_back({arg.type, Colors::getBlue()});
        proc.push_back({", ", Colors::getRed()});
    }
    proc.pop_back();
    proc.push_back({"):", Colors::getGreen()});
    proc.push_back({data.rettype, Colors::getYellow()});

    return proc;
}


std::vector<std::pair<NimProc, std::string>> procs;
class ProcDraw : public QStyledItemDelegate
{

    // QAbstractItemDelegate interface
    QFont topFont;
    QFont docFont;

    int topFontHeight;
    int docFontHeight;


  public:
    ProcDraw() {
        topFont       = QFont("JetBrains Mono", 12);
        docFont       = QFont("JetBrains Mono", 9);
        topFontHeight = QFontMetrics(topFont).height();
        docFontHeight = QFontMetrics(docFont).height();
    }

    void paint(
        QPainter*                   painter,
        const QStyleOptionViewItem& option,
        const QModelIndex&          index) const override {
        if (index.data().canConvert<ModelData>()) {
            let data = qvariant_cast<ModelData>(index.data());
            let proc = procs.at(data.dataIndex);


            painter->setFont(topFont);
            painter->drawText(option.rect, Qt::AlignVCenter, "Test");

            // Draw background
            painter->fillRect(option.rect, Colors::getBrightBlack());
            painter->fillRect(
                option.rect.marginsRemoved(QMargins(1, 1, 1, 1)),
                Colors::getBackground());


            auto textArea = option.rect.marginsRemoved(
                QMargins(2, 2, 2, 2));

            // Draw proc
            drawColored(getDisplayName(proc.first), painter, textArea);

            // Draw docstring
            auto docBox = QRectF(
                textArea.topLeft() + QPointF(0, topFontHeight),
                textArea.bottomRight());

            painter->setFont(docFont);
            painter->setPen(QPen(Colors::getYellow()));
            let lines = proc.first.docstring.split('\n');

            QString doc;
            for (int i = 0; i < std::min(lines.length(), 3); ++i) {
                doc += lines[i] + "\n";
            }

            painter->drawText(docBox, doc);
        }
    }

    QSize sizeHint(
        const QStyleOptionViewItem& option,
        const QModelIndex&          index) const override {
        if (index.data().canConvert<ModelData>()) {
            let data  = qvariant_cast<ModelData>(index.data());
            let proc  = procs.at(data.dataIndex);
            let lines = std::min(4, proc.first.docstring.count('\n') + 1);
            return QSize(120, 10 + lines * docFontHeight + topFontHeight);
        } else {
            return QSize(120, 80);
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
    if (!query.exec("SELECT procid, procname, docstring FROM procs")) {
        qDebug() << "Query failed" << query.lastError().text();
        return;
    }

    while (query.next()) {
        NimProc res;
        res.id        = query.value(0).toInt();
        res.name      = query.value(1).toString();
        res.docstring = query.value(2).toString();

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

        res.args = args;
        procs.push_back({res, getSearchName(res)});
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
