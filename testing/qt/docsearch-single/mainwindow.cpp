#include "mainwindow.hpp"
#include "fuzzysearchwidget.hpp"
#include <QDebug>
#include <QStyledItemDelegate>
#include <QtSql>

#include "base_16_colors.hpp"
#include <QLabel>
#include <QVBoxLayout>
#include <fstream>
#include <vector>

struct NimProcArg
{
    QString name;
    QString type;
};

struct NimProc
{
    int                 id;
    QString             name;
    QString             rettype;
    QVector<NimProcArg> args;
    QString             docstring;
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

QString getSearchName(NimProc proc) {
    QString res;
    res += proc.name;
    res += "(";
    for (auto& arg : proc.args) {
        res += arg.name + ": " + arg.type + ", ";
    }
    res += "): " + proc.rettype;

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
    proc.push_back({"): ", Colors::getGreen()});
    proc.push_back({data.rettype, Colors::getWhite()});

    return proc;
}

QVector<QPair<NimProc, QString>> procs;
class ProcDraw : public QStyledItemDelegate
{

    // QAbstractItemDelegate interface
    QFont topFont;
    QFont docFont;

    int topFontHeight;
    int docFontHeight;

  public:
    ProcDraw() {
        topFont       = QFont("consolas", 12);
        docFont       = QFont("consolas", 9);
        topFontHeight = QFontMetrics(topFont).height();
        docFontHeight = QFontMetrics(docFont).height();
    }

    void paint(
        QPainter*                   painter,
        const QStyleOptionViewItem& option,
        const QModelIndex&          index) const override {
        if (index.data().canConvert<ModelData>()) {
            const auto data = qvariant_cast<ModelData>(index.data());
            const auto proc = procs.at(data.dataIndex);

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
            const auto lines = proc.first.docstring.split('\n');

            QString doc;
            for (int i = 0; i < std::min(lines.length(), 3); ++i) {
                doc += lines[i] + "\n";
            }

            painter->drawText(docBox, doc);
        } else {
            painter->drawText(option.rect, "error converting data");
        }
    }

    QSize sizeHint(const QStyleOptionViewItem&, const QModelIndex& index)
        const override {
        if (index.data().canConvert<ModelData>()) {
            const auto data  = qvariant_cast<ModelData>(index.data());
            const auto proc  = procs.at(data.dataIndex);
            const auto lines = std::min(
                4, proc.first.docstring.count('\n') + 1);
            return QSize(120, 10 + lines * docFontHeight + topFontHeight);
        } else {
            return QSize(120, 80);
        }
    }
};

MainWindow::MainWindow(QWidget* parent) : QMainWindow(parent) {
    const auto fuzzy = new FuzzySearchWidget();
    const auto lyt   = new QVBoxLayout();
    const auto warn  = new QLabel();

    {
        warnlbl = new QLabel();
        QFont font;
        font.setFamily("JetBrains Mono");
        warnlbl->setFont(font);
        warnlbl->setStyleSheet(
            "QLabel { background-color : rgba(100%, 0%, 0%, 60%); }");
        warnlbl->setAlignment(Qt::AlignCenter);
        warnlbl->setMinimumHeight(36);

        lyt->addWidget(warnlbl);

        connect(
            fuzzy,
            &FuzzySearchWidget::sortCompleted,
            [fuzzy, this](float time) {
                warnlbl->setText(
                    QString("Sorted list of %1 items in %3 msec")
                        .arg(fuzzy->size())
                        .arg(time / 1000000.0));
            });
    }

    QFont font;
    font.setFamily("JetBrains Mono");
    warn->setFont(font);
    warn->setStyleSheet(
        "QLabel { background-color : rgba(100%, 0%, 0%, 60%); }");
    warn->setAlignment(Qt::AlignCenter);
    warn->setMaximumHeight(36);

    lyt->addWidget(warn);
    lyt->addWidget(fuzzy);

    setCentralWidget(new QWidget);
    centralWidget()->setLayout(lyt);
    fuzzy->setItemDelegate(new ProcDraw());

    auto install_fallback = [&]() {
        procs.push_back(
            {NimProc{0, "test", "RETT", {}, "docstring"}, "test"});
        procs.push_back(
            {NimProc{0, "on_failure", "RETSDt", {}, "docstring"},
             "on_failure"});
        procs.push_back(
            {NimProc{0, "test2", "RETSDt", {}, "docstring"}, "test2"});

        QVector<QString> dict;


        for (const auto& it : procs) {
            dict.push_back(it.second);
        }

        fuzzy->setDictionary(dict);
    };

    auto db     = QSqlDatabase::addDatabase("QSQLITE");
    auto dbpath = "/tmp/db.sqlite"; // PROJECT_PATH "database.tmp.db";
    qDebug() << "Reading db from" << dbpath;
    db.setDatabaseName(dbpath);

    if (!db.open()) {
        qDebug() << "Open failed" << db.lastError().text();
        install_fallback();
        return;
    }

    QSqlQuery query;

    qDebug() << "Reading procnames from procs";
    if (!query.exec(
            "SELECT procid, procname, docstring, rettype FROM procs")) {
        qDebug() << "Query failed" << query.lastError().text();
        install_fallback();
        return;
    }

    int nodocCnt = 0;
    while (query.next()) {
        NimProc res;
        res.id        = query.value(0).toInt();
        res.name      = query.value(1).toString();
        res.docstring = query.value(2).toString();
        res.rettype   = query.value(3).toString();

        if (res.docstring.length() < 2) {
            ++nodocCnt;
        }

        QSqlQuery argquery(
            QString("SELECT arg, type FROM arguments WHERE procid == %1")
                .arg(res.id));

        QVector<NimProcArg> args;
        while (argquery.next()) {
            NimProcArg res;
            res.name = argquery.value(0).toString();
            res.type = argquery.value(1).toString();
            args.push_back(res);
        }

        res.args = args;
        procs.push_back({res, getSearchName(res)});
    }

    std::ofstream ofile("/tmp/thefile.txt");

    QVector<QString> dict;
    dict.reserve(procs.size());
    for (const auto& proc : procs) {
        const auto qname = proc.second;
        if (qname.contains("exec")) {
            qDebug() << qname;
        }
        ofile << proc.second.toStdString() << "\n";
        dict.push_back(proc.second);
    }

    warn->setText(
        QString("Out of %1 procs %2 are missing documentation (%3%)")
            .arg(procs.size())
            .arg(nodocCnt)
            .arg(100.0 * nodocCnt / procs.size()));


    fuzzy->setDictionary(dict);
}
