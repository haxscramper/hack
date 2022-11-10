#include "fuzzysearchwidget.hpp"
#include "textinputfield.hpp"

#include <QVBoxLayout>
#include <QElapsedTimer>

QColor DraculaColors::background  = QColor(40, 42, 54);
QColor DraculaColors::currentLine = QColor(68, 71, 90);
QColor DraculaColors::selection   = QColor(68, 71, 90);
QColor DraculaColors::foreground  = QColor(248, 248, 242);
QColor DraculaColors::comment     = QColor(98, 114, 164);
QColor DraculaColors::cyan        = QColor(139, 233, 253);
QColor DraculaColors::green       = QColor(80, 250, 123);
QColor DraculaColors::orange      = QColor(255, 184, 108);
QColor DraculaColors::pink        = QColor(255, 121, 198);
QColor DraculaColors::purple      = QColor(189, 147, 249);
QColor DraculaColors::red         = QColor(255, 85, 85);
QColor DraculaColors::yellow      = QColor(241, 250, 140);


FuzzySearchWidget::FuzzySearchWidget(QWidget* parent) : QWidget(parent) {
    auto lyt = new QVBoxLayout();


    auto input = new TextInputField();

    input->setMaximumHeight(120);

    lyt->addWidget(input);

    view = new QListView();


    proxy = new FuzzySearchProxyModel();
    list  = new ListItemModel();

    proxy->setSourceModel(list);
    view->setModel(proxy);

    proxy->setDynamicSortFilter(false);

    lyt->addWidget(view);

    connect(input, &QLineEdit::textChanged, [this](const QString& text) {
        this->setPattern(text);
    });

    connect(
        input,
        &TextInputField::ctrlEnterPressed,
        this,
        &FuzzySearchWidget::sortOnPattern);

    setLayout(lyt);
}

void FuzzySearchWidget::setDictionary(const QVector<QString>& dict) {
    list->setItems(dict);
}

void FuzzySearchWidget::setAutoSortThreshold(int threshold) {
    maxDictSize = threshold;
}

void FuzzySearchWidget::setItemDelegate(QStyledItemDelegate* delegate) {
    view->setItemDelegate(delegate);
}

int FuzzySearchWidget::size() const {
    return list->rowCount(list->index(0, 0));
}

void FuzzySearchWidget::setPattern(const QString& _patt) {
    pattern = _patt;
    if (size() < maxDictSize) {
        sortOnPattern();
    }
}

inline QElapsedTimer make_timer() {
    QElapsedTimer res;
    res.start();
    return res;
}


void FuzzySearchWidget::sortOnPattern() {
    auto timer = make_timer();
    proxy->updateScores(pattern);
    const auto score_time = timer.nsecsElapsed();
    emit       scoreUpdateCompleted(score_time);
    timer.restart();
    proxy->sort(0);

    const auto sort_time = timer.nsecsElapsed();
    emit       sortCompleted(sort_time);
}

void FuzzySearchWidget::setMaxItemShowed(int value) {
    proxy->setMaxItemShowed(value);
}

void FuzzySearchProxyModel::setMaxItemShowed(int value) {
    maxItemShowed = value;
}
