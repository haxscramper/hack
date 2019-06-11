#include "assemblyview.hpp"
#include <debugmacro/all.hpp>
#include <support/misc.hpp>

#include "assemblyview_arrange.cpp"

static QRectF rectUnder(QRectF rect) {
    return QRectF(rect.bottomLeft(), rect.bottomRight() + QPointF(0, 30));
}

AssemblyView::AssemblyView(QWidget* parent)
    : QGraphicsView(parent)
    , scene(new QGraphicsScene(this))
//  ,
{

    setScene(scene.get());
    this->setDragMode(QGraphicsView::ScrollHandDrag);
}

void AssemblyView::addStep(
    QString     name,
    QStringList dependencies,
    QString     target) {
    INFO << "Added build step" << name;

    AssemblyStepUptr step = std::make_unique<AssemblyStep>();
    step->setNetworkData({target, dependencies});

    scene->addItem(step.get());

    assemblySteps[name] = std::move(step);
}


void AssemblyView::addItem(QString name) {
    AssemblyItemUptr item = std::make_unique<AssemblyItem>();

    item->setCaption(name);

    scene->addItem(item.get());

    assemblyItems[name] = std::move(item);
}

void AssemblyView::clear() {
    assemblyItems.clear();
    assemblySteps.clear();
    scene->clear();
}

static void connectLines(
    QGraphicsScene*                                scene,
    std::unordered_map<QString, AssemblyStepUptr>& assemblySteps,
    std::unordered_map<QString, AssemblyItemUptr>& assemblyItems

) {
    auto configureLine = [](QGraphicsLineItem* line) {
        line->setZValue(-1);
        QPen pen;
        pen.setWidth(2);
        line->setPen(pen);
    };


    for (std::pair<const QString, AssemblyStepUptr>& _step :
         assemblySteps) {
        AssemblyStep* step = _step.second.get();
        // Set all targets for all items
        for (AssemblyItem* dep : step->getDependencies()) {
            configureLine(scene->addLine(
                dep->getCenter().x(),
                dep->getCenter().y(),
                step->getCenter().x(),
                step->getCenter().y()));
        }

        // Set result for step
        AssemblyItem* res = assemblyItems[step->getNetworkData().first]
                                .get();
        step->setResult(res);

        configureLine(scene->addLine(
            res->getCenter().x(),
            res->getCenter().y(),
            step->getCenter().x(),
            step->getCenter().y()));
    }
}

static void buildDependencies(
    std::unordered_map<QString, AssemblyStepUptr>& assemblySteps,
    std::unordered_map<QString, AssemblyItemUptr>&
        assemblyItems) { // Build dependencies
    for (std::pair<const QString, AssemblyStepUptr>& _step :
         assemblySteps) {
        AssemblyStep* step = _step.second.get();
        INFO << "Dependencies of" << _step.first;
        // Set all targets for all items

        for (QString dep : step->getNetworkData().second) {
            LOG << dep;
            assemblyItems[dep]->setTarget(step);
            step->addDependency(assemblyItems[dep].get());
        }

        INFO << "Result:";
        LOG << step->getNetworkData().first;

        // Set result for step
        AssemblyItem* res = assemblyItems[step->getNetworkData().first]
                                .get();

        step->setResult(res);
        res->setSource(step);
    }
}

static void arrangeNodes(
    std::unordered_map<QString, AssemblyItemUptr>& assemblyItems) {

    // Arrange nodes in order
    AssemblyItem* final;
    { // Find final item (no steps depen on it)
        INFO << "Finding final item in the network";

        for (std::pair<const QString, AssemblyItemUptr>& item :
             assemblyItems) {
            LOG << item.first << "has target" << item.second->hasTarget();
            if (!item.second->hasTarget()) {
                INFO << "Setting" << item.first << "as final";
                final = item.second.get();
                break;
            }
        }
    }


    final->setCenter({xCenter, yStart});

    // Steps currently being arranged
    std::vector<AssemblyStep*> currentSteps = {final->getSource().value()};

    currentSteps.front()->setCenter({xCenter, yStart + levelSpacing});

    size_t buildLevel = 1;

    do {
        LOG << "Arranging items for level" << buildLevel;
        DEBUG_INDENT

        LOG << "Getting items list";
        std::vector<AssemblyItem*> levelItems;
        { // Get items for this level
            for (AssemblyStep* step : currentSteps) {
                LOG << "Getting dependencies of step"
                    << step->getAssemblyTime();
                spt::concatenate_copy(levelItems, step->getDependencies());
            }
        }


        position_build_items(levelItems, buildLevel);

        LOG << "Getting list of required steps";
        std::vector<AssemblyStep*> nextSteps = get_assembly_steps(
            levelItems);


        position_build_steps(nextSteps, buildLevel);

        currentSteps = std::move(nextSteps);
        ++buildLevel;
        DEBUG_DEINDENT
    } while (currentSteps.size() != 0);
}

void AssemblyView::update() {
    buildDependencies(assemblySteps, assemblyItems);
    arrangeNodes(assemblyItems);
    connectLines(scene.get(), assemblySteps, assemblyItems);
}


std::vector<AssemblyItem*> AssemblyStep::getDependencies() const {
    return dependencies;
}

void AssemblyStep::setResult(AssemblyItem* _result) {
    result = _result;
}

void AssemblyStep::addDependency(AssemblyItem* dependency) {
    dependencies.push_back(dependency);
}

QRectF AssemblyStep::boundingRect() const {
    float height = 50;
    float width  = 100;
    return QRectF(
        center.x() - width / 2, center.y() - height / 2, width, height);
}

void AssemblyStep::paint(
    QPainter*                                        painter,
    [[maybe_unused]] const QStyleOptionGraphicsItem* option,
    [[maybe_unused]] QWidget*                        widget) {

    QBrush brush;
    QPen   pen;

    {
        brush.setColor(Qt::white);
        pen.setColor(Qt::white);
        brush.setStyle(Qt::SolidPattern);

        painter->setBrush(brush);
        painter->setPen(pen);

        int margin = 20;

        painter->drawRect(
            boundingRect() + QMargins(margin, margin, margin, margin));
    }

    {
        pen.setWidth(2);
        brush.setStyle(Qt::Dense4Pattern);
        brush.setColor(QColor("orange"));
        pen.setColor(Qt::black);

        painter->setBrush(brush);
        painter->setPen(pen);

        painter->drawRect(boundingRect());
        painter->drawText(
            rectUnder(boundingRect()),
            Qt::AlignCenter,
            QString::number(assemblyTime));
    }
}

bool AssemblyItem::hasTarget() const {
    return target.has_value();
}

QRectF AssemblyItem::boundingRect() const {
    float height = 50;
    float width  = 50;
    return QRectF(
        center.x() - width / 2, center.y() - height / 2, width, height);
}

void AssemblyItem::paint(
    QPainter*                                        painter,
    [[maybe_unused]] const QStyleOptionGraphicsItem* option,
    [[maybe_unused]] QWidget*                        widget) {

    QBrush brush;
    QPen   pen;

    {
        brush.setColor(Qt::white);
        pen.setColor(Qt::white);
        brush.setStyle(Qt::SolidPattern);

        painter->setBrush(brush);
        painter->setPen(pen);

        int margin = 20;

        painter->drawEllipse(
            boundingRect() + QMargins(margin, margin, margin, margin));
    }

    {
        pen.setWidth(2);
        brush.setStyle(Qt::Dense4Pattern);
        brush.setColor(Qt::green);
        pen.setColor(Qt::black);

        painter->setBrush(brush);
        painter->setPen(pen);

        painter->drawEllipse(boundingRect());
        painter->drawText(
            rectUnder(boundingRect()), Qt::AlignCenter, caption);
    }
}
