#pragma once

#include "class_macro.hpp"
#include <QGraphicsItem>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QWidget>
#include <algorithm/qwidget_cptr.hpp>
#include <memory>
#include <optional>
#include <unordered_map>

#include <functional>

namespace std {
template <>
struct hash<QString> {
    std::size_t operator()(const QString& s) const {
        return qHash(s);
    }
};
} // namespace std

class AssemblyItem;

class AssemblyStep : public QGraphicsItem
{
    DEFINE_MEMBER_GET_SET(float, assemblyTime, AssemblyTime);
    DEFINE_MEMBER_GET_SET(QPointF, center, Center);

    AssemblyItem*              result;
    std::vector<AssemblyItem*> dependencies;
    std::vector<AssemblyItem*> getDependencies() const;


    using BaseData = std::pair<QString, QStringList>;

    DEFINE_MEMBER_GET_SET(BaseData, networkData, NetworkData);

  public:
    void setResult(AssemblyItem* _result);
    void addDependency(AssemblyItem* dependency);


    // QGraphicsItem interface
  public:
    QRectF boundingRect() const;
    void   paint(
          QPainter*                       painter,
          const QStyleOptionGraphicsItem* option,
          QWidget*                        widget);
};

using AssemblyStepUptr = std::unique_ptr<AssemblyStep>;

class AssemblyItem : public QGraphicsItem
{
  private:
    DEFINE_MEMBER_GET_SET(std::optional<AssemblyStep*>, target, Target);
    DEFINE_MEMBER_GET_SET(std::optional<AssemblyStep*>, source, Source);
    DEFINE_MEMBER_GET_SET(QString, caption, Caption);
    DEFINE_MEMBER_GET_SET(QPointF, center, Center);

  public:
    bool hasTarget() const;


    // QGraphicsItem interface
  public:
    QRectF boundingRect() const;
    void   paint(
          QPainter*                       painter,
          const QStyleOptionGraphicsItem* option,
          QWidget*                        widget);
};

using AssemblyItemUptr = std::unique_ptr<AssemblyItem>;

class AssemblyView : public QGraphicsView
{
  public:
    AssemblyView(QWidget* parent);

    void addStep(QString name, QStringList dependencies, QString target);
    void addItem(QString name);

    void clear();
    void update();

  private:
    const std::unique_ptr<QGraphicsScene>         scene;
    std::unordered_map<QString, AssemblyStepUptr> assemblySteps;
    std::unordered_map<QString, AssemblyItemUptr> assemblyItems;
};
