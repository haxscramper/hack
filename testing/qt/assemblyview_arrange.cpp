#include "assemblyview.hpp"
#include <debugmacro/all.hpp>
#include <support/misc.hpp>

const float xCenter      = 400;
const float yStart       = 300;
const float itemSpacing  = 100;
const float levelSpacing = 120;
const float stepsSpacing = 130;


static void position_build_items(
    std::vector<AssemblyItem*>& levelItems,
    size_t                      buildLevel) { // Arrange items
    LOG << "Arranging items";
    DEBUG_INDENT
    size_t farLeft = xCenter - levelItems.size() * itemSpacing / 2;
    size_t yPos    = yStart + (2 * buildLevel) * levelSpacing;
    LOG << "Placing items at y:" << yPos;


    for_i(idx, levelItems.size()) {
        auto   dep  = levelItems[idx];
        size_t xPos = farLeft + itemSpacing * idx;
        LOG << dep->getCaption() << "x:" << xPos;
        dep->setCenter(QPointF(xPos, yPos));
    }

    DEBUG_DEINDENT
}

static void position_build_steps(
    std::vector<AssemblyStep*>& nextSteps,
    size_t                      buildLevel) { // Arrange steps
    LOG << "Arranging steps";
    DEBUG_INDENT

    size_t farLeft = xCenter - nextSteps.size() * stepsSpacing / 2;
    size_t idx     = 0;

    size_t yPos = yStart + (2 * buildLevel + 1) * levelSpacing;
    LOG << "Placing steps at y:" << yPos;

    for (AssemblyStep* step : nextSteps) {
        size_t xPos = farLeft + idx * stepsSpacing;
        LOG << "Step x:" << xPos;
        step->setCenter(QPointF(xPos, yPos));
        ++idx;
    }
    DEBUG_DEINDENT
}

static std::vector<AssemblyStep*> get_assembly_steps(
    std::vector<AssemblyItem*>& levelItems) {
    // Get list of required steps
    DEBUG_INDENT
    std::vector<AssemblyStep*> nextSteps;
    LOG << "Items on level" << levelItems.size();

    nextSteps.reserve(levelItems.size());


    std::transform(
        levelItems.begin(),
        levelItems.end(),
        std::back_inserter(nextSteps),
        [](AssemblyItem* item) -> AssemblyStep* {
            LOG << item->getCaption() << "has source"
                << item->getSource().has_value();
            return item->getSource().value_or(nullptr);
        });

    LOG << "Steps: " << nextSteps.size();

    INFO << "Erasing empty steps";

    nextSteps.erase(
        std::remove_if(
            nextSteps.begin(),
            nextSteps.end(),
            [](AssemblyStep* item) -> bool {
                LOG << (item == nullptr);
                return item == nullptr;
            }),
        nextSteps.end());

    LOG << "Steps: " << nextSteps.size();


    DEBUG_DEINDENT

    return nextSteps;
}
