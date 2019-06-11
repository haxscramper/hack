#!/usr/bin/env python
from __future__ import annotations

import sys
from typing import *

isGuiMode: Bool = True

from PySide2.QtWidgets import (
    QApplication,
    QLabel,
    QGraphicsScene,
    QGraphicsView,
    QGraphicsLineItem,
    QGraphicsItem,
    QMainWindow,
    QTableWidget,
    QPushButton,
    QSplitter,
    QWidget,
    QVBoxLayout,
    QComboBox,
    QTableWidgetItem,
    QFrame,
    QStyleOptionGraphicsItem,
)

from PySide2.QtGui import (
    QPainter,
    QBrush,
    QPen,
    QColor,
)

from PySide2.QtCore import (
    QMarginsF,
    QPointF,
    Qt,
    QRectF,
)

xCenter: int = 1000
yStart: int = 300
itemSpacing: int = 100
levelSpacing: int = 120
stepsSpacing: int = 130

name_col: int = 0
deps_col: int = 1
rest_col: int = 2
type_col: int = 3

__logIndent: int = 0


def log(*argv) -> None:
    if __logIndent != 0:
        print(" " * 4 * __logIndent, *argv)
    else:
        print(*argv)


def logup(*argv) -> None:
    log(*argv)
    global __logIndent
    __logIndent = __logIndent + 1


def logdown() -> None:
    global __logIndent
    __logIndent = __logIndent - 1


def widenRect(rect: QRectF, margin: float) -> QRectF:
    return rect + QMarginsF(margin, margin, margin, margin)


def rectUnder(rect: QRectF) -> QRectF:
    return QRectF(rect.bottomLeft(), rect.bottomRight() + QPointF(0, 30))


class NetworkData:
    targetName: str = ""
    depsName: List[str] = []

    def __init__(self) -> None:
        pass


class AssemblyStep(QGraphicsItem):
    def __init__(self) -> None:
        QGraphicsItem.__init__(self)

        self.assemblyTime: int = 0
        self.center: QPointF = QPointF()
        self.result: AssemblyItem = AssemblyItem()
        self.dependencies: List[AssemblyItem] = []
        self.networkData: NetworkData = NetworkData()

    def addDependency(self, dependency: AssemblyItem) -> None:
        self.dependencies = self.dependencies + [dependency]

    def boundingRect(self) -> QRectF:
        height: int = 50
        width: int = 100

        return QRectF(self.center.x() - width / 2,
                      self.center.y() - height / 2, width, height)

    def paint(self, painter: QPainter, option: QStyleOptionGraphicsItem,
              widget: QWidget) -> None:
        brush = QBrush()
        pen = QPen()

        # Draw background white rectangle
        brush.setColor(Qt.white)
        pen.setColor(Qt.white)
        brush.setStyle(Qt.SolidPattern)

        painter.setBrush(brush)
        painter.setPen(pen)

        painter.drawRect(widenRect(self.boundingRect(), 5))

        # Draw build step itself
        pen.setWidth(2)

        brush.setStyle(Qt.SolidPattern)
        brush.setColor(QColor("orange"))
        pen.setColor(Qt.black)

        painter.setBrush(brush)
        painter.setPen(pen)

        painter.drawRect(self.boundingRect())
        painter.drawText(rectUnder(self.boundingRect()), Qt.AlignCenter,
                         str(self.assemblyTime))


class AssemblyItem(QGraphicsItem):
    def __init__(self) -> None:
        QGraphicsItem.__init__(self)

        self.target: Optional[AssemblyStep] = None
        self.source: Optional[AssemblyStep] = None
        self.caption: str = ""
        self.center: QPointF = QPointF()

    def boundingRect(self) -> QRectF:
        height: float = 50
        width: float = 50

        return QRectF(self.center.x() - width / 2,
                      self.center.y() - height / 2, width, height)

    def hasTarget(self) -> bool:
        return self.target != None

    def paint(self, painter: QPainter, option: QStyleOptionGraphicsItem,
              widget: QWidget) -> None:
        brush = QBrush()
        pen = QPen()

        brush.setColor(Qt.white)
        pen.setColor(Qt.white)
        brush.setStyle(Qt.SolidPattern)

        painter.setBrush(brush)
        painter.setPen(pen)

        boundRect: QRectF = self.boundingRect()

        painter.drawEllipse(widenRect(self.boundingRect(), 5))

        pen.setWidth(2)
        brush.setStyle(Qt.SolidPattern)
        brush.setColor(Qt.green)
        pen.setColor(Qt.black)

        painter.setBrush(brush)
        painter.setPen(pen)

        painter.drawEllipse(boundRect)
        rectUnder: QRectF = QRectF(boundRect.bottomLeft(),
                                   boundRect.bottomRight() + QPointF(0, 30))

        painter.drawText(rectUnder, Qt.AlignCenter, self.caption)


def connectLines(scene: QGraphicsScene,
                 assemblySteps: MutableMapping[str, AssemblyStep],
                 assemblyItems: MutableMapping[str, AssemblyItem]) -> None:
    def configureLine(line: QGraphicsLineItem) -> None:
        pass

    for stepName, step in assemblySteps.items():
        for dep in step.dependencies:
            configureLine(
                scene.addLine(dep.center.x(), dep.center.y(), step.center.x(),
                              step.center.y()))

        res: AssemblyItem = assemblyItems[step.networkData.targetName]
        step.resutl = res

        configureLine(
            scene.addLine(res.center.x(),
                          res.center.y(),
                          step.center.x(),
                          step.center.y()))


def getAssemblySteps(levelItems: List[AssemblyItem]) -> List[AssemblyStep]:
    return [item.source for item in levelItems if item.source is not None]


def positionBuildItems(levelItems: List[AssemblyItem],
                       buildLevel: int) -> None:
    farLeft: int = xCenter - len(levelItems) * int(itemSpacing / 2)
    yPos: int = yStart + (2 * buildLevel) * levelSpacing

    logup("Position build items")

    for idx in range(0, len(levelItems)):
        dep: AssemblyItem = levelItems[idx]
        xPos: int = farLeft + itemSpacing * idx
        dep.center = QPointF(xPos, yPos)
        log("X:", xPos, "Y:", yPos)

    logdown()


def positionBuildSteps(nextSteps: List[AssemblyStep], buildLevel: int) -> None:
    idx: int = 0

    farLeft: int = xCenter - len(nextSteps) * int(stepsSpacing / 2)
    yPos: int = yStart + (2 * buildLevel + 1) * levelSpacing

    logup("Position build steps")
    log("farLeft:", farLeft, "yPos:", yPos)

    for step in nextSteps:
        xPos: int = farLeft + idx * stepsSpacing
        step.center = QPointF(xPos, yPos)
        log("X:", xPos, "Y:", yPos, "idx:", idx)
        idx = idx + 1

    logdown()


def buildDependencies(assemblySteps: MutableMapping[str, AssemblyStep],
                      assemblyItems: MutableMapping[str, AssemblyItem]
                      ) -> None:
    for stepName, step in assemblySteps.items():
        dependecies: List[str] = step.networkData.depsName

        for dep in dependecies:
            assemblyItems[dep].target = step
            step.addDependency(assemblyItems[dep])

        res = assemblyItems[step.networkData.targetName]

        step.result = assemblyItems[step.networkData.targetName]
        res.source = step


def arrangeNodes(assemblyItems: Mapping[str, AssemblyItem]) -> None:
    logup("Arraning nodes")
    final: AssemblyItem

    for itemName, item in assemblyItems.items():
        if not item.hasTarget():
            final = item
            break

    final.center = QPointF(xCenter, yStart)

    if final.source is not None:
        currentSteps: List[AssemblyStep] = [final.source]

    currentSteps[0].center = QPointF(xCenter, yStart + levelSpacing)
    buildLevel: int = 1

    while len(currentSteps) != 0:
        log("Arranging nodes on level", buildLevel)

        levelItems: List[AssemblyItem] = []

        for step in currentSteps:
            levelItems = levelItems + step.dependencies

        positionBuildItems(levelItems, buildLevel)

        nextSteps = getAssemblySteps(levelItems)
        positionBuildSteps(nextSteps, buildLevel)

        currentSteps = nextSteps
        buildLevel = buildLevel + 1


class AssemblyView(QGraphicsView):
    def __init__(self) -> None:
        QGraphicsView.__init__(self)

        self.scene: QGraphicsScene = QGraphicsScene()
        self.setScene(self.scene)

        self.assemblySteps: MutableMapping[str, AssemblyStep] = {}
        self.assemblyItems: MutableMapping[str, AssemblyItem] = {}

    def addStep(self, name: str, dependencies: List[str], target: str) -> None:
        logup("Add step:", name)
        log("Deps:", dependencies)
        log("Target:", target)

        step = AssemblyStep()
        netData = NetworkData()
        netData.targetName = target
        netData.depsName = dependencies
        step.networkData = netData

        self.scene.addItem(step)
        self.assemblySteps[name] = step
        logdown()

    def addItem(self, name: str) -> None:
        logup("Adding item:", name)

        item = AssemblyItem()
        item.caption = name

        self.scene.addItem(item)
        self.assemblyItems[name] = item

        logdown()

    def clear(self) -> None:
        self.assemblyItems = {}
        self.assemblySteps = {}
        self.scene.clear()

    def update(self) -> None:
        buildDependencies(self.assemblySteps, self.assemblyItems)
        arrangeNodes(self.assemblyItems)
        connectLines(self.scene, self.assemblySteps, self.assemblyItems)
        self.grab(self.sceneRect().toRect()).save("scene.png")



class MainWindow(QMainWindow):
    def __init__(self) -> None:
        QMainWindow.__init__(self)
        self.view = AssemblyView()
        self.addRow_pbtn = QPushButton()
        self.compute_pbtn = QPushButton()
        self.clearAll_pbtn = QPushButton()
        self.input_tbl = QTableWidget()

        splitter = QSplitter()
        self.setCentralWidget(splitter)
        splitter.addWidget(self.view)

        input_wgt = QFrame()
        input_lyt = QVBoxLayout()

        splitter.addWidget(input_wgt)
        input_wgt.setLayout(input_lyt)
        input_lyt.addWidget(self.input_tbl)
        input_lyt.addWidget(self.addRow_pbtn)
        input_lyt.addWidget(self.clearAll_pbtn)
        input_lyt.addWidget(self.compute_pbtn)

        _itbl = self.input_tbl
        _itbl.setColumnCount(4)
        _itbl.setColumnWidth(name_col, 100)
        _itbl.setColumnWidth(deps_col, 100)
        _itbl.setColumnWidth(rest_col, 100)

        _itbl.setHorizontalHeaderLabels(
            ["Name", "Dependency", "Result", "Type"])

        self.addRow_pbtn.setText("Add row")
        self.clearAll_pbtn.setText("Clear all")
        self.compute_pbtn.setText("Compute")

        splitter.setSizes([600, 400])

        self.addRow_pbtn.clicked.connect(self.newDataRow)
        self.clearAll_pbtn.clicked.connect(self.__clearAll)
        self.compute_pbtn.clicked.connect(self.runComputations)

    def __clearAll(self) -> None:
        self.input_tbl.setRowCount(0)
        self.view.clear()

    def runComputations(self) -> None:
        print("### Running computations")

        _itbl = self.input_tbl

        print("Table row count", _itbl.rowCount())

        for row in range(_itbl.rowCount()):
            if _itbl.cellWidget(row, type_col).currentIndex() == 0:
                itemName: str = _itbl.item(row, name_col).text()
                self.view.addItem(itemName)
            else:
                stepName: str = _itbl.item(row, name_col).text()
                targetName: str = _itbl.item(row, rest_col).text()
                stepDeps: List[str] = _itbl.item(row,
                                                 deps_col).text().split(",")

                self.view.addStep(stepName, stepDeps, targetName)
                self.view.addItem(targetName)

        if isGuiMode:
            self.view.update()


        print("Done")

    def newDataRow(self) -> None:
        self.input_tbl.insertRow(self.input_tbl.rowCount())
        lastRow: int = self.input_tbl.rowCount() - 1

        for col in range(3):
            self.input_tbl.setItem(lastRow, col, QTableWidgetItem())

        select = QComboBox(self.input_tbl)
        select.addItem("Assembly item")
        select.addItem("Assembly step")

        self.input_tbl.setCellWidget(lastRow, type_col, select)

    def populateTest(self) -> None:
        # List of inputs, result name, build time, additional dependencies
        assembly = [
            # 1G
            [["input1_1", "input2_1"], "result1", 10, []],
            [["input1_2", "input2_2"], "result2", 10, []],
            [["input1_3"], "result3", 10, ["result1", "result2"]],
            # 2
            [["input1_1_2", "input2_1_2"], "result1_2", 10, []],
            [["input1_3_2", "input3_3_2"], "result3_2", 10, ["result1_2"]],
            # Total final
            [["additional"], "final", 10, ["result3", "result3_2"]]
        ]

        offset: int = 0

        for step in assembly:
            inputItems: List[str] = step[0]
            processRow: int = offset + len(inputItems)

            for row in range(len(inputItems)):
                self.newDataRow()
                self.input_tbl.item(row + offset,
                                    name_col).setText(inputItems[row])

            self.newDataRow()

            typeSelector: QComboBox = self.input_tbl.cellWidget(
                processRow, type_col)

            typeSelector.setCurrentIndex(processRow)

            self.input_tbl.item(processRow, name_col).setText("##" + step[1])
            self.input_tbl.item(processRow, rest_col).setText(step[1])
            self.input_tbl.item(processRow, deps_col).setText(
                ",".join(inputItems + step[3]))

            offset = processRow + 1


if __name__ == '__main__':
    try:
        app = QApplication(sys.argv)
        window = MainWindow()
        window.populateTest()
        window.update()

        if isGuiMode:
            window.show()
            app.exec_()

    except Exception:
        pass

    sys.exit()

print("Done")
