import QtQuick 2.15
import QtQuick.Window 2.15
import QmlLogger 1.0

Window {
    width: 640
    height: 480
    visible: true
    title: qsTr("Hello World")
    Logger {
        id: logger
    }

    Shortcut {
        sequence: "Ctrl+E,Ctrl+W"
        onActivated: {
            logger.log(Logger.Debug, "lk;jsdafafkjlfasdklj;fsadd");
        }
    }

    Shortcut {
        sequence: "enter"
        onActivated: {
            logger.log("JS: pressed.")
            textArea.copy()
        }
    }

    TextEdit {
        width: 240
        focus: true
        font.family: "consolas"
        font.pointSize: 16
        selectByMouse: true
    }
}
