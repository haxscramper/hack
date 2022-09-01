import QtQuick 2.15
import QtQuick.Window 2.15
import QmlLogger 1.0
import Model 1.0

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
            logger.debug("lk;jsdafafkjlfasdklj;fsadd");
        }
    }

    TextInput {
        focus: true
        font.family: "consolas"
        font.pointSize: 16
        selectByMouse: true
        text: "Starting text"
        onTextChanged: {
            logger.trace(text);
        }
    }

    ListView {
        width: 200;
        height: 250

        required model

        delegate: Text {
            required property string qdata
            required property int score

            text: "Animal: " + qdata + ", " + score
        }
    }

}
