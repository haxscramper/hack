import QtQuick 2.15
import QtQuick.Window 2.15
import QtQuick.Layouts 1.15
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
            console.debug("info");
        }
    }

    Column {
        spacing: 2
        TextInput {
            font.family: "consolas"
            font.pointSize: 16
            text: "Starting input text"
            color: "red"
            onTextChanged: {
                console.log("Updating scopres to text " + text);
                procs_model.updateScores(text);
            }
        }

        ListView {
            model: procs_model

            delegate: Item {
    //            property ModelData model_item: procs_model.getModelData(index)
                Text {
                    color: "green"
                    font.family: "consolas"
                    font.pointSize: 16
                    text: "Test " + procs_model.getRowCount()
    //                text: model_item.qdata
                }
            }
        }
    }



}
