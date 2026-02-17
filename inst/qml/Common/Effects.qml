import QtQuick
import JASP.Controls
import JASP

RadioButtonGroup
{
    name: "effects"
    title: qsTr("Effects")

    RadioButton { value: "individual"; label: qsTr("Individual"); checked: true }
    RadioButton { value: "time"; label: qsTr("Time") }
    RadioButton { value: "twoways"; label: qsTr("Individual and Time") }
    RadioButton { value: "nested"; label: qsTr("Nested") }
}