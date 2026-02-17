import QtQuick
import JASP.Controls
import JASP


Section
{
    title: qsTr("Assumption Checks")

    CheckBox
    {
        name: "hausmanTest"
        label: qsTr("Hausman test")
        checked: false
    }
}