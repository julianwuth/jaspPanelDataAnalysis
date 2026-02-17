import QtQuick.Layouts
import JASP.Controls
import JASP.Widgets
import JASP

import './Common' as Common

Form
{
    Common.VariableInput{}

    Section
    {
        title : qsTr("Statistics")

        RadioButtonGroup
        {
            name : 'model'
            title : qsTr("Model")
            RadioButton { value: "within"; label: qsTr("Fixed"); checked: true }
            RadioButton { value: "pooling"; label: qsTr("Pooling") }
            RadioButton { value: "fd"; label: qsTr("First-Difference") }
        }
        RadioButtonGroup
        {
            name : 'effect'
            title : qsTr("Effect")
            RadioButton { value: "individual"; label: qsTr("Individual"); checked: true }
            RadioButton { value: "time"; label: qsTr("Time") }
        }
    }
}