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
            RadioButton { value: "onestep"; label: qsTr("One-step"); checked: true }
            RadioButton { value: "twosteps"; label: qsTr("Two-steps") }
        }

        RadioButtonGroup
        {
            name : 'effect'
            title : qsTr("Effect")
            RadioButton { value: "twoways"; label: qsTr("Two-ways"); checked: true }
            RadioButton { value: "individual"; label: qsTr("Individual") }
        }

        RadioButtonGroup
        {
            name : 'transformation'
            title : qsTr("Transformation")
            RadioButton { value: "d"; label: qsTr("Difference GMM"); checked: true }
            RadioButton { value: "ld"; label: qsTr("System GMM") }
        }

    }
}